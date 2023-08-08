//! Drawing of single change events.

use eframe::{egui, epaint::Color32};

use crate::{change_distribution::ChangeDistribution, change_event::ChangeEvent};

/// The background color that a hovered row is highlighted with.
const HOVERED_HIGHLIGHT_COLOR: Color32 = Color32::WHITE;

/// How close to the highlight color the hovered entry should get.
const HOVERED_HIGHLIGHT_PERCENT: f64 = 0.1;

/// Draws a single change event.
pub(crate) fn draw(
    change: Option<ChangeEvent>,
    ui: &mut egui::Ui,
    rect: egui::Rect,
    default_bg: Color32,
    is_highlighted: bool,
    file_sizes: Option<sniff::Change<u64>>,
) {
    let size_change = if let Some(sizes) = file_sizes {
        (sizes.from, sizes.to)
    } else if let Some(change) = change {
        if change.size_increased() {
            (0, 1)
        } else if change.size_decreased() {
            (1, 0)
        } else {
            (0, 0)
        }
    } else {
        (0, 0)
    };

    let (fg, bg) = change_colors(
        change,
        size_change,
        ui.style().noninteractive().text_color(),
        default_bg,
    );

    let painter = ui.painter().with_clip_rect(rect);
    painter.rect_filled(
        rect,
        0.0,
        if is_highlighted {
            super::lerp_color(bg, HOVERED_HIGHLIGHT_COLOR, HOVERED_HIGHLIGHT_PERCENT)
        } else {
            bg
        },
    );

    let Some(change) = change else { return };

    if change.is_empty() {
        return;
    }

    draw_change_grid(painter, rect, fg, change);
}

/// Draws a grid of the changes that occurred in the given metadata.
fn draw_change_grid(
    painter: egui::Painter,
    rect: egui::Rect,
    text_color: Color32,
    change: ChangeEvent,
) {
    /// The width of the grid of changes.
    const GRID_WIDTH: usize = 2;

    /// The height of the grid of changes.
    const GRID_HEIGHT: usize = 3;

    let grid_entries = [
        ("A", change.accessed_timestamp()),
        ("M", change.modified_timestamp()),
        ("C", change.created_timestamp()),
        ("m", change.inode_timestamp()),
        (
            if change.size_increased() { "G" } else { "s" },
            change.size_changed(),
        ),
        ("I", change.inode_changed()),
    ];
    assert!(grid_entries.len() <= GRID_HEIGHT * GRID_WIDTH);

    let mut grid_x = 0;
    let mut grid_y = 0;
    let x_step: f32 = rect.height() / 2.0 / GRID_WIDTH as f32;
    let y_step: f32 = rect.height() / GRID_HEIGHT as f32;

    for (text, should_display) in grid_entries {
        if should_display {
            painter.text(
                rect.left_top() + egui::vec2(grid_x as f32 * x_step, grid_y as f32 * y_step),
                eframe::emath::Align2::LEFT_TOP,
                text,
                egui::FontId {
                    size: rect.height() / GRID_HEIGHT as f32,
                    family: egui::FontFamily::Monospace,
                },
                text_color,
            );
        }

        grid_x = (grid_x + 1) % GRID_WIDTH;
        if grid_x == 0 {
            // we wrapped x
            grid_y += 1;
        }
        if grid_y == GRID_HEIGHT {
            // we're at the end and cannot display any more entries
            break;
        }
    }
}

/// Computes the colors for a single change.
fn change_colors(
    change: Option<ChangeEvent>,
    (old_size, new_size): (u64, u64),
    default_fg: Color32,
    default_bg: Color32,
) -> (Color32, Color32) {
    let Some(change) = change else {
        // return a warning color, if not usable change event could be determined
        return (Color32::BLACK, Color32::from_rgb(255, 0, 255));
    };

    if change.added() {
        (Color32::BLACK, Color32::GREEN)
    } else if change.deleted() {
        (Color32::BLACK, Color32::RED)
    } else if change.content_changed() {
        match old_size.cmp(&new_size) {
            std::cmp::Ordering::Less => (
                Color32::BLACK,
                super::lerp_color(
                    Color32::from_rgb(100, 255, 0),
                    Color32::from_rgb(220, 255, 0),
                    old_size as f64 / new_size as f64,
                ),
            ),
            std::cmp::Ordering::Equal => (Color32::BLACK, Color32::YELLOW),
            std::cmp::Ordering::Greater => (
                Color32::BLACK,
                super::lerp_color(
                    Color32::from_rgb(255, 100, 0),
                    Color32::from_rgb(255, 220, 0),
                    new_size as f64 / old_size as f64,
                ),
            ),
        }
    } else {
        (default_fg, default_bg)
    }
}

/// The width of a single distribution graph.
const DISTRIBUTION_GRAPH_WIDTH: f32 = 150.0;

/// The horizontal space taken up to signify a single addition and/or deletion.
const DISTRIBUTION_ADD_DELETE_SPACE: f32 = 6.0;

/// The height of a change in the change distribution graph.
const DISTRIBUTION_HEIGHT: f32 = 24.0;

/// The height of the legend bar in the change distribution graph.
const DISTRIBUTION_BAR_HEIGHT: f32 = 3.0;

impl ChangeDistribution {
    /// Shows this change distribution in the GUI.
    pub(crate) fn show(&self, ui: &mut egui::Ui) {
        let total = self.changes.values().sum::<u32>();

        let (all_rect, _) = ui.allocate_exact_size(
            egui::vec2(
                DISTRIBUTION_GRAPH_WIDTH,
                DISTRIBUTION_HEIGHT + DISTRIBUTION_BAR_HEIGHT,
            ),
            egui::Sense::hover(),
        );

        let draw_front_rect = |ui: &mut egui::Ui, change, rect| {
            draw(
                Some(change),
                ui,
                rect,
                egui::Color32::from_gray(40),
                false,
                None,
            );

            if ui.rect_contains_pointer(rect) {
                egui::show_tooltip_at_pointer(
                    ui.ctx(),
                    "change_distribution_graph_tooltip".into(),
                    |ui| {
                        ui.label("one per sample");
                    },
                );
            }
        };

        let front_rect = egui::Rect::from_min_size(
            all_rect.min,
            egui::vec2(
                DISTRIBUTION_ADD_DELETE_SPACE,
                DISTRIBUTION_HEIGHT + DISTRIBUTION_BAR_HEIGHT,
            ),
        );
        match (self.has_single_addition, self.has_single_deletion) {
            (true, true) => {
                draw_front_rect(
                    ui,
                    ChangeEvent::DELETED,
                    egui::Rect::from_min_max(front_rect.left_center(), front_rect.right_bottom()),
                );
                draw_front_rect(
                    ui,
                    ChangeEvent::ADDED,
                    egui::Rect::from_min_max(front_rect.left_top(), front_rect.right_center()),
                );
            }
            (true, false) => {
                draw_front_rect(ui, ChangeEvent::ADDED, front_rect);
            }
            (false, true) => {
                draw_front_rect(ui, ChangeEvent::DELETED, front_rect);
            }
            (false, false) => (),
        };

        let mut current_offset = if self.has_single_addition || self.has_single_deletion {
            front_rect.width()
        } else {
            0.0
        };

        let total_width = DISTRIBUTION_GRAPH_WIDTH - current_offset;

        for (i, (&event, &num)) in self.changes.iter().enumerate() {
            let percent = num as f32 / total as f32;
            let rect = egui::Rect::from_min_size(
                all_rect.min + egui::vec2(current_offset, 0.0),
                egui::vec2(
                    percent * total_width,
                    DISTRIBUTION_HEIGHT + DISTRIBUTION_BAR_HEIGHT,
                ),
            );
            current_offset += rect.width();

            let mut change_rect = rect;
            change_rect.set_height(DISTRIBUTION_HEIGHT);
            let bar_rect = egui::Rect::from_min_max(change_rect.left_bottom(), rect.right_bottom());

            draw(
                Some(event),
                ui,
                change_rect,
                egui::Color32::from_gray(40),
                false,
                None,
            );
            ui.painter()
                .rect_filled(bar_rect, 0.0, crate::gui::utils::categorical_color(i));

            if ui.rect_contains_pointer(rect) {
                egui::show_tooltip_at_pointer(
                    ui.ctx(),
                    "change_distribution_graph_tooltip".into(),
                    |ui| {
                        ui.horizontal(|ui| {
                            let (rect, _) = ui.allocate_exact_size(
                                egui::vec2(DISTRIBUTION_HEIGHT / 2.0, DISTRIBUTION_HEIGHT),
                                egui::Sense::hover(),
                            );
                            draw(
                                Some(event),
                                ui,
                                rect,
                                egui::Color32::from_gray(40),
                                false,
                                None,
                            );

                            if total != 100 {
                                ui.vertical(|ui| {
                                    ui.style_mut().spacing.item_spacing.y /= 2.0;

                                    ui.label(format!("{:.0}%", num as f32 / total as f32 * 100.0));
                                    ui.label(format!("{}", num));
                                });
                            } else {
                                ui.label(format!("{:.0}%", num as f32 / total as f32 * 100.0));
                            }
                        });
                    },
                );
            }
        }
    }

    /// Shows the legend for shown distribution graph.
    pub(crate) fn show_legend(&self, ui: &mut egui::Ui) {
        let total = self.changes.values().sum::<u32>();

        ui.horizontal(|ui| {
            for (i, (&event, &num)) in self.changes.iter().enumerate() {
                let (rect, _) = ui.allocate_exact_size(
                    egui::vec2(
                        DISTRIBUTION_HEIGHT / 2.0,
                        DISTRIBUTION_HEIGHT + DISTRIBUTION_BAR_HEIGHT,
                    ),
                    egui::Sense::hover(),
                );
                let mut change_rect = rect;
                change_rect.set_height(DISTRIBUTION_HEIGHT);
                let bar_rect =
                    egui::Rect::from_min_max(change_rect.left_bottom(), rect.right_bottom());

                draw(
                    Some(event),
                    ui,
                    change_rect,
                    egui::Color32::from_gray(40),
                    false,
                    None,
                );
                ui.painter()
                    .rect_filled(bar_rect, 0.0, crate::gui::utils::categorical_color(i));

                if total != 100 {
                    ui.vertical(|ui| {
                        ui.style_mut().spacing.item_spacing.y /= 2.0;

                        ui.label(format!("{:.0}%", num as f32 / total as f32 * 100.0));
                        ui.label(format!("{}", num));
                    });
                } else {
                    ui.label(format!("{:.0}%", num as f32 / total as f32 * 100.0));
                }
            }
        });
    }
}
