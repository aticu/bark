//! Drawing of single change distributions.

use eframe::egui;

use crate::{change_distribution::ChangeDistribution, change_event::ChangeEvent};

use super::change_event;

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
            change_event::draw(
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
                        ui.label("exactly one");
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

            change_event::draw(
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
                            change_event::draw(
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

        // almost made a typo to `get_rekt`
        let get_rect = |ui: &mut egui::Ui| {
            let (rect, _) = ui.allocate_exact_size(
                egui::vec2(
                    DISTRIBUTION_HEIGHT / 2.0,
                    DISTRIBUTION_HEIGHT + DISTRIBUTION_BAR_HEIGHT,
                ),
                egui::Sense::hover(),
            );
            rect
        };

        ui.horizontal(|ui| {
            if self.has_single_addition {
                let rect = get_rect(ui);
                change_event::draw(
                    Some(ChangeEvent::ADDED),
                    ui,
                    rect,
                    egui::Color32::from_gray(40),
                    false,
                    None,
                );
                ui.label("exactly one");
            }

            if self.has_single_deletion {
                let rect = get_rect(ui);
                change_event::draw(
                    Some(ChangeEvent::DELETED),
                    ui,
                    rect,
                    egui::Color32::from_gray(40),
                    false,
                    None,
                );
                ui.label("exactly one");
            }

            for (i, (&event, &num)) in self.changes.iter().enumerate() {
                let rect = get_rect(ui);
                let mut change_rect = rect;
                change_rect.set_height(DISTRIBUTION_HEIGHT);
                let bar_rect =
                    egui::Rect::from_min_max(change_rect.left_bottom(), rect.right_bottom());

                change_event::draw(
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
