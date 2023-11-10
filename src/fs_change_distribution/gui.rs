//! Drawing of single change distributions.

use eframe::egui;

use crate::{
    fs_change_distribution::FsChangeDistribution,
    fs_changes::{FsChangeCounts, FsChanges},
    gui::utils::categorical_color,
};

/// The width of a single distribution graph.
const DISTRIBUTION_GRAPH_WIDTH: f32 = 150.0;

/// The horizontal space taken up to signify a single addition and/or deletion.
const DISTRIBUTION_ADD_DELETE_SPACE: f32 = 6.0;

/// The height of a change in the change distribution graph.
const DISTRIBUTION_HEIGHT: f32 = 24.0;

/// The height of the legend bar in the change distribution graph.
const DISTRIBUTION_BAR_HEIGHT: f32 = 3.0;

impl FsChangeDistribution {
    /// Shows this change distribution in the GUI.
    pub(crate) fn show(&self, ui: &mut egui::Ui, counts: Option<&FsChangeCounts>) {
        let (all_rect, _) = ui.allocate_exact_size(
            egui::vec2(
                DISTRIBUTION_GRAPH_WIDTH,
                DISTRIBUTION_HEIGHT + DISTRIBUTION_BAR_HEIGHT,
            ),
            egui::Sense::hover(),
        );

        let draw_front_rect = |ui: &mut egui::Ui, change, rect| {
            crate::fs_changes::gui::draw(
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
                        ui.label("up to one");
                    },
                );
            }
        };

        let front_rect = egui::Rect::from_min_size(
            all_rect.min,
            egui::vec2(DISTRIBUTION_ADD_DELETE_SPACE, all_rect.height()),
        );
        match (self.permits_addition, self.permits_deletion) {
            (true, true) => {
                draw_front_rect(
                    ui,
                    FsChanges::DELETED,
                    egui::Rect::from_min_max(front_rect.left_center(), front_rect.right_bottom()),
                );
                draw_front_rect(
                    ui,
                    FsChanges::ADDED,
                    egui::Rect::from_min_max(front_rect.left_top(), front_rect.right_center()),
                );
            }
            (true, false) => {
                draw_front_rect(ui, FsChanges::ADDED, front_rect);
            }
            (false, true) => {
                draw_front_rect(ui, FsChanges::DELETED, front_rect);
            }
            (false, false) => (),
        };

        let mut current_offset = 0.0;

        let rect = if self.permits_addition || self.permits_deletion {
            egui::Rect::from_min_max(front_rect.right_top(), all_rect.right_bottom())
        } else {
            all_rect
        };

        match &self.kind {
            super::DistributionKind::Empty => {
                ui.painter()
                    .rect_filled(rect, egui::Rounding::ZERO, egui::Color32::from_gray(40));

                ui.painter().text(
                    rect.center(),
                    egui::Align2::CENTER_CENTER,
                    "???",
                    egui::FontId {
                        family: egui::FontFamily::Proportional,
                        size: rect.height(),
                    },
                    ui.style().noninteractive().text_color(),
                );

                if ui.rect_contains_pointer(rect) {
                    egui::show_tooltip_at_pointer(
                        ui.ctx(),
                        "change_distribution_graph_tooltip".into(),
                        |ui| {
                            ui.label("empty distribution");
                        },
                    );
                }
            }
            super::DistributionKind::Deterministic { changes } => draw_bar_component(
                ui,
                rect,
                *changes,
                0,
                &mut current_offset,
                1.0,
                Some((1.0, 1.0)),
                get_count(*changes, counts),
            ),
            super::DistributionKind::Complex { changes } => {
                for (i, (&changes, &probability)) in changes.iter().enumerate() {
                    draw_bar_component(
                        ui,
                        rect,
                        changes,
                        i,
                        &mut current_offset,
                        *probability,
                        Some((*probability, *probability)),
                        get_count(changes, counts),
                    )
                }
            }
        }
    }

    /// Shows the legend for shown distribution graph.
    pub(crate) fn show_legend(&self, ui: &mut egui::Ui, counts: Option<&FsChangeCounts>) {
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
            if self.permits_addition {
                let rect = get_rect(ui);
                crate::fs_changes::gui::draw(
                    Some(FsChanges::ADDED),
                    ui,
                    rect,
                    egui::Color32::from_gray(40),
                    false,
                    None,
                );
                ui.label("any amount");
            }

            if self.permits_deletion {
                let rect = get_rect(ui);
                crate::fs_changes::gui::draw(
                    Some(FsChanges::DELETED),
                    ui,
                    rect,
                    egui::Color32::from_gray(40),
                    false,
                    None,
                );
                ui.label("any amount");
            }

            match &self.kind {
                super::DistributionKind::Empty => {
                    ui.label("empty distribution");
                }
                super::DistributionKind::Deterministic { changes } => {
                    draw_component_legend(ui, *changes, 0, (1.0, 1.0), get_count(*changes, counts))
                }
                super::DistributionKind::Complex { changes } => {
                    for (i, (&changes, &probability)) in changes.iter().enumerate() {
                        draw_component_legend(
                            ui,
                            changes,
                            i,
                            (*probability, *probability),
                            get_count(changes, counts),
                        )
                    }
                }
            }
        });
    }
}

/// Returns the count to show for the given changes, if one should be shown.
fn get_count(changes: FsChanges, counts: Option<&FsChangeCounts>) -> Option<u32> {
    let total_counts = counts.map(|counts| counts.total_count());
    let show_counts = total_counts.is_some() && total_counts != Some(100);

    if show_counts {
        counts.and_then(|counts| counts.get(changes))
    } else {
        None
    }
}

/// Draws a single component of the probability distribution bar.
#[allow(clippy::too_many_arguments)]
fn draw_bar_component(
    ui: &mut egui::Ui,
    rect: egui::Rect,
    changes: FsChanges,
    index: usize,
    current_offset: &mut f32,
    probability: f64,
    show_probability: Option<(f64, f64)>,
    count: Option<u32>,
) {
    let rect = egui::Rect::from_min_size(
        rect.min + egui::vec2(*current_offset, 0.0),
        egui::vec2(probability as f32 * rect.width(), rect.height()),
    );
    *current_offset += rect.width();

    let mut change_rect = rect;
    change_rect.set_height(DISTRIBUTION_HEIGHT);
    let bar_rect = egui::Rect::from_min_max(change_rect.left_bottom(), rect.right_bottom());

    crate::fs_changes::gui::draw(
        Some(changes),
        ui,
        change_rect,
        egui::Color32::from_gray(40),
        false,
        None,
    );
    ui.painter_at(bar_rect)
        .rect_filled(bar_rect, 0.0, categorical_color(index));

    if ui.rect_contains_pointer(rect) {
        if let Some(show_probability) = show_probability {
            egui::show_tooltip_at_pointer(
                ui.ctx(),
                "change_distribution_graph_tooltip".into(),
                |ui| {
                    ui.horizontal(|ui| {
                        draw_component_legend(ui, changes, index, show_probability, count);
                    });
                },
            );
        }
    }
}

/// Draws the legend for a single component of the probability distribution bar.
fn draw_component_legend(
    ui: &mut egui::Ui,
    changes: FsChanges,
    index: usize,
    (min_probability, max_probability): (f64, f64),
    count: Option<u32>,
) {
    let (rect, _) = ui.allocate_exact_size(
        egui::vec2(
            DISTRIBUTION_HEIGHT / 2.0,
            DISTRIBUTION_HEIGHT + DISTRIBUTION_BAR_HEIGHT,
        ),
        egui::Sense::hover(),
    );
    let mut change_rect = rect;
    change_rect.set_height(DISTRIBUTION_HEIGHT);
    let bar_rect = egui::Rect::from_min_max(change_rect.left_bottom(), rect.right_bottom());

    crate::fs_changes::gui::draw(
        Some(changes),
        ui,
        change_rect,
        egui::Color32::from_gray(40),
        false,
        None,
    );
    ui.painter()
        .rect_filled(bar_rect, 0.0, categorical_color(index));

    ui.vertical(|ui| {
        ui.style_mut().spacing.item_spacing.y /= 2.0;

        if min_probability == max_probability {
            ui.label(format!("{:.0}%", min_probability * 100.0));
        } else {
            ui.label(format!(
                "{:.0}%-{:.0}%",
                min_probability * 100.0,
                max_probability * 100.0,
            ));
        }
        if let Some(count) = count {
            ui.label(format!("{}", count));
        }
    });
}
