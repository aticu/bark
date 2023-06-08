//! Handles displaying of rules.

use eframe::egui::{self, Color32};

use crate::rules::Rule;

impl Rule {
    /// Displays a summary of the rule.
    pub(crate) fn show(
        &self,
        ui: &mut egui::Ui,
        font_size: f32,
        row_len: Option<usize>,
        show_glob: bool,
        compare: Option<Rule>,
    ) {
        let row_len = row_len.unwrap_or(usize::MAX);
        let mut iter = self.frequencies.as_array().into_iter().peekable();
        let mut other_iter = compare
            .into_iter()
            .flat_map(|compare| compare.frequencies.as_array());

        ui.horizontal(|ui| {
            ui.vertical(|ui| {
                while iter.peek().is_some() {
                    ui.horizontal(|ui| {
                        for (name, val) in iter.by_ref().take(row_len) {
                            let (rect, _) = ui.allocate_exact_size(
                                egui::vec2(font_size * 10.0, font_size * 2.0),
                                egui::Sense::hover(),
                            );
                            display_rule_frequency(
                                ui,
                                rect,
                                name,
                                val,
                                other_iter.next().map(|(_, val)| val),
                            );
                        }
                    });
                }
            });

            if show_glob {
                ui.label(format!("{}", self.path_matcher));
            }
        });
    }
}
/// Displays a single rule frequency.
fn display_rule_frequency(
    ui: &mut egui::Ui,
    rect: egui::Rect,
    name: &str,
    val: f64,
    other_val: Option<f64>,
) {
    if !ui.is_rect_visible(rect) {
        return;
    }

    let text_height = rect.size().y * 0.5;
    let bar_height = rect.size().y - text_height;
    let corner_radius = rect.size().y * 0.2;

    let painter = ui.painter().with_clip_rect(rect);
    let top_rect =
        egui::Rect::from_min_size(rect.left_top(), egui::vec2(rect.width(), text_height));

    painter.rect_filled(
        top_rect,
        egui::Rounding {
            nw: corner_radius,
            ne: corner_radius,
            ..Default::default()
        },
        Color32::from_gray(60),
    );
    painter.text(
        rect.center_top(),
        egui::Align2::CENTER_TOP,
        name,
        egui::FontId {
            size: text_height,
            family: egui::FontFamily::Proportional,
        },
        Color32::from_gray(180),
    );

    let compare = other_val.is_some();
    let min_val = val.min(other_val.unwrap_or(val));
    let max_val = val.max(other_val.unwrap_or(val));

    let left_midpoint = egui::pos2(
        rect.left() + rect.width() * min_val as f32,
        rect.top() + text_height,
    );
    let right_midpoint = egui::pos2(
        rect.left() + rect.width() * max_val as f32,
        rect.top() + text_height,
    );
    let left_rect = egui::Rect::from_two_pos(rect.left_bottom(), left_midpoint);
    let mid_rect = egui::Rect::from_two_pos(left_rect.right_bottom(), right_midpoint);
    let right_rect = egui::Rect::from_two_pos(right_midpoint, rect.right_bottom());

    let left_painter = painter.with_clip_rect(left_rect);
    let mid_painter = painter.with_clip_rect(mid_rect);
    let right_painter = painter.with_clip_rect(right_rect);

    let text = if compare && other_val != Some(val) {
        format!(
            "{:.0} (found {:.0})",
            val * 100.0,
            other_val.unwrap() * 100.0
        )
    } else {
        format!("{:.0}", val * 100.0)
    };
    let font_id = egui::FontId {
        size: bar_height,
        family: egui::FontFamily::Monospace,
    };
    let bottom_rect = egui::Rect::from_two_pos(left_rect.left_top(), right_rect.right_bottom());
    let rounding = egui::Rounding {
        sw: corner_radius,
        se: corner_radius,
        ..Default::default()
    };

    left_painter.rect_filled(
        bottom_rect,
        rounding,
        if compare {
            Color32::GRAY
        } else {
            super::lerp_color(Color32::RED, Color32::GREEN, val)
        },
    );
    left_painter.text(
        rect.center_bottom(),
        egui::Align2::CENTER_BOTTOM,
        &text,
        font_id.clone(),
        Color32::BLACK,
    );
    if compare {
        mid_painter.rect_filled(
            bottom_rect,
            rounding,
            if Some(val) > other_val {
                Color32::RED
            } else {
                Color32::GREEN
            },
        );
        mid_painter.text(
            rect.center_bottom(),
            egui::Align2::CENTER_BOTTOM,
            &text,
            font_id.clone(),
            Color32::BLACK,
        );
    }
    right_painter.rect_filled(bottom_rect, rounding, Color32::BLACK);
    right_painter.text(
        rect.center_bottom(),
        egui::Align2::CENTER_BOTTOM,
        &text,
        font_id,
        Color32::WHITE,
    );
}
