//! Implements a GUI for writing rules.

use eframe::{egui, epaint::Color32};

use crate::{
    file::Files,
    glob::{self, glob_pieces, suggest_glob},
    rules::{Rule, RuleStorage},
};

/// A widget for rule writing.
pub(crate) struct RuleWriter {
    /// The string representation of the glob that will make up the rule.
    glob_str: String,
    /// The piece representation of the glob that will make up the rule.
    glob_pieces: Vec<glob::GlobPiece>,
    /// The files that the rule writer works on.
    files: &'static Files,
    /// A change list to display all the files that match the given rule.
    matching_changes: super::change_list::ChangeList,
}

impl RuleWriter {
    /// Creates a rule writer for the given files.
    pub(crate) fn new(files: &'static Files, outer_rules: &RuleStorage) -> Self {
        let suggested_glob = suggest_glob(files, outer_rules).unwrap_or_default();
        RuleWriter {
            glob_str: glob::pieces_to_str(&suggested_glob),
            glob_pieces: suggested_glob,
            files,
            matching_changes: super::change_list::ChangeList::new(
                files,
                "Files matching the rule",
                false,
            ),
        }
    }

    /// Displays this rule writer.
    pub(crate) fn display(&mut self, ui: &mut egui::Ui, outer_rules: &mut RuleStorage) {
        egui::ScrollArea::both()
            .max_height(f32::INFINITY)
            .show(ui, |ui| {
                ui.horizontal(|ui| {
                    if let Some(rule) = Rule::from_glob(&self.glob_str, self.files) {
                        if ui
                            .add(egui::Button::new("Add").fill(Color32::DARK_GREEN))
                            .clicked()
                        {
                            outer_rules.insert(rule);
                            self.glob_pieces =
                                suggest_glob(self.files, outer_rules).unwrap_or_default();
                            self.glob_str = glob::pieces_to_str(&self.glob_pieces);
                        }
                    }
                    ui.label("Enter the rule glob:");
                    let mut glob_str = self.glob_str.clone();
                    ui.add(
                        egui::TextEdit::singleline(&mut glob_str)
                            .desired_width(40.0)
                            .clip_text(false)
                            .id_source("change_list_text_edit"),
                    );

                    if glob_str != self.glob_str {
                        self.glob_pieces = glob_pieces(&glob_str);
                        self.glob_str = glob::pieces_to_str(&self.glob_pieces);
                    }
                });

                ui.horizontal(|ui| {
                    let mut changed = false;
                    for piece in &mut self.glob_pieces {
                        ui.vertical(|ui| {
                            for replacement in piece.possible_replacements() {
                                if ui
                                    .add(
                                        egui::Button::new(
                                            egui::RichText::new(
                                                piece.display_replacement(replacement),
                                            )
                                            .color(Color32::BLACK),
                                        )
                                        .fill(
                                            if replacement == piece.current_replacement() {
                                                Color32::LIGHT_BLUE
                                            } else {
                                                Color32::DARK_GRAY
                                            },
                                        ),
                                    )
                                    .clicked()
                                {
                                    piece.apply_replacement(replacement);
                                    changed = true;
                                }
                            }
                        });
                    }
                    if changed {
                        self.glob_str = glob::pieces_to_str(&self.glob_pieces);
                    }
                });

                let rule = Rule::from_glob(&self.glob_str, self.files);

                let mut storage = RuleStorage::new();
                if let Some(rule) = rule {
                    self.display_rule_summary(ui, &rule);
                    storage.insert(rule);
                }

                ui.separator();

                self.matching_changes.display(ui, &storage);
            });
    }

    /// Displays a summary of a rule.
    pub(crate) fn display_rule_summary(&mut self, ui: &mut egui::Ui, rule: &Rule) {
        ui.horizontal(|ui| {
            for (name, val) in rule.frequencies.iter() {
                let font_size = 12.0;
                let (rect, _) = ui.allocate_exact_size(
                    egui::vec2(font_size * 10.0, font_size * 2.0),
                    egui::Sense::hover(),
                );
                display_rule_frequency(ui, rect, name, val);
            }
        });
    }
}

/// Displays a single rule frequency.
fn display_rule_frequency(ui: &mut egui::Ui, rect: egui::Rect, name: &str, val: f64) {
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

    let midpoint = egui::pos2(
        rect.left() + rect.width() * val as f32,
        rect.top() + text_height,
    );
    let left_rect = egui::Rect::from_two_pos(rect.left_bottom(), midpoint);
    let right_rect = egui::Rect::from_two_pos(midpoint, rect.right_bottom());

    let left_painter = painter.with_clip_rect(left_rect);
    let right_painter = painter.with_clip_rect(right_rect);

    let text = format!("{:.0}", val * 100.0);
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
        super::lerp_color(Color32::RED, Color32::GREEN, val),
    );
    left_painter.text(
        rect.center_bottom(),
        egui::Align2::CENTER_BOTTOM,
        &text,
        font_id.clone(),
        Color32::BLACK,
    );
    right_painter.rect_filled(bottom_rect, rounding, Color32::BLACK);
    right_painter.text(
        rect.center_bottom(),
        egui::Align2::CENTER_BOTTOM,
        &text,
        font_id,
        Color32::WHITE,
    );
}
