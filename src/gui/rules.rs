//! Handles displaying of rules.

use std::path::PathBuf;

use eframe::egui::{self, Color32};

use crate::{
    file::Files,
    rules::{Rule, RuleStorage},
};

pub(crate) struct RuleList {
    /// The source list of changed files.
    files: &'static Files,
    /// The filter used for rules.
    filter: String,
    /// The match status of shown rules.
    match_status: MatchStatus,
    /// The content of the tag text editing box.
    tag_str: String,
    /// The number of rules shown last frame.
    shown_rules: usize,
    /// The file to store the rules to when a rule is added.
    rule_file: Option<PathBuf>,
    /// The last error that occurred.
    last_err: Option<String>,
}

/// The match status of shown rules.
enum MatchStatus {
    /// Only show rules matching at least one file.
    OnlyMatching,
    /// Only show rules not matching any file.
    OnlyNonMatching,
    /// Show all rules ignoring whether they match a file.
    Ignore,
}

impl RuleList {
    /// Creates a new rule list.
    pub(crate) fn new(files: &'static Files, rule_file: Option<PathBuf>) -> Self {
        Self {
            files,
            filter: String::new(),
            match_status: MatchStatus::Ignore,
            tag_str: String::new(),
            shown_rules: 0,
            rule_file,
            last_err: None,
        }
    }

    /// Displays the rules in the given rule storage.
    pub(crate) fn display(&mut self, ui: &mut egui::Ui, storage: &mut RuleStorage) {
        let mut apply_tag = None;

        if let Some(last_err) = &self.last_err {
            ui.label(last_err);
        }

        ui.horizontal(|ui| {
            ui.label("Filter:");
            ui.text_edit_singleline(&mut self.filter);
            match self.match_status {
                MatchStatus::Ignore => {
                    if ui.button("Ignoring whether rules match").clicked() {
                        self.match_status = MatchStatus::OnlyMatching;
                    }
                }
                MatchStatus::OnlyMatching => {
                    if ui.button("Showing only matching rules").clicked() {
                        self.match_status = MatchStatus::OnlyNonMatching;
                    }
                }
                MatchStatus::OnlyNonMatching => {
                    if ui.button("Showing only non matching rules").clicked() {
                        self.match_status = MatchStatus::Ignore;
                    }
                }
            }

            ui.separator();

            if ui
                .button(format!("Tag all {} shown rules with", self.shown_rules))
                .clicked()
            {
                apply_tag = Some(std::mem::take(&mut self.tag_str));
                if apply_tag.as_deref() == Some("") {
                    apply_tag = None;
                }
            }
            ui.text_edit_singleline(&mut self.tag_str);
        });

        let mut shown_rules = 0;

        egui::ScrollArea::both()
            .auto_shrink([false, true])
            .show(ui, |ui| {
                for rule in storage.iter_mut() {
                    // TODO: maybe do something like smart case here?
                    if format!("{}", rule.path_matcher())
                        .to_lowercase()
                        .contains(&self.filter.to_lowercase())
                    {
                        let matcher = rule.path_matcher();
                        let match_count = self.files.match_count(matcher);

                        let show = match self.match_status {
                            MatchStatus::Ignore => true,
                            MatchStatus::OnlyMatching => match_count > 0,
                            MatchStatus::OnlyNonMatching => match_count == 0,
                        };
                        if show {
                            ui.horizontal(|ui| {
                                ui.add_sized(
                                    egui::vec2(
                                        20.0,
                                        ui.style().text_styles[&egui::TextStyle::Body].size,
                                    ),
                                    egui::Label::new(format!("{match_count}")),
                                );
                                rule.show(ui, 9.0, None, true, None);

                                shown_rules += 1;

                                if let Some(tag) = &apply_tag {
                                    rule.tag(tag);
                                }
                            });
                        }
                    }
                }
            });

        self.shown_rules = shown_rules;

        if apply_tag.is_some() {
            if let Some(rule_file) = &self.rule_file {
                if let Err(err) = storage.save(rule_file) {
                    self.last_err = Some(err.to_string());
                }
            }
        }
    }
}

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
        let mut iter = self.frequencies().into_iter().peekable();
        let mut other_iter = compare
            .into_iter()
            .flat_map(|compare| compare.frequencies());

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

            for tag in &self.tags {
                const TAG_FONT: egui::FontId = egui::FontId::proportional(12.0);

                let tag_layout = ui.painter().layout_no_wrap(
                    String::from(&**tag),
                    TAG_FONT,
                    ui.style().noninteractive().text_color(),
                );

                let (rect, _) =
                    ui.allocate_exact_size(tag_layout.rect.size(), egui::Sense::hover());
                let rect = rect.expand(2.0);
                ui.painter().rect_filled(rect, 5.0, Color32::LIGHT_BLUE);
                ui.painter().text(
                    rect.center(),
                    egui::Align2::CENTER_CENTER,
                    &**tag,
                    TAG_FONT,
                    Color32::BLACK,
                );
            }

            if show_glob {
                ui.label(format!("{}", self.path_matcher()));
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
