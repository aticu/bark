//! Handles displaying of rules.

use std::path::PathBuf;

use eframe::egui::{self, Color32};

use crate::{
    file::{File, Files},
    future_value::FutureValue,
    rules::{MatchCountCache, Rule, RuleStorage},
};

/// Displays a list of rules.
pub(crate) struct RuleList {
    /// The source list of changed files.
    files: &'static Files,
    /// The filter used for rules.
    filter: String,
    /// The match status of shown rules.
    match_status: MatchStatus,
    /// The number of rules shown last frame.
    shown_rules: usize,
    /// The file to store the rules to when a rule is added.
    rule_file: Option<PathBuf>,
    /// The cache for the match counts of the rules.
    match_count_cache: FutureValue<MatchCountCache>,
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
            shown_rules: 0,
            rule_file,
            match_count_cache: Default::default(),
        }
    }

    /// Displays the rules in the given rule storage.
    pub(crate) fn display(&mut self, ui: &mut egui::Ui, storage: &mut RuleStorage) {
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
        });

        let mut shown_rules = 0;

        if self.match_count_cache.update_check(storage) {
            let ctx = ui.ctx().clone();
            self.match_count_cache
                .update((storage.clone(), self.files), move || ctx.request_repaint());
        }

        if self.match_count_cache.is_current(storage) {
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
                            let match_count = self
                                .match_count_cache
                                .get()
                                .unwrap()
                                .get_count(matcher)
                                .unwrap_or(0);

                            let show = match self.match_status {
                                MatchStatus::Ignore => true,
                                MatchStatus::OnlyMatching => match_count > 0,
                                MatchStatus::OnlyNonMatching => match_count == 0,
                            };
                            if show {
                                ui.horizontal(|ui| {
                                    ui.add_sized(
                                        [20.0, ui.style().text_styles[&egui::TextStyle::Body].size],
                                        egui::Label::new(format!("{match_count}")),
                                    );
                                    rule.show(ui, true);

                                    shown_rules += 1;
                                });
                            }
                        }
                    }

                    if ui
                        .add(
                            egui::Button::new("add current data source to rules")
                                .fill(Color32::DARK_RED),
                        )
                        .clicked()
                    {
                        for rule in storage.iter_mut() {
                            rule.add_data_source(self.files);
                        }
                        if let Some(rule_file) = &self.rule_file {
                            storage.save(rule_file);
                        }
                    }
                });
        } else {
            ui.horizontal(|ui| {
                ui.spinner();
                ui.label("computing rule list");
            });
        }

        self.shown_rules = shown_rules;
    }
}

impl Rule {
    /// Displays a summary of the rule.
    pub(crate) fn show(&self, ui: &mut egui::Ui, show_glob: bool) {
        let sample_count = self.sample_count();
        ui.add_sized(
            [80.0, ui.style().text_styles[&egui::TextStyle::Body].size],
            egui::Label::new(format!(
                "{} sample{}",
                sample_count,
                if sample_count != 1 { "s" } else { "" }
            )),
        );

        for distribution in self.distributions() {
            distribution.show(ui, None);
        }

        if show_glob {
            ui.label(format!("{}", self.path_matcher()));
        }
    }

    /// Displays the match information for the given file.
    pub(crate) fn display_file_match(&self, ui: &mut egui::Ui, file: &File) {
        if let Some((distribution, _)) = self.best_matching_distribution(file) {
            if self.distributions().len() > 1 {
                ui.label(format!(
                    "Best match (out of {}):",
                    self.distributions().len()
                ));
            } else {
                ui.label("Best match:");
            }
            distribution.show(ui, None);
            distribution.show_legend(ui, None);
        } else {
            ui.label(format!(
                "No matching distribution found (out of {})",
                self.distributions().len()
            ));
            for distribution in self.distributions() {
                distribution.show(ui, None);
                distribution.show_legend(ui, None);
            }
        }
    }
}
