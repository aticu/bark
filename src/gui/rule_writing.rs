//! Implements a GUI for writing rules.

use std::path::PathBuf;

use eframe::{egui, epaint::Color32};

use crate::{
    file::Files,
    path_matcher::{suggest_matcher, PathMatcher, PathMatcherPart},
    rules::{Rule, RuleStorage},
};

use super::utils;

/// A widget for rule writing.
pub(crate) struct RuleWriter {
    /// The string representation of the glob that will make up the rule.
    matcher_str: String,
    /// The suggested matcher.
    suggested_matcher: Option<String>,
    /// The suggested lenient_matcher.
    suggested_lenient_matcher: Option<String>,
    /// The content of the "always replace" textbox.
    always_replace_source: String,
    /// The content of the "with" textbox.
    always_replace_target: String,
    /// The files that the rule writer works on.
    files: &'static Files,
    /// A collection of named lists of paths to use for match testing.
    path_lists: &'static std::collections::BTreeMap<String, crate::input::PathList>,
    /// The selected path list as the one representing the current system.
    selected_path_list: Option<String>,
    /// The selected byte range from the previous frame.
    prev_selection: Option<std::ops::Range<usize>>,
    /// The number of paths to skip for a new suggestion.
    skip_count: usize,
    /// A receiver for the next matcher suggestion.
    matcher_suggestion_receiver: Option<std::sync::mpsc::Receiver<(String, Option<String>)>>,
    /// The file to store the rules to when a rule is added.
    rule_file: Option<PathBuf>,
    /// A change list to display all the files that match the given rule.
    matching_changes: super::change_list::ChangeList,
}

impl RuleWriter {
    /// Creates a rule writer for the given files.
    pub(crate) fn new(
        files: &'static Files,
        outer_rules: &RuleStorage,
        rule_file: Option<PathBuf>,
        path_lists: &'static std::collections::BTreeMap<String, crate::input::PathList>,
        ctx: egui::Context,
    ) -> Self {
        let mut this = RuleWriter {
            matcher_str: String::new(),
            suggested_matcher: None,
            suggested_lenient_matcher: None,
            always_replace_source: String::new(),
            always_replace_target: String::new(),
            files,
            path_lists,
            selected_path_list: None,
            prev_selection: None,
            skip_count: 0,
            matcher_suggestion_receiver: None,
            rule_file,
            matching_changes: super::change_list::ChangeList::new(
                files,
                "Files matching the rule",
                true,
            ),
        };

        this.renew_suggestion(outer_rules, ctx);

        this
    }

    /// Displays this rule writer.
    pub(crate) fn display(&mut self, ui: &mut egui::Ui, outer_rules: &mut RuleStorage) {
        if let Some(receiver) = &self.matcher_suggestion_receiver {
            if let Ok((suggested_matcher, suggested_lenient_matcher)) = receiver.try_recv() {
                if self.matcher_str.is_empty() {
                    if !self.always_replace_target.is_empty()
                        && !self.always_replace_source.is_empty()
                    {
                        self.matcher_str = suggested_matcher
                            .replace(&self.always_replace_source, &self.always_replace_target);
                    } else {
                        self.matcher_str = suggested_matcher.clone();
                    }
                }

                self.suggested_matcher = Some(suggested_matcher);
                self.suggested_lenient_matcher = suggested_lenient_matcher;
                self.matcher_suggestion_receiver = None;
            }
        }

        egui::ScrollArea::both()
            .max_height(f32::INFINITY)
            .show(ui, |ui| {
                let mut selection = None;

                ui.horizontal(|ui| {
                    if let Some(rule) =
                        Rule::from_matcher(self.matcher_str.as_str().into(), self.files)
                    {
                        if ui
                            .add(egui::Button::new("Add").fill(Color32::DARK_GREEN))
                            .clicked()
                        {
                            outer_rules.insert(rule);
                            if let Some(rule_file) = &self.rule_file {
                                outer_rules.save(rule_file);
                            }
                            self.renew_suggestion(outer_rules, ui.ctx().clone());
                        }
                    }
                    if ui.button("skip").clicked() {
                        self.skip_count += 1;
                        self.renew_suggestion(outer_rules, ui.ctx().clone());
                    }
                    ui.label("Enter the rule matcher:");

                    let output = egui::TextEdit::singleline(&mut self.matcher_str)
                        .desired_width(40.0)
                        .clip_text(false)
                        .id_source("rule_list_text_edit")
                        .show(ui);

                    if let Some(cursor_range) = output.cursor_range {
                        selection = self.expand_selection(cursor_range);
                    }
                });

                ui.horizontal(|ui| {
                    ui.label("Always replace");
                    ui.text_edit_singleline(&mut self.always_replace_source);
                    ui.label("with");
                    ui.text_edit_singleline(&mut self.always_replace_target);
                });

                let mut hovered_matcher_str = self.display_replacements(ui);
                self.prev_selection = selection;

                if !self.path_lists.is_empty() {
                    ui.separator();

                    let matcher = if let Some(matcher_str) = &hovered_matcher_str {
                        PathMatcher::from(&**matcher_str)
                    } else {
                        PathMatcher::from(&*self.matcher_str)
                    };

                    ui.horizontal(|ui| {
                        ui.add_sized([70.0, 0.0], egui::Label::new("current"));
                        ui.separator();
                        self.display_paths_lists(ui, matcher);
                    });

                    ui.separator();

                    for (matcher_str, name) in [
                        (
                            self.suggested_matcher
                                .as_deref()
                                .map(Into::<PathMatcher>::into),
                            "suggested",
                        ),
                        (
                            self.suggested_lenient_matcher
                                .as_deref()
                                .map(Into::<PathMatcher>::into),
                            "lenient",
                        ),
                    ] {
                        if let Some(matcher) = matcher_str {
                            ui.horizontal(|ui| {
                                let response = ui.add_sized(
                                    [70.0, 0.0],
                                    egui::Label::new(name).sense(egui::Sense::click()),
                                );

                                let rendered = format!("{matcher}");
                                if response.clicked() {
                                    self.matcher_str = rendered;
                                } else if response.hovered() {
                                    response.on_hover_ui_at_pointer(|ui| {
                                        ui.label(&rendered);
                                    });
                                    hovered_matcher_str = Some(rendered);
                                }

                                ui.separator();
                                self.display_paths_lists(ui, matcher);
                            });
                            ui.separator();
                        }
                    }
                }

                let rule = Rule::from_matcher(
                    hovered_matcher_str
                        .as_deref()
                        .unwrap_or(self.matcher_str.as_str())
                        .into(),
                    self.files,
                );

                let mut storage = RuleStorage::new();
                if let Some(rule) = rule {
                    ui.horizontal(|ui| {
                        rule.show(ui, false);
                    });

                    storage.insert(rule);
                }

                ui.separator();

                self.matching_changes.display(ui, &storage);
            });
    }

    /// Displays the possible replacements for the current selection.
    fn display_replacements(&mut self, ui: &mut egui::Ui) -> Option<String> {
        let mut hovered_matcher_str = None;

        if let Some(selection) = self.prev_selection.clone() {
            if selection.end <= self.matcher_str.len() {
                ui.horizontal(|ui| {
                    let mut lit = None;
                    let matcher_str = self.matcher_str.clone();

                    if !PathMatcher::from(&matcher_str[selection.clone()]).is_literal() {
                        let current_matcher = PathMatcher::from(matcher_str.as_str());

                        if let Some(file) = self
                            .files
                            .alphabetical_order()
                            .find(|file| current_matcher.matches_path(file.path))
                        {
                            let matching_path = file.path;
                            let start_matcher = PathMatcher::from(&matcher_str[..selection.start]);
                            let end_matcher = PathMatcher::from(&matcher_str[..selection.end]);

                            if let (Some(start), Some(end)) = (
                                start_matcher.match_len(matching_path),
                                end_matcher.match_len(matching_path),
                            ) {
                                lit = Some(&matching_path[start..end]);
                            }
                        }
                    } else {
                        lit = Some(&matcher_str[selection.clone()]);
                    }

                    if let Some(lit) = lit {
                        let prefix = &matcher_str[..selection.start];
                        let suffix = &matcher_str[selection.end..];

                        let mut show_button = |text: &str, replacement: String| {
                            let response = ui.button(text);

                            if response.clicked() {
                                self.matcher_str = replacement;
                            } else if response.hovered() {
                                hovered_matcher_str = Some(replacement);
                            }
                        };

                        if lit != &matcher_str[selection.clone()] {
                            show_button(
                                lit,
                                format!("{prefix}{}{suffix}", PathMatcherPart::Literal(lit.into())),
                            );
                        }

                        for (part, _) in
                            crate::path_matcher::possible_replacements(lit, true, false, &None)
                        {
                            show_button(&format!("{}", part), format!("{prefix}{}{suffix}", part));
                        }
                    }
                });
            }
        }

        hovered_matcher_str
    }

    /// Returns the byte indices of the current selection.
    fn expand_selection(
        &self,
        cursor_range: egui::text_edit::CursorRange,
    ) -> Option<std::ops::Range<usize>> {
        let mut cursors = [
            cursor_range.primary.ccursor.index,
            cursor_range.secondary.ccursor.index,
        ];
        cursors.sort();

        // FIXME: should this maybe consider grapheme clusters instead of chars?
        let [start, end] =
            cursors.map(|pos| self.matcher_str.char_indices().nth(pos).map(|(idx, _)| idx));

        let (start, end) = if let (Some(start), Some(end)) = (start, end) {
            match (
                self.matcher_str[..start].split('<').next_back(),
                self.matcher_str[end..].split_inclusive('>').next(),
            ) {
                (Some(before), Some(after))
                    if (self.matcher_str[..start].contains('<')
                        || self.matcher_str[start..].starts_with('<'))
                        && (self.matcher_str[..end].ends_with('>') || after.ends_with('>')) =>
                {
                    let new_start = if self.matcher_str[start..].starts_with('<') {
                        start
                    } else {
                        start - before.len() - 1
                    };
                    let new_end = if self.matcher_str[..end].ends_with('>') {
                        end
                    } else {
                        end + after.len()
                    };

                    let matcher = PathMatcher::from(&self.matcher_str[new_start..new_end]);
                    if !matcher.is_literal() && matcher.parts.len() == 1 {
                        (Some(new_start), Some(new_end))
                    } else {
                        (Some(start), Some(end))
                    }
                }
                _ => (Some(start), Some(end)),
            }
        } else {
            (start, end)
        };

        match (start, end) {
            (Some(start), Some(end)) if start != end => Some(start..end),
            (Some(start), None) => Some(start..self.matcher_str.len()),
            _ => None,
        }
    }

    /// Displays the matching paths lists for the current glob string.
    fn display_paths_lists(&mut self, ui: &mut egui::Ui, matcher: PathMatcher) {
        ui.horizontal(|ui| {
            let mut first = true;

            let mut names = self.path_lists.keys().collect::<Vec<_>>();
            // sorts the names assuming the following formats
            // - `<name>_<os>`
            // - `<year>-<month>_<os>`
            names.sort_by_key(|name| {
                // unwraps are safe, because split always returns at least one element
                let first_part = name.split('_').next().unwrap();
                let os = name.split('_').last().unwrap();

                (
                    os,
                    first_part.chars().any(|c| c.is_alphabetic()),
                    first_part,
                )
            });

            for name in names {
                let matching_paths = if matcher.parts.is_empty() {
                    Vec::new()
                } else {
                    self.path_lists[name]
                        .matching_paths(matcher.clone())
                        .collect()
                };

                let selected_os = self
                    .selected_path_list
                    .as_ref()
                    .map(|name| name.split('_').last().unwrap());

                if first {
                    first = false;
                } else {
                    ui.separator();
                }

                let (symb, color) = match matching_paths.len() {
                    0 => ("❌", Color32::RED),
                    1 => ("✅", Color32::GREEN),
                    _ => ("?", Color32::YELLOW),
                };

                let text_color = if Some(name) == self.selected_path_list.as_ref() {
                    Color32::YELLOW
                } else if let Some(selected_os) = selected_os && name.ends_with(selected_os) {
                    Color32::LIGHT_YELLOW
                } else {
                    ui.style().noninteractive().text_color()
                };

                let response = utils::sub_ui(ui, egui::Sense::click(), 0.0, |ui| {
                    ui.add_sized([250.0, 0.0], |ui: &mut egui::Ui| {
                        let mut resp = None;
                        ui.with_layout(
                            egui::Layout::left_to_right(egui::Align::LEFT)
                                .with_cross_align(egui::Align::Center),
                            |ui| {
                                ui.label(egui::RichText::new(symb).color(color).size(20.0));
                                resp = Some(ui.label(egui::RichText::new(name).color(text_color)));
                                if matching_paths.len() > 1 {
                                    ui.label(format!("({})", matching_paths.len()));
                                }
                            },
                        );

                        resp.unwrap()
                    });
                });

                if !matching_paths.is_empty() && response.hovered() {
                    egui::show_tooltip_at_pointer(ui.ctx(), "matching paths hover".into(), |ui| {
                        ui.vertical(|ui| {
                            for path in &matching_paths {
                                ui.label(*path);
                            }
                        });
                    });
                }

                if response.clicked() {
                    if Some(name) != self.selected_path_list.as_ref() {
                        self.selected_path_list = Some(name.to_string());
                    } else {
                        self.selected_path_list = None;
                    }
                }
            }
        });
    }

    /// Renews the suggestion for the next rule to create.
    fn renew_suggestion(&mut self, outer_rules: &RuleStorage, ctx: egui::Context) {
        let (selected_path_list, path_lists): (_, Vec<_>) =
            if let Some(selected_path_list) = &self.selected_path_list {
                let os = selected_path_list.split('_').next_back().unwrap();

                (
                    self.path_lists
                        .iter()
                        .find(|(name, _)| name == &selected_path_list)
                        .map(|(_, list)| list),
                    self.path_lists
                        .iter()
                        .filter_map(|(name, list)| name.ends_with(os).then_some(list))
                        .collect(),
                )
            } else {
                (None, self.path_lists.values().collect())
            };

        self.matcher_str.clear();
        self.suggested_matcher = None;
        self.suggested_lenient_matcher = None;

        let (sender, receiver) = std::sync::mpsc::channel();

        let files = self.files;
        let outer_rules = outer_rules.clone();
        let skip_count = self.skip_count;
        std::thread::spawn(move || {
            let matchers = suggest_matcher(
                files,
                &outer_rules,
                selected_path_list,
                &path_lists,
                skip_count,
            )
            .map(|(matcher, lenient_matcher)| {
                (
                    format!("{matcher}",),
                    lenient_matcher.map(|matcher| format!("{matcher}")),
                )
            })
            .unwrap_or_else(|| (String::new(), None));

            sender.send(matchers).ok();
            ctx.request_repaint();
        });

        self.matcher_suggestion_receiver = Some(receiver);
    }
}
