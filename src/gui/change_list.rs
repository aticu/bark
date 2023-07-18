//! Implements displaying of a list of changes.

use std::hash::Hash;

use eframe::{egui, epaint::Color32};

use crate::{
    file::{File, FileId, FileOrder, FileScoreCache, Files},
    fs_tree::{self, FsTree, FsTreeIter},
    future_value::FutureValue,
    path_matcher::PathMatcher,
    rules::{Rule, RuleStorage},
};

/// The font used for normal text such as the time and the path.
const TEXT_FONT: egui::FontId = egui::FontId {
    size: 13.0,
    family: egui::FontFamily::Proportional,
};

/// The background color that a hovered row is highlighted with.
const HOVERED_HIGHLIGHT_COLOR: Color32 = Color32::WHITE;

/// How close to the highlight color the hovered entry should get.
const HOVERED_HIGHLIGHT_PERCENT: f64 = 0.1;

/// The type of threshhold to apply.
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
enum ThreshholdType {
    /// Don't match anything when matching is requested.
    None,
    /// Match things with a match score that is smaller than the value.
    Smaller,
    /// Match things with a match score that is greater than the value.
    Greater,
    /// Match all the things.
    All,
}

/// Determines the thresh hold for rule filtering.
struct Threshholder {
    /// Whether to include matched files or exclude them.
    include_unmatched: bool,
    /// The value of the threshhold.
    val: f64,
    /// The type of threshhold to apply.
    ty: ThreshholdType,
}

impl Hash for Threshholder {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.include_unmatched.hash(state);
        self.val.to_ne_bytes().hash(state);
        self.ty.hash(state);
    }
}

impl Threshholder {
    /// Displays the threshholder into the `ui`.
    fn display(&mut self, ui: &mut egui::Ui) {
        ui.label("showing all files");

        if ui
            .button(if self.include_unmatched {
                "including"
            } else {
                "excluding"
            })
            .clicked()
        {
            self.include_unmatched = !self.include_unmatched;
        }

        ui.label("files not matching a rule and include");

        match self.ty {
            ThreshholdType::None => {
                if ui.button("nothing else").clicked() {
                    self.ty = ThreshholdType::Smaller;
                }
            }
            ThreshholdType::Smaller => {
                if ui
                    .button("matching files files with score smaller than")
                    .clicked()
                {
                    self.ty = ThreshholdType::Greater;
                }
            }
            ThreshholdType::Greater => {
                if ui
                    .button("matching files files with score greater than")
                    .clicked()
                {
                    self.ty = ThreshholdType::All;
                }
            }
            ThreshholdType::All => {
                if ui.button("everything else").clicked() {
                    self.ty = ThreshholdType::None;
                }
            }
        }

        if matches!(self.ty, ThreshholdType::Smaller | ThreshholdType::Greater) {
            ui.add(egui::Slider::new(&mut self.val, 0.0..=100.0).text("Score"));
        }
    }

    /// Determines if the given file should be included.
    fn should_include_file(&self, match_score: Option<f64>) -> bool {
        match match_score {
            Some(score) => match (self.ty, self.val) {
                (ThreshholdType::Greater, thresh) => score > thresh / 100.0,
                (ThreshholdType::Smaller, thresh) => score < thresh / 100.0,
                (ThreshholdType::None, _) => !self.include_unmatched,
                (_, _) => true,
            },
            None => self.ty == ThreshholdType::All || self.include_unmatched,
        }
    }
}

/// The context for a single drawing of the change list.
struct DrawCtx<'draw> {
    /// The dimensions of the time part of a row.
    time_dimensions: egui::Vec2,
    /// The dimensions of the match score part of a row.
    match_score_dimensions: egui::Vec2,
    /// The current position of drawing.
    current_pos: egui::Pos2,
    /// Whether anything was hovered so far.
    any_hovered: bool,
    /// The rules to use for drawing.
    rules: &'draw RuleStorage,
}

impl<'draw> DrawCtx<'draw> {
    /// Creates the context for a single drawing of the change list.
    fn new(ui: &mut egui::Ui, rules: &'draw RuleStorage, change_height: f32) -> Self {
        let mut time_dimensions = ui
            .painter()
            .layout_no_wrap(
                String::from("9999±999s"),
                TEXT_FONT,
                ui.style().noninteractive().text_color(),
            )
            .rect
            .size();
        time_dimensions.y = time_dimensions.y.max(change_height);
        time_dimensions.x += 2.0; // leave a small margin

        let mut match_score_dimensions = ui
            .painter()
            .layout_no_wrap(
                String::from("100"),
                TEXT_FONT,
                ui.style().noninteractive().text_color(),
            )
            .rect
            .size();
        match_score_dimensions.y = match_score_dimensions.y.max(change_height);
        match_score_dimensions.x += 2.0; // leave a small margin

        Self {
            time_dimensions,
            match_score_dimensions,
            current_pos: ui.cursor().left_top(),
            any_hovered: false,
            rules,
        }
    }
}

/// A list of changes that occurred.
pub(crate) struct ChangeList {
    /// The source list of changed files.
    files: &'static Files,
    /// The name of this change list.
    name: &'static str,
    /// The last time in seconds in the changed files.
    ///
    /// This is used to compute the background color for the displayed times.
    last_time: Option<f64>,
    /// The order in which the files should be sorted.
    ///
    /// If `None`, the files are sorted structurally in a tree.
    order_type: Option<FileOrder>,
    /// The tree view of `files`.
    ///
    /// If there is a `FileId`, its changes will be shown as in a flat view.
    ///
    /// The `bool` indicates whether the contents of a directory should be hidden.
    file_tree: FsTree<(Option<FileId>, bool)>,
    /// The currently hovered element.
    hovered: Option<ChangeListElement>,
    /// The threshhold for rule filtering.
    threshhold: Threshholder,
    /// The height of a single change field.
    change_height: f32,
    /// The number of files filtered out.
    filtered_files: usize,
    /// The number of files shown.
    shown_files: usize,
    /// The cache for file match scores.
    file_score_cache: FutureValue<FileScoreCache>,
    /// The maximum size of the change list last frame.
    last_frame_size: Option<egui::Vec2>,
    /// The state hash from last frame.
    last_frame_state_hash: Option<u64>,
    /// The search text entered into the search box.
    search_text: String,
}

impl ChangeList {
    /// Creates a new list of changes to display from the given files.
    pub(crate) fn new(
        files: &'static Files,
        name: &'static str,
        include_unmatched: bool,
        order_type: Option<FileOrder>,
    ) -> Self {
        let mut file_tree = FsTree::new();
        for file in files.alphabetical_order() {
            file_tree.insert(file.path, (Some(file.file_id), false));
        }
        ChangeList {
            files,
            name,
            last_time: files.chronological_order().last().and_then(|file| {
                file.file
                    .change_time
                    .as_ref()
                    .map(|time| time.avg.as_seconds_f64())
            }),
            order_type,
            file_tree,
            hovered: None,
            threshhold: Threshholder {
                include_unmatched,
                val: 50.0,
                ty: ThreshholdType::None,
            },
            change_height: 24.0,
            filtered_files: 0,
            shown_files: 0,
            file_score_cache: Default::default(),
            last_frame_size: None,
            last_frame_state_hash: None,
            search_text: String::new(),
        }
    }

    /// Displays the change list into the given `Ui`.
    pub(crate) fn display(&mut self, ui: &mut egui::Ui, rules: &RuleStorage) {
        self.draw_controls(ui);

        self.shown_files = 0;
        self.filtered_files = 0;

        let state_hash = self.state_hash();
        if Some(state_hash) != self.last_frame_state_hash {
            self.last_frame_state_hash = Some(state_hash);
            self.last_frame_size = None;
        }

        if self.file_score_cache.update_check(rules) {
            let ctx = ui.ctx().clone();
            self.file_score_cache
                .update((rules.clone(), self.files), move || ctx.request_repaint());
            self.last_frame_size = None;
        }

        if self.file_score_cache.is_current(rules) {
            egui::ScrollArea::both()
                .auto_shrink([false, true])
                .id_source(self.name)
                .show(ui, |ui| {
                    let mut draw_ctx = DrawCtx::new(ui, rules, self.change_height);
                    let start_pos = draw_ctx.current_pos;
                    let mut size = egui::Vec2::ZERO;

                    match self.order_type {
                        Some(order) => {
                            self.draw_flat_list(ui, &mut draw_ctx, &mut size, order);
                        }
                        None => {
                            self.draw_tree(ui, &mut draw_ctx, &mut size);
                        }
                    }

                    if !draw_ctx.any_hovered {
                        self.hovered = None;
                    }

                    if let Some(last_frame_size) = self.last_frame_size {
                        // extend the UI to the correct size
                        ui.allocate_rect(
                            egui::Rect::from_min_size(
                                start_pos + last_frame_size,
                                egui::Vec2::ZERO,
                            ),
                            egui::Sense::hover(),
                        );
                    } else {
                        self.last_frame_size = Some(size);
                    }
                });
        } else {
            ui.horizontal(|ui| {
                ui.spinner();
                ui.label("computing change list");
            });
        }
    }

    /// Draws the controls at the top of the change list.
    fn draw_controls(&mut self, ui: &mut egui::Ui) {
        ui.horizontal(|ui| {
            ui.label(self.name);

            if ui
                .button(match self.order_type {
                    Some(FileOrder::Alphabetical) => "sorted alphabetically",
                    Some(FileOrder::Chronological) => "sorted chronologically",
                    None => "sorted structurally",
                })
                .clicked()
            {
                self.order_type = match self.order_type {
                    Some(FileOrder::Alphabetical) => Some(FileOrder::Chronological),
                    Some(FileOrder::Chronological) => None,
                    None => Some(FileOrder::Alphabetical),
                }
            }

            self.threshhold.display(ui);

            ui.label(format!(
                "(showing {}, filtered {})",
                self.shown_files, self.filtered_files
            ));

            ui.add(
                egui::Slider::new(&mut self.change_height, 20.0..=40.0)
                    .text("Zoom")
                    .show_value(false),
            );
        });

        ui.horizontal(|ui| {
            ui.label("Search:");
            ui.text_edit_singleline(&mut self.search_text);
        });
    }

    /// Draws a directory tree in the change list.
    fn draw_tree(&mut self, ui: &mut egui::Ui, draw_ctx: &mut DrawCtx, size: &mut egui::Vec2) {
        let start_pos = draw_ctx.current_pos;
        let mut y = 0;
        let mut iter = FsTreeIter::new();
        let mut tree =
            self.file_tree
                .copy_filtered(|(file_id, collapsed)| match (file_id, collapsed) {
                    (_, true) => fs_tree::FilterResult::DiscardChildren,
                    (Some(file_id), false) => {
                        let file = self.files.get(*file_id).unwrap();
                        let match_score = self
                            .file_score_cache
                            .get()
                            .unwrap()
                            .get_score(*file_id)
                            .flatten();

                        if self.threshhold.should_include_file(match_score)
                            && (self.search_text.is_empty()
                                || file
                                    .paths
                                    .iter()
                                    .any(|path| path.contains(&self.search_text)))
                        {
                            fs_tree::FilterResult::Keep
                        } else {
                            fs_tree::FilterResult::Discard
                        }
                    }
                    (None, false) => fs_tree::FilterResult::Discard,
                });

        while let Some(result) = iter.next(&mut tree) {
            y += 1;
            let y = y - 1;
            let file_id = result.value.0;

            if file_id.is_some() {
                self.shown_files += 1;
            }

            let file = file_id.and_then(|file_id| self.files.get(file_id));
            let match_score = file_id
                .and_then(|file_id| self.file_score_cache.get().unwrap().get_score(file_id))
                .flatten();
            self.draw_changes(ui, draw_ctx, y, file, match_score);

            if result.name != "/" {
                let mut collapsed = result.value.1;
                self.draw_dir_markers(
                    ui,
                    draw_ctx,
                    result.parents,
                    result.has_children || result.value.1,
                    &mut collapsed,
                );

                if collapsed != result.value.1 {
                    self.file_tree.get_mut(&result.path()).unwrap().val_mut().1 = collapsed;
                }
            }
            self.draw_path(ui, draw_ctx, file, &result.name, y);

            size.x = (draw_ctx.current_pos.x - start_pos.x).max(size.x);
            size.y += draw_ctx.time_dimensions.y;

            draw_ctx.current_pos.x = start_pos.x;
            draw_ctx.current_pos.y += draw_ctx.time_dimensions.y;
        }

        self.filtered_files = self.files.alphabetical_order().count() - self.shown_files;
    }

    /// Draws the changes as a flat list as opposed to a tree.
    fn draw_flat_list(
        &mut self,
        ui: &mut egui::Ui,
        draw_ctx: &mut DrawCtx,
        size: &mut egui::Vec2,
        order: FileOrder,
    ) {
        let start_pos = draw_ctx.current_pos;
        for (y, file) in self.files.iter(order).enumerate() {
            let match_score = self
                .file_score_cache
                .get()
                .unwrap()
                .get_score(file.file_id)
                .flatten();

            if !self.threshhold.should_include_file(match_score)
                || !file.path.contains(&self.search_text)
            {
                self.filtered_files += 1;
                continue;
            }
            self.shown_files += 1;

            self.draw_changes(ui, draw_ctx, y, Some(file.file), match_score);
            self.draw_path(ui, draw_ctx, Some(file.file), file.path, y);

            size.x = (draw_ctx.current_pos.x - start_pos.x).max(size.x);
            size.y += draw_ctx.time_dimensions.y;

            draw_ctx.current_pos.x = start_pos.x;
            draw_ctx.current_pos.y += draw_ctx.time_dimensions.y;
        }
    }

    /// Draws the changes in the change list.
    ///
    /// Returns true if the row was visible.
    fn draw_changes(
        &mut self,
        ui: &mut egui::Ui,
        draw_ctx: &mut DrawCtx,
        y: usize,
        file: Option<&File>,
        match_score: Option<f64>,
    ) -> bool {
        let row_height = draw_ctx
            .time_dimensions
            .y
            .max(draw_ctx.match_score_dimensions.y)
            .max(self.change_height)
            .max(TEXT_FONT.size);

        let row_rect =
            egui::Rect::from_min_size(draw_ctx.current_pos, egui::vec2(f32::INFINITY, row_height));

        if !ui.is_rect_visible(row_rect) && self.last_frame_size.is_some() {
            // we've already laid everything out, no need to redo the work
            return false;
        }

        if let Some(file) = file {
            {
                self.draw_time(ui, draw_ctx, file, y);
                draw_ctx.current_pos.x += ui.style().spacing.item_spacing.x;
            }

            {
                self.draw_score(ui, draw_ctx, file, match_score, y);
                draw_ctx.current_pos.x += ui.style().spacing.item_spacing.x;
            }

            for (x, change) in file.changes.iter().enumerate() {
                let is_highlighted = if let Some(hovered) = self.hovered {
                    if hovered.row == y {
                        true
                    } else if let ChangeListElementType::Change { column } = hovered.element_type {
                        column == x
                    } else {
                        false
                    }
                } else {
                    false
                };

                let change_response = self.draw_single_change(
                    ui,
                    &mut draw_ctx.current_pos,
                    x,
                    is_highlighted,
                    change,
                );
                if ui.rect_contains_pointer(change_response.rect) && !draw_ctx.any_hovered {
                    self.hovered = Some(ChangeListElement {
                        row: y,
                        element_type: ChangeListElementType::Change { column: x },
                    });
                    draw_ctx.any_hovered = true;
                }
            }

            draw_ctx.current_pos.x += ui.style().spacing.item_spacing.x;
        } else {
            draw_ctx.current_pos.x += ui.style().spacing.item_spacing.x * 3.0
                + draw_ctx.time_dimensions.x
                + draw_ctx.match_score_dimensions.x
                + self.files.width() as f32 * self.change_height / 2.0;
        }

        ui.is_rect_visible(row_rect)
    }

    /// Draws a component of known size in a row.
    fn draw_sized_row_component(
        &mut self,
        ui: &mut egui::Ui,
        draw_ctx: &mut DrawCtx,
        y: usize,
        size: egui::Vec2,
        element_type: ChangeListElementType,
        draw: impl FnOnce(&mut egui::Ui, egui::Rect),
    ) -> egui::Response {
        let rect = egui::Rect::from_min_size(draw_ctx.current_pos, size);
        draw_ctx.current_pos.x += rect.width();

        let response = ui.allocate_rect(rect, egui::Sense::hover());
        if ui.is_rect_visible(rect) {
            draw(ui, rect);
        }

        if response.hovered() && !draw_ctx.any_hovered {
            draw_ctx.any_hovered = true;
            self.hovered = Some(ChangeListElement {
                row: y,
                element_type,
            });
        }

        response
    }

    /// Draws the time for a single row.
    ///
    /// Returns whether the time was visible.
    fn draw_time(
        &mut self,
        ui: &mut egui::Ui,
        draw_ctx: &mut DrawCtx,
        file: &File,
        y: usize,
    ) -> bool {
        let last_time = self.last_time;
        let time_response = self.draw_sized_row_component(
            ui,
            draw_ctx,
            y,
            draw_ctx.time_dimensions,
            ChangeListElementType::Time,
            |ui, rect| {
                let time = file
                    .change_time
                    .as_ref()
                    .map(|time| time.avg.as_seconds_f64());

                let mut time_text_color = ui.style().noninteractive().text_color();

                if let (Some(last_time), Some(time)) = (last_time, time) {
                    ui.painter().rect_filled(
                        rect,
                        0.0,
                        super::lerp_color(Color32::BLACK, Color32::BLUE, time / last_time),
                    );
                    time_text_color = Color32::WHITE;
                }

                ui.painter().text(
                    rect.center(),
                    egui::Align2::CENTER_CENTER,
                    if let Some(changetime) = &file.change_time {
                        std::borrow::Cow::Owned(format!("{}", changetime))
                    } else {
                        std::borrow::Cow::Borrowed("????±???s")
                    },
                    TEXT_FONT,
                    time_text_color,
                );
            },
        );

        ui.is_rect_visible(time_response.rect)
    }

    /// Draws the score for a single row.
    ///
    /// Returns whether the score was visible.
    fn draw_score(
        &mut self,
        ui: &mut egui::Ui,
        draw_ctx: &mut DrawCtx,
        file: &File,
        match_score: Option<f64>,
        y: usize,
    ) -> bool {
        let score_response = self.draw_sized_row_component(
            ui,
            draw_ctx,
            y,
            draw_ctx.match_score_dimensions,
            ChangeListElementType::MatchScore,
            |ui, rect| match match_score {
                Some(score) => {
                    ui.painter().rect_filled(
                        rect,
                        0.0,
                        super::lerp_color(Color32::RED, Color32::GREEN, score),
                    );
                    ui.painter().text(
                        rect.center(),
                        egui::Align2::CENTER_CENTER,
                        format!("{:.0}", score * 100.0),
                        TEXT_FONT,
                        Color32::BLACK,
                    );
                }
                None => {
                    ui.painter().rect_filled(rect, 0.0, Color32::BROWN);
                    ui.painter().text(
                        rect.center(),
                        egui::Align2::CENTER_CENTER,
                        "???",
                        TEXT_FONT,
                        Color32::BLACK,
                    );
                }
            },
        );
        if score_response.hovered() {
            let mut iter = draw_ctx.rules.rules_matching(file).peekable();

            if iter.peek().is_some() {
                egui::show_tooltip_at_pointer(ui.ctx(), "rule_display_tooltip".into(), |ui| {
                    for rule in iter {
                        rule.show(
                            ui,
                            9.0,
                            Some(3),
                            true,
                            Rule::from_matcher(
                                PathMatcher::from_literal_path(file.path()),
                                self.files,
                            ),
                        );
                    }
                });
            }
        }

        ui.is_rect_visible(score_response.rect)
    }

    /// Draws the path for a single row.
    fn draw_path(
        &mut self,
        ui: &mut egui::Ui,
        draw_ctx: &mut DrawCtx,
        file: Option<&File>,
        path: &str,
        y: usize,
    ) {
        let path_rect = ui.painter().text(
            egui::pos2(
                draw_ctx.current_pos.x,
                draw_ctx.current_pos.y + draw_ctx.time_dimensions.y / 2.0,
            ),
            egui::Align2::LEFT_CENTER,
            path,
            TEXT_FONT,
            ui.style().noninteractive().text_color(),
        );
        let path_response = ui.allocate_rect(path_rect, egui::Sense::hover()).hovered();
        if path_response && !draw_ctx.any_hovered {
            self.hovered = Some(ChangeListElement {
                row: y,
                element_type: ChangeListElementType::Path,
            });
            draw_ctx.any_hovered = true;

            if let Some(file) = file {
                egui::show_tooltip_at_pointer(ui.ctx(), "hover_tooltip".into(), |ui| {
                    ui.label(file.paths.join("\n"))
                });
            }
        }

        draw_ctx.current_pos.x += path_rect.width();
    }

    /// Draws the directory tree prefix.
    ///
    /// The `dir_info` refers to whether the folder at that level is at it's last level or not.
    fn draw_dir_markers(
        &mut self,
        ui: &mut egui::Ui,
        draw_ctx: &mut DrawCtx,
        dir_info: &[(String, bool)],
        allow_collapse: bool,
        collapsed: &mut bool,
    ) {
        let stroke = egui::Stroke::new(1.0, ui.style().noninteractive().text_color());

        let no_branch_ongoing = |ui: &mut egui::Ui, rect: egui::Rect| {
            let painter = ui.painter();
            painter.line_segment([rect.center_top(), rect.center_bottom()], stroke);
        };
        let branch_ongoing = |ui: &mut egui::Ui, rect: egui::Rect| {
            let painter = ui.painter();
            painter.line_segment([rect.center_top(), rect.center_bottom()], stroke);
            painter.line_segment([rect.center(), rect.right_center()], stroke);
        };
        let just_branch = |ui: &mut egui::Ui, rect: egui::Rect| {
            let painter = ui.painter();
            painter.line_segment([rect.center_top(), rect.center()], stroke);
            painter.line_segment([rect.center(), rect.right_center()], stroke);
        };

        for (i, last_in_level) in dir_info.iter().enumerate() {
            let last_level = i == dir_info.len() - 1;
            let rect = egui::Rect::from_min_size(
                draw_ctx.current_pos,
                egui::vec2(self.change_height / 2.0, self.change_height),
            );
            draw_ctx.current_pos.x += rect.width();

            match (last_in_level.1, last_level) {
                (false, false) => no_branch_ongoing(ui, rect),
                (false, true) => branch_ongoing(ui, rect),
                (true, false) => (),
                (true, true) => just_branch(ui, rect),
            }

            if !last_in_level.1 || last_level {
                ui.allocate_rect(rect, egui::Sense::hover())
                    .on_hover_text_at_pointer(&last_in_level.0);
            }
        }

        if allow_collapse {
            let painter = ui.painter();
            let rect = egui::Rect::from_min_size(
                draw_ctx.current_pos,
                egui::vec2(self.change_height / 2.0, self.change_height),
            );
            draw_ctx.current_pos.x += rect.width();

            let half_triangle_height = self.change_height / 8.0;
            let triangle_width = self.change_height / 5.0;

            painter.line_segment([rect.left_center(), rect.center()], stroke);
            if *collapsed {
                painter.add(egui::epaint::PathShape {
                    points: vec![
                        rect.center() + egui::vec2(-half_triangle_height, triangle_width),
                        rect.center() + egui::vec2(-half_triangle_height, -triangle_width),
                        rect.center() + egui::vec2(half_triangle_height, 0.0),
                    ],
                    closed: true,
                    fill: Color32::DARK_RED,
                    stroke: egui::Stroke::NONE,
                });
            } else {
                painter.line_segment([rect.center(), rect.center_bottom()], stroke);
                painter.add(egui::epaint::PathShape {
                    points: vec![
                        rect.center() + egui::vec2(-triangle_width, -half_triangle_height),
                        rect.center() + egui::vec2(triangle_width, -half_triangle_height),
                        rect.center() + egui::vec2(0.0, half_triangle_height),
                    ],
                    closed: true,
                    fill: ui.style().noninteractive().text_color(),
                    stroke: egui::Stroke::NONE,
                });
            }

            if ui.allocate_rect(rect, egui::Sense::click()).clicked() {
                *collapsed = !*collapsed;
            }
        }
    }

    /// Draws a single change.
    fn draw_single_change(
        &self,
        ui: &mut egui::Ui,
        pos: &mut egui::Pos2,
        x: usize,
        is_highlighted: bool,
        change: &Option<sniff::MetaEntryDiff<time::Duration>>,
    ) -> egui::Response {
        let rect = egui::Rect::from_min_size(
            *pos,
            egui::vec2(self.change_height / 2.0, self.change_height),
        );
        pos.x += rect.width();

        let response = ui.allocate_rect(rect, egui::Sense::hover());
        if !ui.is_rect_visible(rect) {
            return response;
        }

        let (fg, bg) = change_colors(change, ui.style().noninteractive().text_color(), x);

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

        if ui.rect_contains_pointer(response.rect) {
            if let Some(summary) = change_summary(change) {
                egui::show_tooltip_at_pointer(ui.ctx(), "hover_tooltip".into(), |ui| {
                    ui.label(summary)
                });
            }
        }

        let Some(change) = change else { return response };

        self.draw_change_grid(painter, rect, fg, change.meta_info());

        response
    }

    /// Draws a grid of the changes that occurred in the given metadata.
    fn draw_change_grid(
        &self,
        painter: egui::Painter,
        rect: egui::Rect,
        text_color: Color32,
        meta: &sniff::MetadataInfo<time::Duration>,
    ) {
        /// The width of the grid of changes.
        const GRID_WIDTH: usize = 2;

        /// The height of the grid of changes.
        const GRID_HEIGHT: usize = 3;

        let grid_entries = [
            ("A", meta.accessed.is_changed()),
            ("M", meta.modified.is_changed()),
            ("C", meta.created.is_changed()),
            ("m", meta.inode_modified.is_changed()),
            (
                meta.changes
                    .iter()
                    .find(|c| matches!(c, sniff::MetadataChange::Size(_)))
                    .map(|c| match c {
                        sniff::MetadataChange::Size(change) if change.cmp().is_lt() => "G",
                        sniff::MetadataChange::Size(change) if change.cmp().is_gt() => "s",
                        _ => "S",
                    })
                    .unwrap_or("S"),
                meta.changes
                    .iter()
                    .any(|c| matches!(c, sniff::MetadataChange::Size(_))),
            ),
            ("I", meta.inode.is_changed()),
        ];
        assert!(grid_entries.len() <= GRID_HEIGHT * GRID_WIDTH);

        let mut grid_x = 0;
        let mut grid_y = 0;
        let x_step: f32 = self.change_height / 2.0 / GRID_WIDTH as f32;
        let y_step: f32 = self.change_height / GRID_HEIGHT as f32;

        for (text, should_display) in grid_entries {
            if should_display {
                painter.text(
                    rect.left_top() + egui::vec2(grid_x as f32 * x_step, grid_y as f32 * y_step),
                    eframe::emath::Align2::LEFT_TOP,
                    text,
                    egui::FontId {
                        size: self.change_height / GRID_HEIGHT as f32,
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

    /// The hash of the internal state of the change list.
    fn state_hash(&self) -> u64 {
        use std::{
            collections::hash_map::DefaultHasher,
            hash::{Hash as _, Hasher as _},
        };

        let mut hasher = DefaultHasher::new();
        self.order_type.hash(&mut hasher);
        self.threshhold.hash(&mut hasher);
        self.change_height.to_bits().hash(&mut hasher);
        self.filtered_files.hash(&mut hasher);
        self.shown_files.hash(&mut hasher);
        self.search_text.hash(&mut hasher);
        self.file_tree.hash(&mut hasher);
        hasher.finish()
    }
}

/// Computes the colors for a single change.
fn change_colors(
    change: &Option<sniff::MetaEntryDiff<time::Duration>>,
    default_fg: Color32,
    x: usize,
) -> (Color32, Color32) {
    const COLOR_TABLE: [Color32; 2] = [Color32::from_gray(40), Color32::from_gray(45)];
    let default_bg = COLOR_TABLE[x % COLOR_TABLE.len()];

    if let Some(change) = change {
        let meta = change.meta_info();

        if meta
            .changes
            .iter()
            .any(|c| !matches!(c, sniff::MetadataChange::Size(_)))
        {
            (Color32::BLACK, Color32::from_rgb(255, 0, 255))
        } else {
            use sniff::{
                EntryDiff::*,
                MetaEntryDiff::{Added, Deleted, EntryChange},
            };
            match change {
                Added(_) => (Color32::BLACK, Color32::GREEN),
                Deleted(_) => (Color32::BLACK, Color32::RED),
                EntryChange(FileChanged { .. }, meta) => {
                    if let Some(sniff::MetadataChange::Size(change)) = meta
                        .changes
                        .iter()
                        .find(|c| matches!(c, sniff::MetadataChange::Size(_)))
                    {
                        match change.cmp() {
                            std::cmp::Ordering::Less => (
                                Color32::BLACK,
                                super::lerp_color(
                                    Color32::from_rgb(100, 255, 0),
                                    Color32::from_rgb(220, 255, 0),
                                    change.from as f64 / change.to as f64,
                                ),
                            ),
                            std::cmp::Ordering::Equal => (Color32::BLACK, Color32::YELLOW),
                            std::cmp::Ordering::Greater => (
                                Color32::BLACK,
                                super::lerp_color(
                                    Color32::from_rgb(255, 100, 0),
                                    Color32::from_rgb(255, 220, 0),
                                    change.to as f64 / change.from as f64,
                                ),
                            ),
                        }
                    } else {
                        (Color32::BLACK, Color32::YELLOW)
                    }
                }
                EntryChange(SymlinkChanged { .. }, _) => (Color32::BLACK, Color32::LIGHT_YELLOW),
                EntryChange(TypeChange(_) | OtherChange, _) => {
                    (Color32::BLACK, Color32::from_rgb(0, 255, 255))
                }
                _ => (default_fg, default_bg),
            }
        }
    } else {
        (default_fg, default_bg)
    }
}

/// Refers to a single element in the given change list.
#[derive(Debug, Clone, Copy)]
struct ChangeListElement {
    /// The row that the element resides in.
    row: usize,
    /// The type of element that is being referred to.
    element_type: ChangeListElementType,
}

/// The type of a change list element.
#[derive(Debug, Clone, Copy)]
enum ChangeListElementType {
    /// Refers to the time in a row.
    Time,
    /// Refers to the match score in a row.
    MatchScore,
    /// Refers to a change in the given column in a row.
    Change {
        /// The column that is being referred to.
        column: usize,
    },
    /// Refers to a path in a row.
    Path,
}

/// Summarizes the given change into a string fitting a tooltip.
fn change_summary(change: &Option<sniff::MetaEntryDiff<time::Duration>>) -> Option<String> {
    let mut result = String::new();

    if let Some(change) = change {
        match change {
            sniff::MetaEntryDiff::Added(info) => {
                result.push_str("added\n");
                summarize_meta_info(&mut result, info);
            }
            sniff::MetaEntryDiff::Deleted(info) => {
                result.push_str("deleted\n");
                summarize_meta_info(&mut result, info);
            }
            sniff::MetaEntryDiff::MetaOnlyChange(info) => {
                summarize_meta_info(&mut result, info);
            }
            sniff::MetaEntryDiff::EntryChange(entry_diff, info) => {
                summarize_entry_diff(&mut result, entry_diff);
                summarize_meta_info(&mut result, info);
            }
        }
    } else {
        return None;
    }

    Some(result)
}

/// Summarizes the given metadata information into the given string.
fn summarize_meta_info(result: &mut String, info: &sniff::MetadataInfo<time::Duration>) {
    let write_timestamp = |result: &mut String, ts: &Option<time::Duration>| {
        if let Some(ts) = ts {
            result.push_str(&format!("{ts:.2}"));
        } else {
            result.push_str("<none>");
        }
    };

    result.push_str("inode: ");
    write_maybe_change(result, &info.inode, write_opt_val);
    result.push('\n');

    result.push_str("created: ");
    write_maybe_change(result, &info.created, write_timestamp);
    result.push('\n');

    result.push_str("modified: ");
    write_maybe_change(result, &info.modified, write_timestamp);
    result.push('\n');

    result.push_str("accessed: ");
    write_maybe_change(result, &info.accessed, write_timestamp);
    result.push('\n');

    result.push_str("inode modified: ");
    write_maybe_change(result, &info.inode_modified, write_timestamp);

    for change in &info.changes {
        result.push('\n');
        match change {
            sniff::MetadataChange::Size(size) => {
                result.push_str("size change: ");
                write_change(result, size, |result, val| {
                    result.push_str(&format!("{val}"))
                });
            }
            sniff::MetadataChange::NtfsAttributes(attr) => {
                result.push_str("ntfs attribute change: ");
                write_change(result, attr, write_opt_val);
            }
            sniff::MetadataChange::UnixPermissions(perm) => {
                result.push_str("unix permission change: ");
                write_change(result, perm, write_opt_val);
            }
            sniff::MetadataChange::Nlink(nlink) => {
                result.push_str("number of links change: ");
                write_change(result, nlink, write_opt_val);
            }
            sniff::MetadataChange::Uid(uid) => {
                result.push_str("user id change: ");
                write_change(result, uid, write_opt_val);
            }
            sniff::MetadataChange::Gid(gid) => {
                result.push_str("group id change: ");
                write_change(result, gid, write_opt_val);
            }
            sniff::MetadataChange::NamedStream(ty, val) => {
                result.push_str(&format!("named stream change ({ty:?}): "));
                write_change(result, val, |result, val| {
                    if let Some(val) = val {
                        match std::str::from_utf8(val) {
                            Ok(as_str) => {
                                result.push('"');
                                result.push_str(as_str);
                                result.push('"');
                            }
                            Err(_) => result.push_str(&format!("{val:?}")),
                        }
                    } else {
                        result.push_str("none");
                    }
                });
            }
        }
    }
}

/// Summarizes the given entry difference into the given string.
fn summarize_entry_diff(result: &mut String, diff: &sniff::EntryDiff) {
    let write_str = |result: &mut String, val: &String| {
        result.push_str(val);
    };
    match diff {
        sniff::EntryDiff::FileChanged { hash_change } => {
            result.push_str("file content changed: ");
            write_change(result, hash_change, write_str);
            result.push('\n');
        }
        sniff::EntryDiff::SymlinkChanged { path_change } => {
            result.push_str("symlink changed: ");
            write_change(result, path_change, write_str);
            result.push('\n');
        }
        sniff::EntryDiff::TypeChange(change) => {
            result.push_str("entry type changed: ");
            write_change(result, change, write_str);
            result.push('\n');
        }
        sniff::EntryDiff::OtherChange => result.push_str("other change\n"),
    }
}

/// Writes an optional value into the string.
fn write_opt_val<T: std::fmt::Display>(result: &mut String, val: &Option<T>) {
    if let Some(val) = val {
        result.push_str(&val.to_string());
    } else {
        result.push_str("<none>");
    }
}

/// Writes the given change into the string.
fn write_change<T>(
    result: &mut String,
    change: &sniff::Change<T>,
    mut write: impl FnMut(&mut String, &T),
) {
    write(result, &change.from);
    result.push_str(" -> ");
    write(result, &change.to);
}

/// Writes the given possible change into the string.
fn write_maybe_change<T>(
    result: &mut String,
    maybe_change: &sniff::MaybeChange<T>,
    mut write: impl FnMut(&mut String, &T),
) {
    match maybe_change {
        sniff::MaybeChange::Same(val) => write(result, val),
        sniff::MaybeChange::Change(change) => {
            write(result, &change.from);
            result.push_str(" -> ");
            write(result, &change.to);
        }
    }
}
