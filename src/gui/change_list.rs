//! Implements displaying of a list of changes.

use eframe::{egui, epaint::Color32};

use crate::{
    change_distribution::ChangeDistribution,
    change_event::ChangeEvent,
    file::{File, FileId, FileOrder, FileScoreCache, Files},
    fs_tree::{self, FsTree, FsTreeIter},
    future_value::FutureValue,
    rules::RuleStorage,
};

use super::file_filter::FileFilter;

/// The font used for normal text such as the time and the path.
const TEXT_FONT: egui::FontId = egui::FontId {
    size: 13.0,
    family: egui::FontFamily::Proportional,
};

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
                String::from(".999"),
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
    /// The filter filter to choose which files to show.
    file_filter: Option<FileFilter>,
    /// The height of a single change field.
    change_height: f32,
    /// Include only one copy of each file.
    include_only_one_copy: bool,
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
    pub(crate) fn new(files: &'static Files, name: &'static str, rule_writing: bool) -> Self {
        let include_only_one_copy = rule_writing;

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
            order_type: None,
            file_tree,
            hovered: None,
            file_filter: (!rule_writing).then_some(FileFilter::new()),
            change_height: 24.0,
            include_only_one_copy,
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

        if let Some(file_score_cache) = self.file_score_cache.get() {
            if let Some(file_filter) = &mut self.file_filter {
                file_filter.draw(ui, self.files, file_score_cache)
            }
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

        if let Some(FileOrder::Alphabetical) | None = self.order_type {
            ui.horizontal(|ui| {
                ui.checkbox(
                    &mut self.include_only_one_copy,
                    "include only one path per file",
                );
                if self.order_type.is_none() && ui.button("uncollapse all folders").clicked() {
                    self.file_tree
                        .mutate_vals(|(_, collapsed)| *collapsed = false);
                }
            });
        }

        ui.horizontal(|ui| {
            ui.label("Search:");
            ui.text_edit_singleline(&mut self.search_text);
        });
    }

    /// Draws a directory tree in the change list.
    fn draw_tree(&mut self, ui: &mut egui::Ui, draw_ctx: &mut DrawCtx, size: &mut egui::Vec2) {
        let start_pos = draw_ctx.current_pos;
        let row_height = self.row_height(draw_ctx);

        let mut y = 0;
        let mut iter = FsTreeIter::new();
        let mut tree = self.file_tree.clone_filtered(|(file_id, collapsed), path| {
            match (file_id, collapsed) {
                (_, true) => fs_tree::FilterResult::DiscardChildren,
                (Some(file_id), false) => {
                    if self.should_show_file(*file_id, Some(path))
                        && (!self.include_only_one_copy
                            || self.files.get(*file_id).unwrap().path() == path)
                    {
                        fs_tree::FilterResult::Keep
                    } else {
                        // don't discard the children, since they may need to be shown
                        fs_tree::FilterResult::Discard
                    }
                }
                (None, false) => fs_tree::FilterResult::Discard,
            }
        });

        while let Some(result) = iter.next(&mut tree) {
            y += 1;
            let y = y - 1;
            let mut file_id = result.value.0;

            if file_id
                .map(|file_id| !self.should_show_file(file_id, None))
                .unwrap_or(false)
            {
                file_id = None;
            }

            if file_id.is_some() {
                self.shown_files += 1;
            }

            if self.row_needs_drawing(ui, row_height, draw_ctx) {
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
            }

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
        let row_height = self.row_height(draw_ctx);

        for (y, file) in self.files.iter(order).enumerate() {
            let match_score = self
                .file_score_cache
                .get()
                .unwrap()
                .get_score(file.file_id)
                .flatten();

            if !self.should_show_file(file.file_id, Some(file.path)) {
                self.filtered_files += 1;
                continue;
            }
            self.shown_files += 1;

            if self.row_needs_drawing(ui, row_height, draw_ctx) {
                self.draw_changes(ui, draw_ctx, y, Some(file.file), match_score);
                self.draw_path(ui, draw_ctx, Some(file.file), file.path, y);

                size.x = (draw_ctx.current_pos.x - start_pos.x).max(size.x);
                size.y += draw_ctx.time_dimensions.y;
            }

            draw_ctx.current_pos.x = start_pos.x;
            draw_ctx.current_pos.y += draw_ctx.time_dimensions.y;
        }
    }

    /// Determine whether or not the file needs to be shown.
    fn should_show_file(&self, file_id: FileId, path: Option<&str>) -> bool {
        let Some(file) = self.files.get(file_id) else { return false };
        let match_score = self
            .file_score_cache
            .get()
            .unwrap()
            .get_score(file_id)
            .flatten();

        let should_include_file = self
            .file_filter
            .as_ref()
            .map(|file_filter| file_filter.should_include_file(match_score))
            .unwrap_or(match_score.is_some());

        let search_matches_path = self.search_text.is_empty()
            || file
                .paths
                .iter()
                .any(|path| path.contains(&self.search_text));

        let is_canonical_version = path.map(|path| path == file.path()).unwrap_or(true);

        should_include_file
            && search_matches_path
            && (self.include_only_one_copy || is_canonical_version)
    }

    /// Determines the row height for a single drawing.
    fn row_height(&self, draw_ctx: &DrawCtx) -> f32 {
        draw_ctx
            .time_dimensions
            .y
            .max(draw_ctx.match_score_dimensions.y)
            .max(self.change_height)
            .max(TEXT_FONT.size)
    }

    /// Determines if the current row needs to be drawn.
    fn row_needs_drawing(&self, ui: &egui::Ui, row_height: f32, draw_ctx: &DrawCtx) -> bool {
        let row_rect =
            egui::Rect::from_min_size(draw_ctx.current_pos, egui::vec2(f32::INFINITY, row_height));

        // we've already laid everything out, no need to redo the work
        ui.is_rect_visible(row_rect) || self.last_frame_size.is_none()
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
    ) {
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
                self.draw_single_change(change, ui, draw_ctx, x, y);
            }

            draw_ctx.current_pos.x += ui.style().spacing.item_spacing.x;
        } else {
            draw_ctx.current_pos.x += ui.style().spacing.item_spacing.x * 3.0
                + draw_ctx.time_dimensions.x
                + draw_ctx.match_score_dimensions.x
                + self.files.width() as f32 * self.change_height / 2.0;
        }
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
            |ui, rect| {
                ui.painter()
                    .rect_filled(rect, 0.0, super::utils::score_color(match_score));

                match match_score {
                    Some(score) => {
                        let text = if score >= 0.995_f64 {
                            String::from("100")
                        } else if score >= 0.1 {
                            format!("{:.1}", score * 100.0)
                        } else if score >= 0.01 {
                            format!("{:.2}", score * 100.0)
                        } else if score >= 0.00001 {
                            let mut text = format!("{:.3}", score * 100.0);
                            text.remove(0);
                            text
                        } else if score == 0.0 {
                            String::from("0")
                        } else {
                            let log10 = score.log10().floor() + 2.0;

                            if log10 > -10.0 {
                                format!("e{}", log10)
                            } else {
                                format!("{}", log10)
                            }
                        };
                        ui.painter().text(
                            rect.center(),
                            egui::Align2::CENTER_CENTER,
                            text,
                            TEXT_FONT,
                            Color32::BLACK,
                        );
                    }
                    None => {
                        ui.painter().text(
                            rect.center(),
                            egui::Align2::CENTER_CENTER,
                            "???",
                            TEXT_FONT,
                            Color32::BLACK,
                        );
                    }
                }
            },
        );
        if score_response.hovered() {
            let rule = draw_ctx.rules.best_matching_rule(file);
            let measured_distribution = ChangeDistribution::from_file(file);

            if rule.is_some() || measured_distribution.is_some() {
                egui::show_tooltip_at_pointer(ui.ctx(), "rule_display_tooltip".into(), |ui| {
                    if let Some(measured_distribution) = measured_distribution {
                        ui.label("Found:");
                        measured_distribution.show(ui);
                        measured_distribution.show_legend(ui);
                    }

                    if let Some(rule) = rule {
                        rule.display_file_match(ui, file);
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

    /// Draws a single change event.
    fn draw_single_change(
        &mut self,
        change: &Option<sniff::MetaEntryDiff<time::Duration>>,
        ui: &mut egui::Ui,
        draw_ctx: &mut DrawCtx,
        x: usize,
        y: usize,
    ) {
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

        const COLOR_TABLE: [Color32; 2] = [Color32::from_gray(40), Color32::from_gray(45)];
        let default_bg = COLOR_TABLE[x % COLOR_TABLE.len()];
        let rect = egui::Rect::from_min_size(
            draw_ctx.current_pos,
            egui::vec2(self.change_height / 2.0, self.change_height),
        );
        let change_response = ui.allocate_rect(rect, egui::Sense::hover());
        if ui.is_rect_visible(rect) {
            super::change_event::draw(
                ChangeEvent::measure(change),
                ui,
                rect,
                default_bg,
                is_highlighted,
                change.as_ref().and_then(|changes| {
                    changes
                        .meta_info()
                        .changes
                        .iter()
                        .find_map(|change| match change {
                            sniff::MetadataChange::Size(change) => Some(change),
                            _ => None,
                        })
                        .cloned()
                }),
            )
        }

        draw_ctx.current_pos.x += rect.width();

        if ui.rect_contains_pointer(change_response.rect) && !draw_ctx.any_hovered {
            if let Some(summary) = change_summary(change) {
                egui::show_tooltip_at_pointer(ui.ctx(), "hover_tooltip".into(), |ui| {
                    ui.label(summary)
                });
            }
            self.hovered = Some(ChangeListElement {
                row: y,
                element_type: ChangeListElementType::Change { column: x },
            });
            draw_ctx.any_hovered = true;
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
        self.file_filter.hash(&mut hasher);
        self.change_height.to_bits().hash(&mut hasher);
        self.filtered_files.hash(&mut hasher);
        self.shown_files.hash(&mut hasher);
        self.search_text.hash(&mut hasher);
        self.include_only_one_copy.hash(&mut hasher);
        self.file_tree.hash(&mut hasher);
        hasher.finish()
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
    let write_hash = |result: &mut String, val: &sniff::Hash| {
        result.push_str(&format!("{val:?}"));
    };
    match diff {
        sniff::EntryDiff::FileChanged { hash_change } => {
            result.push_str("file content changed: ");
            write_change(result, hash_change, write_hash);
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
