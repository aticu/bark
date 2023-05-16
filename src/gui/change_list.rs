//! Implements displaying of a list of changes.

use eframe::{egui, epaint::Color32};

use crate::file::{File, FileOrder, Files};

/// The font used for normal text such as the time and the path.
const TEXT_FONT: egui::FontId = egui::FontId {
    size: 13.0,
    family: egui::FontFamily::Proportional,
};

/// The font used for the change boxes.
const CHANGE_FONT: egui::FontId = egui::FontId {
    size: 13.0,
    family: egui::FontFamily::Monospace,
};

/// The size of a single change in the list.
const CHANGE_SIZE: egui::Vec2 = egui::vec2(10.0, 20.0);

/// The background color that a hovered row is highlighted with.
const HOVERED_HIGHLIGHT_COLOR: Color32 = Color32::WHITE;

/// How close to the highlight color the hovered entry should get.
const HOVERED_HIGHLIGHT_PERCENT: f64 = 0.1;

/// The background color that a non meta data change is highlighted with.
const NON_META_HIGHLIGHT_COLOR: Color32 = Color32::WHITE;

/// How close to the highlight color the non meta entry change should get.
const NON_META_HIGHLIGHT_PERCENT: f64 = 0.15;

/// A list of changes that occurred.
pub(crate) struct ChangeList {
    /// The source list of changed files.
    files: &'static Files,
    /// The name of this change list.
    name: &'static str,
    /// The last time in seconds in the changed files.
    last_time: Option<f64>,
    /// The order in which the files should be sorted.
    order_type: FileOrder,
    /// The currently hovered element.
    hovered: Option<ChangeListElement>,
}

impl ChangeList {
    /// Creates a new list of changes to display from the given files.
    pub(crate) fn new(files: &'static Files, name: &'static str) -> Self {
        ChangeList {
            files,
            name,
            last_time: files.chronological_order().last().and_then(|(_, file)| {
                file.change_time
                    .as_ref()
                    .map(|time| time.avg.as_seconds_f64())
            }),
            order_type: FileOrder::Chronological,
            hovered: None,
        }
    }

    /// Displays the change list into the given `Ui`.
    pub(crate) fn display<F>(&mut self, ui: &mut egui::Ui, mut filter: F)
    where
        F: FnMut(&str, &File) -> bool,
    {
        let mut time_dimensions = ui
            .painter()
            .layout_no_wrap(
                String::from("9999±999s"),
                TEXT_FONT,
                ui.style().noninteractive().text_color(),
            )
            .rect
            .size();
        time_dimensions.y = time_dimensions.y.max(CHANGE_SIZE.y);
        let time_dimensions = time_dimensions;

        let mut any_hovered = false;

        ui.horizontal(|ui| {
            ui.label(self.name);

            if ui
                .button(match self.order_type {
                    FileOrder::Alphabetical => "sort chronologically",
                    FileOrder::Chronological => "sort alphabetically",
                })
                .clicked()
            {
                self.order_type = match self.order_type {
                    FileOrder::Alphabetical => FileOrder::Chronological,
                    FileOrder::Chronological => FileOrder::Alphabetical,
                }
            }
        });

        egui::ScrollArea::both()
            .auto_shrink([false, true])
            .id_source(self.name)
            .show(ui, |ui| {
                let start_pos = ui.cursor().left_top();
                let mut current_pos = ui.cursor().left_top();

                for (y, (path, file)) in self.files.iter(self.order_type).enumerate() {
                    if !filter(path, file) {
                        continue;
                    }

                    self.draw_row(
                        ui,
                        &mut current_pos,
                        time_dimensions,
                        &mut any_hovered,
                        y,
                        (path, file),
                    );

                    current_pos.x = start_pos.x;
                    current_pos.y += time_dimensions.y;
                }
            });

        if !any_hovered {
            self.hovered = None;
        }
    }

    /// Draws a single row of the change list.
    fn draw_row(
        &mut self,
        ui: &mut egui::Ui,
        current_pos: &mut egui::Pos2,
        time_dimensions: egui::Vec2,
        any_hovered: &mut bool,
        y: usize,
        (path, file): (&str, &File),
    ) {
        let time_rect = egui::Rect::from_min_size(*current_pos, time_dimensions);
        current_pos.x += time_rect.width();

        let time_response = ui.allocate_rect(time_rect, egui::Sense::hover());
        if ui.is_rect_visible(time_rect) {
            let time = file
                .change_time
                .as_ref()
                .map(|time| time.avg.as_seconds_f64());

            let mut time_text_color = ui.style().noninteractive().text_color();

            if let (Some(last_time), Some(time)) = (self.last_time, time) {
                ui.painter().rect_filled(
                    time_rect,
                    0.0,
                    super::lerp_color(Color32::BLACK, Color32::BLUE, time / last_time),
                );
                time_text_color = Color32::WHITE;
            }

            ui.painter().text(
                time_rect.right_center(),
                egui::Align2::RIGHT_CENTER,
                if let Some(changetime) = &file.change_time {
                    std::borrow::Cow::Owned(format!("{}", changetime))
                } else {
                    std::borrow::Cow::Borrowed("????±???s")
                },
                TEXT_FONT,
                time_text_color,
            );
        }
        if time_response.hovered() && !*any_hovered {
            self.hovered = Some(ChangeListElement {
                row: y,
                element_type: ChangeListElementType::Time,
            });
            *any_hovered = true;
        }

        current_pos.x += ui.style().spacing.item_spacing.x;

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

            let change_response =
                self.draw_single_change(ui, current_pos, x, is_highlighted, change);
            if ui.rect_contains_pointer(change_response.rect) && !*any_hovered {
                self.hovered = Some(ChangeListElement {
                    row: y,
                    element_type: ChangeListElementType::Change { column: x },
                });
                *any_hovered = true;
            }
        }

        current_pos.x += ui.style().spacing.item_spacing.x;

        let path_rect = ui.painter().text(
            egui::pos2(current_pos.x, current_pos.y + time_dimensions.y / 2.0),
            egui::Align2::LEFT_CENTER,
            path,
            TEXT_FONT,
            ui.style().noninteractive().text_color(),
        );
        let path_response = ui.allocate_rect(path_rect, egui::Sense::hover()).hovered();
        if path_response && !*any_hovered {
            self.hovered = Some(ChangeListElement {
                row: y,
                element_type: ChangeListElementType::Path,
            });
            *any_hovered = true;

            if file.paths.len() > 1 {
                egui::show_tooltip_at_pointer(ui.ctx(), "hover_tooltip".into(), |ui| {
                    ui.label(file.paths.join("\n"))
                });
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
        let color_table = [Color32::from_gray(40), Color32::from_gray(45)];

        let bg = color_table[x % color_table.len()];
        let default_fg = ui.style().noninteractive().text_color();

        use crate::rules::ChangeKind;
        let (symb, fg, was_non_meta_change) = match ChangeKind::from(change) {
            ChangeKind::Unchanged => (" ", default_fg, false),
            ChangeKind::InodeModifiedTimestamp => ("m", default_fg, false),
            ChangeKind::AccessedTimestamp => ("A", default_fg, false),
            ChangeKind::ModifiedTimestamp => ("M", Color32::RED, false),
            ChangeKind::CreatedTimestamp => ("C", Color32::RED, false),
            ChangeKind::SizeChange => ("S", Color32::DARK_RED, true),
            ChangeKind::InodeChange => ("I", Color32::DARK_RED, true),
            ChangeKind::Complex => ("*", Color32::WHITE, true),
            ChangeKind::ContentModification => ("±", Color32::YELLOW, true),
            ChangeKind::SymlinkChange => ("L", Color32::YELLOW, true),
            ChangeKind::Deleted => ("-", Color32::RED, true),
            ChangeKind::Added => ("+", Color32::GREEN, true),
        };

        let bg = if is_highlighted {
            super::lerp_color(bg, HOVERED_HIGHLIGHT_COLOR, HOVERED_HIGHLIGHT_PERCENT)
        } else {
            bg
        };

        let rect = egui::Rect::from_min_size(*pos, CHANGE_SIZE);
        pos.x += rect.width();

        let response = ui.allocate_rect(rect, egui::Sense::hover());
        if ui.is_rect_visible(rect) {
            let painter = ui.painter().with_clip_rect(rect);

            if was_non_meta_change {
                painter.rect_filled(
                    rect,
                    0.0,
                    super::lerp_color(bg, NON_META_HIGHLIGHT_COLOR, NON_META_HIGHLIGHT_PERCENT),
                );
            } else {
                painter.rect_filled(rect, 0.0, bg);
            }
            painter.text(
                rect.center(),
                eframe::emath::Align2::CENTER_CENTER,
                symb,
                CHANGE_FONT,
                fg,
            );
        }

        if ui.rect_contains_pointer(response.rect) {
            if let Some(summary) = change_summary(change) {
                egui::show_tooltip_at_pointer(ui.ctx(), "hover_tooltip".into(), |ui| {
                    ui.label(summary)
                });
            }
        }

        response
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
                result.push_str(&format!("named stream change ({ty:?}): {val:?}"));
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
