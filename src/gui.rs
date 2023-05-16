mod change_list;

use eframe::{egui, epaint::Color32};

use crate::{file::Files, rules::RuleStorage};

pub(crate) struct GuiApp {
    pub(crate) rules: RuleStorage,
    pub(crate) files: &'static Files,
    change_list: change_list::ChangeList,
}

impl GuiApp {
    pub(crate) fn run(rules: RuleStorage, files: Files) -> Result<(), eframe::Error> {
        let files = Box::leak(Box::new(files));
        eframe::run_native(
            "TODO: name",
            eframe::NativeOptions::default(),
            Box::new(|_| {
                Box::new(GuiApp {
                    rules,
                    files,
                    change_list: change_list::ChangeList::new(files, "Filtered changes"),
                })
            }),
        )
    }
}

impl eframe::App for GuiApp {
    fn update(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        /*
        if let Some((x, y)) = self.highlighted {
            egui::TopBottomPanel::bottom("root_bottom_panel").show(ctx, |ui| {
                let (path, file) = match self.sort_type {
                    FileOrder::Alphabetical => self.files.alphabetical_order().nth(y),
                    FileOrder::Chronological => self.files.chronological_order().nth(y),
                }
                .unwrap();
                if let Some(change) = file
                    .changes
                    .get(x)
                    .and_then(|maybe_change| maybe_change.as_ref())
                {
                    ui.label(format!("{path}: {:#?}", change));
                } else {
                    for path in &file.paths {
                        ui.label(path);
                    }
                }
            });
        }
        */

        egui::CentralPanel::default().show(ctx, |ui| {
            self.change_list.display(ui, |_path, file| {
                self.rules
                    .matching_rules(file, f64::EPSILON)
                    .next()
                    .is_none()
            });
        });
    }
}

/// Linearly interpolates between the two given colors.
///
/// If `between` is `0.0`, the result will be `color1` and if it is `1.0`, the result will be
/// `color2`.
fn lerp_color(color1: Color32, color2: Color32, mut between: f64) -> Color32 {
    between = if between.is_nan() {
        0.5
    } else {
        between.clamp(0.0, 1.0)
    };

    let transform = |val1, val2| (val1 as f64 * (1.0 - between) + val2 as f64 * between) as u8;

    Color32::from_rgba_unmultiplied(
        transform(color1.r(), color2.r()),
        transform(color1.g(), color2.g()),
        transform(color1.b(), color2.b()),
        transform(color1.a(), color2.a()),
    )
}
