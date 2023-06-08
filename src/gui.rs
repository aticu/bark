mod change_list;
mod rule_writing;
mod rules;

use eframe::{egui, epaint::Color32};

use crate::{file::Files, rules::RuleStorage};

pub(crate) struct GuiApp {
    pub(crate) rules: RuleStorage,
    rule_writer: rule_writing::RuleWriter,
    change_list: change_list::ChangeList,
    rule_mode: bool,
}

impl GuiApp {
    pub(crate) fn run(
        rules: RuleStorage,
        files: Files,
        rule_file: Option<std::path::PathBuf>,
        path_lists: std::collections::BTreeMap<String, crate::input::PathList>,
    ) -> Result<(), eframe::Error> {
        let files = Box::leak(Box::new(files));
        eframe::run_native(
            "bark - behavior anomaly reconnaissance kit",
            eframe::NativeOptions::default(),
            Box::new(|_| {
                let rule_writer =
                    rule_writing::RuleWriter::new(files, &rules, rule_file, path_lists);
                Box::new(GuiApp {
                    rules,
                    rule_writer,
                    change_list: change_list::ChangeList::new(files, "Filtered changes", true),
                    rule_mode: true,
                })
            }),
        )
    }
}

impl eframe::App for GuiApp {
    fn update(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.checkbox(&mut self.rule_mode, "Write a rule");

            if self.rule_mode {
                self.rule_writer.display(ui, &mut self.rules);
            } else {
                self.change_list.display(ui, &self.rules);
            }
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
