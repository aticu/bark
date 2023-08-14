//! Deals with graphical display and user interaction.

mod change_distribution;
mod change_event;
mod change_list;
mod rule_writing;
mod rules;
mod utils;

use std::time::Duration;

use eframe::egui;

use crate::{file::Files, rules::RuleStorage};

pub(crate) use utils::lerp_color;

/// The bark GUI app state.
pub(crate) struct GuiApp {
    /// The rule writing GUI.
    rule_writer: rule_writing::RuleWriter,
    /// The change list GUI.
    change_list: change_list::ChangeList,
    /// The rule list GUI.
    rule_list: rules::RuleList,
    /// The rules used by the GUI app.
    rules: RuleStorage,
    /// The current GUI tab.
    current_tab: Tab,
    /// The time the last frame took to compute.
    last_frame_time: Option<Duration>,
}

/// The possible tabs in bark.
#[derive(Debug, PartialEq, Eq)]
enum Tab {
    /// The tab where rules are written.
    RuleWriting,
    /// The tab where the filtered changes are listed.
    ChangeList,
    /// The tab where the filtered changes are listed.
    RuleList,
}

impl GuiApp {
    /// Runs the GUI application.
    pub(crate) fn run(
        rules: RuleStorage,
        files: Files,
        rule_file: Option<std::path::PathBuf>,
        path_lists: std::collections::BTreeMap<String, crate::input::PathList>,
    ) -> Result<(), eframe::Error> {
        let files = Box::leak(Box::new(files));
        let path_lists = Box::leak(Box::new(path_lists));
        eframe::run_native(
            "bark - behavior anomaly reconnaissance kit",
            eframe::NativeOptions::default(),
            Box::new(|ctx| {
                Box::new(GuiApp {
                    rule_writer: rule_writing::RuleWriter::new(
                        files,
                        &rules,
                        rule_file.clone(),
                        path_lists,
                        ctx.egui_ctx.clone(),
                    ),
                    change_list: change_list::ChangeList::new(files, "Filtered changes", false),
                    rule_list: rules::RuleList::new(files, rule_file),
                    rules,
                    current_tab: Tab::ChangeList,
                    last_frame_time: None,
                })
            }),
        )
    }
}

impl eframe::App for GuiApp {
    fn update(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        let start = std::time::Instant::now();

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.horizontal(|ui| {
                for (value, text) in [
                    (Tab::RuleWriting, "Rule writing"),
                    (Tab::ChangeList, "Change list"),
                    (Tab::RuleList, "Rule list"),
                ] {
                    ui.selectable_value(&mut self.current_tab, value, text);
                }

                if let Some(frame_time) = self.last_frame_time {
                    ui.painter().text(
                        ui.cursor().min + egui::vec2(ui.available_width(), 0.0),
                        egui::Align2::RIGHT_TOP,
                        format!("{:.0} FPS", 1.0 / frame_time.as_secs_f64()),
                        eframe::epaint::FontId {
                            size: 10.0,
                            family: egui::FontFamily::Proportional,
                        },
                        ui.style().noninteractive().text_color(),
                    );
                }
            });

            match self.current_tab {
                Tab::RuleWriting => self.rule_writer.display(ui, &mut self.rules),
                Tab::ChangeList => self.change_list.display(ui, &self.rules),
                Tab::RuleList => self.rule_list.display(ui, &mut self.rules),
            }
        });

        let frame_time = start.elapsed();
        self.last_frame_time = Some(frame_time);
        if frame_time > Duration::from_millis(50) {
            eprintln!("Unusually long frame time: {frame_time:?}");
        }
    }
}
