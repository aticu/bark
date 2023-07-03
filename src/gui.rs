mod change_list;
mod rule_writing;
mod rules;
mod utils;

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
        eframe::run_native(
            "bark - behavior anomaly reconnaissance kit",
            eframe::NativeOptions::default(),
            Box::new(|_| {
                Box::new(GuiApp {
                    rule_writer: rule_writing::RuleWriter::new(
                        files,
                        &rules,
                        rule_file.clone(),
                        path_lists,
                    ),
                    change_list: change_list::ChangeList::new(files, "Filtered changes", true),
                    rule_list: rules::RuleList::new(files, rule_file),
                    rules,
                    current_tab: Tab::ChangeList,
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
            });

            match self.current_tab {
                Tab::RuleWriting => self.rule_writer.display(ui, &mut self.rules),
                Tab::ChangeList => self.change_list.display(ui, &self.rules),
                Tab::RuleList => self.rule_list.display(ui, &mut self.rules),
            }
        });
        println!("frame time: {:?}", start.elapsed());
    }
}
