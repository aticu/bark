#![feature(let_chains)]

mod file;
mod gui;
mod input;
mod path;
mod rules;

#[derive(Debug, structopt::StructOpt)]
struct Config {
    input: std::path::PathBuf,
    #[structopt(short, long)]
    rules: Vec<std::path::PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let config = <Config as structopt::StructOpt>::from_args();

    let input = input::read_many(&config.input)?;
    let files = file::Files::from_changesets(&input);

    let mut rules = rules::RuleStorage::new();

    for rule_file in &config.rules {
        let content = std::fs::read_to_string(rule_file)?;
        for line in content.lines() {
            if line.trim_start().starts_with('#') {
                continue;
            }
            let Ok(rule) = line.trim().parse() else { continue };
            rules.insert(rule);
        }
    }

    gui::GuiApp::run(rules, files).unwrap();

    Ok(())
}
