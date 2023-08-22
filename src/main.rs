#![feature(let_chains)]

use std::io::Write as _;

mod change_distribution;
mod change_event;
mod file;
mod fs_tree;
mod future_value;
mod gui;
mod input;
mod path_matcher;
mod rules;

/// allows learning normal system behavior and detecting anomalies
#[derive(Debug, structopt::StructOpt)]
enum Config {
    /// runs the main GUI of bark
    Run {
        input: std::path::PathBuf,
        #[structopt(short, long)]
        rules: Option<std::path::PathBuf>,
        #[structopt(short, long)]
        path_list: Vec<std::path::PathBuf>,
    },
    /// adds all sources in a directory to a rulefile
    AddSourcesToRules {
        sources: std::path::PathBuf,
        rules: std::path::PathBuf,
    },
}

fn main() -> anyhow::Result<()> {
    let config = <Config as structopt::StructOpt>::from_args();

    match config {
        Config::Run {
            input,
            rules: rules_path,
            path_list,
        } => {
            let input = input::read(input)?;
            let files = file::Files::from_changesets(&input);
            let mut path_lists = std::collections::BTreeMap::new();

            for path_list in path_list {
                let mut input_paths = Vec::new();
                if path_list.is_file() {
                    input_paths.push(path_list);
                } else if path_list.is_dir() {
                    if let Ok(iter) = std::fs::read_dir(path_list) {
                        for path in iter.flatten() {
                            input_paths.push(path.path());
                        }
                    }
                }

                for path in input_paths {
                    if let Some(name) = path.file_name() {
                        let name = name.to_string_lossy().into_owned();

                        match std::fs::read_to_string(&path) {
                            Ok(content) => {
                                let list = input::PathList::new(
                                    content.lines().map(Box::<str>::from).collect::<Vec<_>>(),
                                );
                                path_lists.insert(name, list);
                            }
                            Err(err) => eprintln!("error reading {}: {err}", path.display()),
                        }
                    }
                }
            }

            let rules = if let Some(rules) = &rules_path {
                match rules::RuleStorage::load(rules) {
                    Ok(storage) => storage,
                    Err(err) => {
                        eprintln!("{err}");
                        eprintln!("Using empty rule storage instead");
                        rules::RuleStorage::new()
                    }
                }
            } else {
                rules::RuleStorage::new()
            };

            gui::GuiApp::run(rules, files, rules_path, path_lists).unwrap();
        }
        Config::AddSourcesToRules {
            sources,
            rules: rules_path,
        } => {
            let mut source_paths = Vec::new();
            if sources.is_dir() {
                if let Ok(iter) = std::fs::read_dir(sources) {
                    for path in iter.flatten() {
                        source_paths.push(path.path());
                    }
                }
            } else {
                source_paths.push(sources);
            }

            println!(
                "Adding {} sources to {}",
                source_paths.len(),
                rules_path.display()
            );

            let mut rules = rules::RuleStorage::load(&rules_path)?;

            for source in source_paths {
                print!("{:.<70}...", source.display());
                std::io::stdout().flush()?;

                let input = match input::read(source) {
                    Ok(val) => val,
                    Err(err) => {
                        println!("ERROR");
                        println!("{err}");
                        continue;
                    }
                };
                let files = file::Files::from_changesets(&input);

                let mut updated = false;
                for rule in rules.iter_mut() {
                    updated |= rule.add_data_source(&files);
                }

                if updated {
                    println!("UPDATED");
                } else {
                    println!("UP TO DATE");
                }
            }

            rules.save(rules_path);
            print!("{:.<70}...", "Waiting for changes to be written to disk");
            std::io::stdout().flush()?;
            std::thread::sleep(std::time::Duration::from_secs(1));
            println!("DONE");
        }
    }

    Ok(())
}
