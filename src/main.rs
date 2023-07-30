#![feature(let_chains)]

mod change_event;
mod file;
mod fs_tree;
mod future_value;
mod gui;
mod input;
mod path_matcher;
mod rules;

#[derive(Debug, structopt::StructOpt)]
struct Config {
    input: std::path::PathBuf,
    #[structopt(short, long)]
    rules: Option<std::path::PathBuf>,
    #[structopt(short, long)]
    path_list: Vec<std::path::PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let config = <Config as structopt::StructOpt>::from_args();

    let input = input::read(config.input)?;
    let files = file::Files::from_changesets(&input);
    let mut path_lists = std::collections::BTreeMap::new();

    for path_list in config.path_list {
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
                            content.lines().map(String::from).collect::<Vec<_>>(),
                        );
                        path_lists.insert(name, list);
                    }
                    Err(err) => eprintln!("error reading {}: {err}", path.display()),
                }
            }
        }
    }

    let rules = if let Some(rules) = &config.rules {
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

    gui::GuiApp::run(rules, files, config.rules, path_lists).unwrap();

    Ok(())
}
