use std::{collections::BTreeSet, fmt};

use sniff::Changeset;

mod file;
mod input;
mod path;
mod rules;

/// Information about timestamp changes for a single file.
#[derive(Debug)]
struct ChangeTime {
    /// The average timestamp since the boot.
    avg: time::Duration,
    /// The standard deviation of the change time since boot, if calculation requested.
    std_dev: Option<time::Duration>,
}

impl PartialEq for ChangeTime {
    fn eq(&self, other: &Self) -> bool {
        self.avg.eq(&other.avg)
    }
}

impl Eq for ChangeTime {}

impl PartialOrd for ChangeTime {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.avg.partial_cmp(&other.avg)
    }
}

impl Ord for ChangeTime {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.avg.cmp(&other.avg)
    }
}

impl fmt::Display for ChangeTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let avg = self.avg.as_seconds_f64();
        if avg >= 10000.0 {
            write!(f, "long")?;
        } else if avg >= 100.0 {
            write!(f, "{:4.0}", avg)?;
        } else if avg >= 10.0 {
            write!(f, "{:4.1}", avg)?;
        } else {
            write!(f, "{:4.2}", avg)?;
        }

        if let Some(std_dev) = self.std_dev {
            write!(f, "±")?;

            let std_dev = std_dev.as_seconds_f64();
            if std_dev >= 1000.0 {
                write!(f, "BIG",)?;
            } else if std_dev >= 10.0 {
                write!(f, "{:3.0}", std_dev)?;
            } else {
                write!(f, "{:3.1}", std_dev)?;
            }
        }

        write!(f, "s")
    }
}

/// Computes the average time for a given path in the input.
fn change_time(
    path: &str,
    input: &[Changeset<time::Duration>],
    compute_std_dev: bool,
) -> Option<ChangeTime> {
    let mut total = time::Duration::ZERO;
    let mut n = 0;

    let max_ts = |change: &sniff::MetaEntryDiff<time::Duration>| {
        let meta = change.meta_info();
        [
            meta.created.new_val(),
            meta.modified.new_val(),
            meta.accessed.new_val(),
            meta.inode_modified.new_val(),
        ]
        .into_iter()
        .filter_map(|ts| *ts)
        .filter(|ts| ts.is_positive())
        .max()
    };

    for changeset in input {
        let Some(change) = changeset.changes.get(path) else { continue };
        let Some(ts) = max_ts(change) else { continue };

        total += ts;
        n += 1;
    }

    if n == 0 {
        return None;
    }
    let avg = total / n;

    if !compute_std_dev {
        return Some(ChangeTime { avg, std_dev: None });
    }

    let mut std_dev_total = 0.0;

    for changeset in input {
        let Some(change) = changeset.changes.get(path) else { continue };
        let Some(ts) = max_ts(change) else { continue };
        let ts_diff: time::Duration = avg - ts;

        std_dev_total += ts_diff.as_seconds_f64().powi(2);
    }

    let std_dev = Some(time::Duration::seconds_f64(
        (std_dev_total / n as f64).sqrt(),
    ));

    Some(ChangeTime { avg, std_dev })
}

#[derive(Debug, structopt::StructOpt)]
struct Config {
    input: std::path::PathBuf,
    #[structopt(short, long)]
    rules: Vec<std::path::PathBuf>,
    #[structopt(short, long)]
    time_sort: bool,
}

fn main() -> anyhow::Result<()> {
    let config = <Config as structopt::StructOpt>::from_args();
    let input = input::read_many(&config.input)?;
    let input: Vec<_> = input
        .iter()
        .map(|changeset| input::transform_to_relative_time(changeset).unwrap().1)
        .collect();

    let mut all_paths: Vec<&str> = input
        .iter()
        .flat_map(|changeset| changeset.changes.keys())
        .map(|path| path.as_str())
        // de-duplicate
        .collect::<BTreeSet<&str>>()
        .into_iter()
        .collect();

    if config.time_sort {
        all_paths.sort_by_cached_key(|&path| change_time(path, &input, false));
    } else {
        all_paths.sort_unstable();
    }

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

    //rules::learn_basic_rules(&mut rules, &all_paths, &input);
    //rules::learn_glob_rules(&mut rules, &all_paths, &input);

    let mut path_changes = std::collections::BTreeMap::new();

    for (i, changeset) in input.iter().enumerate() {
        for (path, change) in &changeset.changes {
            path_changes
                .entry(path.as_str())
                .or_insert_with(|| vec![rules::ChangeKind::Unchanged; input.len()])[i] =
                rules::ChangeKind::from(change);
        }
    }

    let mut applications = 0;

    for path in &all_paths {
        let changes = &path_changes[path];

        let mut cumulative_match_frequency = 0.0;
        for rule in rules.rules_for(path) {
            cumulative_match_frequency += rule.match_frequency_in(path, &changes[..]);
        }
        let unchanged_frequency = changes
            .iter()
            .filter(|&&kind| kind == rules::ChangeKind::Unchanged)
            .count() as f64
            / changes.len() as f64;

        let entry_matches = unchanged_frequency + cumulative_match_frequency >= 0.999;

        if entry_matches {
            applications += 1;
            continue;
        }

        if config.time_sort {
            if let Some(changetime) = change_time(path, &input, true) {
                print!("[{}]", changetime);
            } else {
                print!("[????±???s]");
            }
        }

        for (i, change) in changes.iter().enumerate() {
            use owo_colors::{AnsiColors::*, OwoColorize as _};

            let color_table = [Blue, Cyan];

            let default_bg = color_table[i % color_table.len()];
            let default_fg = Black;

            let (symb, fg, bg) = match change {
                rules::ChangeKind::Unchanged => (' ', default_fg, default_bg),
                rules::ChangeKind::InodeModifiedTimestamp => ('m', default_fg, default_bg),
                rules::ChangeKind::AccessedTimestamp => ('A', default_fg, default_bg),
                rules::ChangeKind::ModifiedTimestamp => ('M', Red, default_bg),
                rules::ChangeKind::CreatedTimestamp => ('C', Red, default_bg),
                rules::ChangeKind::SizeChange => ('S', Magenta, default_bg),
                rules::ChangeKind::InodeChange => ('I', Magenta, default_bg),
                rules::ChangeKind::Complex => ('*', White, Black),
                rules::ChangeKind::ContentModified => ('±', Black, Yellow),
                rules::ChangeKind::Deleted => ('-', White, Red),
                rules::ChangeKind::Added => ('+', Black, Green),
            };

            print!("{}", symb.color(fg).on_color(bg));
        }
        println!(" {}", path);
    }

    println!("loaded {} rules", rules.len());
    println!("found {} paths", path_changes.len());
    println!("filtered {} paths", applications);

    Ok(())
}

// - Change/diff
// - struct File containing paths (joined by inodes), original change lists, average change time
// - type change: handle differently than ContentModified
// - hovering over elements gives details
// - how many entries down is the first where all are after (same for up, before)
// - TODO
