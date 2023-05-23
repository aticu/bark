//! Includes code for learning rules and matching them.

mod storage;

pub(crate) use storage::RuleStorage;

use crate::{
    file::{File, Files},
    glob::glob_matches,
};

/// A rule to describe one facet of the system behavior.
#[derive(Debug)]
pub(crate) struct Rule {
    /// What paths this rules applies to.
    glob: String,
    /// The frequencies of changes at the matches paths.
    pub(crate) frequencies: ChangeFrequencies,
}

impl Rule {
    /// Creates a rule with the given glob.
    pub(crate) fn from_glob(glob: &str, files: &Files) -> Option<Rule> {
        let frequencies = ChangeFrequencies::compute(glob, files.chronological_order())?;

        Some(Rule {
            glob: glob.to_string(),
            frequencies,
        })
    }

    /// Returns a score between `0.0` and `1.0` for how much the rule matches the file.
    ///
    /// This will be `1.0` if the file behavior can be deterministically explained by the rule and
    /// `0.0` if it cannot be explained at all.
    pub(crate) fn match_score(&self, file: &File) -> f64 {
        let Some(matching_path) = file.paths.iter().find(|path| glob_matches(&self.glob, path)) else { return 0.0 };

        let Some(frequencies) =
            ChangeFrequencies::compute(&self.glob, std::iter::once((matching_path.as_str(), file))) else { return 0.0 };

        // here we treat the frequencies as vectors in `[0.0, 1.0]^n` where `n` is the number of
        // frequencies recorded
        //
        // then we compute the cosine angle between these vectors
        // because of how the vector space is set up, this value will always be between 0 and 1
        ChangeFrequencies::dot_product(&self.frequencies, &frequencies)
            / (self.frequencies.magnitude() * frequencies.magnitude())
    }
}

/// Contains all frequencies of changes at the given paths.
#[derive(Debug)]
pub(crate) struct ChangeFrequencies {
    /// The frequency with which files at the given paths are added.
    pub(crate) added: f64,
    /// The frequency with which files at the given paths are deleted.
    pub(crate) deleted: f64,
    /// The frequency with which the file content is changed at the given paths.
    pub(crate) content_changed: f64,
    /// The frequency with which symlinks are changed at the given paths.
    pub(crate) symlink_changed: f64,
    /// The frequency with which the file inode is changed.
    pub(crate) inode_changed: f64,
    /// The frequency with which the file size increased.
    pub(crate) size_increase: f64,
    /// The frequency with which the file size decreased.
    pub(crate) size_decrease: f64,
    /// The frequency with which the inode modification timestamp of the file is changed.
    pub(crate) inode_timestamp: f64,
    /// The frequency with which the created timestamp of the file is changed.
    pub(crate) created_timestamp: f64,
    /// The frequency with which the modified timestamp of the file is changed.
    pub(crate) modified_timestamp: f64,
    /// The frequency with which the accessed timestamp of the file is changed.
    pub(crate) accessed_timestamp: f64,
}

impl ChangeFrequencies {
    /// Compute the change frequencies for the given files.
    ///
    /// Gives up on complex changes that are not easily represented by the model.
    fn compute<'a>(
        glob: &'a str,
        files: impl Iterator<Item = (&'a str, &'a File)>,
    ) -> Option<ChangeFrequencies> {
        let mut added = 0;
        let mut deleted = 0;
        let mut content_changed = 0;
        let mut symlink_changed = 0;
        let mut inode_changed = 0;
        let mut size_increase = 0;
        let mut size_decrease = 0;
        let mut inode_timestamp = 0;
        let mut created_timestamp = 0;
        let mut modified_timestamp = 0;
        let mut accessed_timestamp = 0;
        let mut total = 0;

        for (path, file) in files {
            if !glob_matches(glob, path) {
                continue;
            }

            use sniff::{EntryDiff::*, MetaEntryDiff::*};

            // to better handle deletions and additions, we track when the file existed
            // if there are no additions or deletions, the file must have existed the whole time
            let mut exists = if let Some(c) = file
                .changes
                .iter()
                .find(|c| matches!(c, Some(Added(_)) | Some(Deleted(_))))
            {
                matches!(c, Some(Deleted(_)))
            } else {
                true
            };

            for change in &file.changes {
                // we only count this change if the file exists or there's actually a change (which
                // would likely be an addition if the file didn't exist)
                if exists || change.is_some() {
                    total += 1;
                }

                let Some(change) = change else { continue };
                let meta = change.meta_info();

                match change {
                    Added(_) => {
                        exists = true;
                        added += 1;
                    }
                    Deleted(_) => {
                        exists = false;
                        deleted += 1;
                    }
                    MetaOnlyChange(_) => (),
                    EntryChange(change, _) => match change {
                        FileChanged { .. } => content_changed += 1,
                        SymlinkChanged { .. } => symlink_changed += 1,
                        TypeChange(_) => return None,
                        OtherChange => return None,
                    },
                }

                if meta.inode.is_changed() {
                    inode_changed += 1;
                }
                match &meta.changes[..] {
                    [sniff::MetadataChange::Size(change)] => match change.cmp() {
                        std::cmp::Ordering::Less => size_increase += 1,
                        std::cmp::Ordering::Equal => (),
                        std::cmp::Ordering::Greater => size_decrease += 1,
                    },
                    [] => (),
                    _ => return None,
                }
                if meta.inode_modified.is_changed() {
                    inode_timestamp += 1;
                }
                if meta.created.is_changed() {
                    created_timestamp += 1;
                }
                if meta.modified.is_changed() {
                    modified_timestamp += 1;
                }
                if meta.accessed.is_changed() {
                    accessed_timestamp += 1;
                }
            }
        }

        if total == 0 {
            return None;
        }

        macro_rules! return_val {
            ($total:ident; $($name:ident)*) => {
                Some(ChangeFrequencies {
                    $(
                        $name: ($name as f64 / $total as f64).clamp(0.0, 1.0),
                    )*
                })
            }
        }
        return_val! {
            total;
            added
            deleted
            content_changed
            symlink_changed
            inode_changed
            size_increase
            size_decrease
            inode_timestamp
            created_timestamp
            modified_timestamp
            accessed_timestamp
        }
    }

    /// An iterator over all frequencies.
    pub(crate) fn iter(&self) -> impl Iterator<Item = (&'static str, f64)> {
        let Self {
            added,
            deleted,
            content_changed,
            symlink_changed,
            inode_changed,
            size_increase,
            size_decrease,
            inode_timestamp,
            created_timestamp,
            modified_timestamp,
            accessed_timestamp,
        } = self;
        [
            ("added", *added),
            ("deleted", *deleted),
            ("content changed", *content_changed),
            ("symlink changed", *symlink_changed),
            ("inode changed", *inode_changed),
            ("size increase", *size_increase),
            ("size decrease", *size_decrease),
            ("inode timestamp", *inode_timestamp),
            ("created timestamp", *created_timestamp),
            ("modified timestamp", *modified_timestamp),
            ("accessed timestamp", *accessed_timestamp),
        ]
        .into_iter()
    }

    /// The magnitude of the change vector.
    fn magnitude(&self) -> f64 {
        self.iter().map(|(_, f)| f * f).sum::<f64>().sqrt()
    }

    /// Computes the dot product between the two given change frequencies.
    fn dot_product(frequencies1: &ChangeFrequencies, frequencies2: &ChangeFrequencies) -> f64 {
        frequencies1
            .iter()
            .zip(frequencies2.iter())
            .map(|((_, f1), (_, f2))| f1 * f2)
            .sum()
    }
}

/*
/// Learns simple rules that only concern a single path.
pub(crate) fn learn_basic_rules<Timestamp>(
    rules: &mut Vec<Rule>,
    all_paths: &BTreeSet<&str>,
    input: &[Changeset<Timestamp>],
) {
    for path in all_paths {
        let mut change_kinds = std::collections::BTreeMap::new();
        for changeset in input {
            let change_kind = if let Some(change) = changeset.changes.get(*path) {
                ChangeKind::from(change)
            } else {
                continue;
            };

            *change_kinds.entry(change_kind).or_insert(0) += 1;
        }

        let rule = 'rule: {
            if let Some((first_kind, first_count)) = change_kinds.pop_last() {
                if first_kind == ChangeKind::Complex {
                    // we shouldn't create rules for complex changes
                    break 'rule None;
                }

                if let Some((second_kind, second_count)) = change_kinds.pop_last() {
                    if !change_kinds.is_empty() {
                        break 'rule None;
                    }
                    if second_kind == ChangeKind::Complex {
                        // we shouldn't create rules for complex changes
                        break 'rule None;
                    }

                    let frequency = (first_count + second_count) as f64 / input.len() as f64;
                    if frequency < FREQUENCY_TRESHHOLD {
                        break 'rule None;
                    }
                    break 'rule Some(Rule {
                        frequency,
                        matcher: PathMatcher::Glob(crate::path::glob_escape(path)),
                        kind: RuleKind::TwoChangeKinds(first_kind, second_kind),
                    });
                } else {
                    let frequency = first_count as f64 / input.len() as f64;
                    if frequency < FREQUENCY_TRESHHOLD {
                        break 'rule None;
                    }

                    break 'rule Some(Rule {
                        frequency,
                        matcher: PathMatcher::Glob(crate::path::glob_escape(path)),
                        kind: RuleKind::SingleChange(first_kind),
                    });
                }
            }

            None
        };

        if let Some(rule) = rule {
            rules.push(rule);
        }
    }
}
*/
