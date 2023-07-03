//! Includes code for learning rules and matching them.

mod storage;

use inlinable_string::InlinableString;
use smallvec::SmallVec;
pub(crate) use storage::RuleStorage;

use crate::{
    file::{File, Files},
    path_matcher::PathMatcher,
};

/// The number of frequencies that are being reported.
const FREQUENCY_NUM: usize = 11;

/// A rule to describe one facet of the system behavior.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct Rule {
    /// What paths this rules applies to.
    path_matcher: PathMatcher,
    /// The frequencies of changes at the matches paths.
    frequencies: ChangeFrequencies,
    /// Tags that can classify the rule.
    ///
    /// Tags are just small pieces of text associated with a rule that can be used for a number of
    /// purposes.
    /// For example to explain where a rule originates (e.g. `win10`, `shutdown_button`).
    pub(crate) tags: SmallVec<[InlinableString; 2]>,
}

impl Rule {
    /// Creates a rule with the given path matcher.
    pub(crate) fn from_matcher(path_matcher: PathMatcher, files: &Files) -> Option<Rule> {
        let frequencies =
            ChangeFrequencies::compute(&path_matcher, files.chronological_order().files())?;

        Some(Rule {
            path_matcher,
            frequencies,
            tags: Default::default(),
        })
    }

    /// Returns the path matcher that this rule uses.
    pub(crate) fn path_matcher(&self) -> &PathMatcher {
        &self.path_matcher
    }

    /// The frequencies for the changes that this rule describes.
    pub(crate) fn frequencies(&self) -> [(&'static str, f64); FREQUENCY_NUM] {
        self.frequencies.as_array()
    }

    /// Tags a rule with the given tag.
    ///
    /// This has no effect if the rule is already tagged with the tag.
    pub(crate) fn tag(&mut self, tag: &str) {
        if !self.tags.iter().any(|t| *t == tag) {
            self.tags.push(tag.into());
        }
    }

    /// Returns a score between `0.0` and `1.0` for how much the rule matches the file.
    ///
    /// This will be `1.0` if the file behavior can be deterministically explained by the rule and
    /// `0.0` if it cannot be explained at all.
    pub(crate) fn match_score(&self, file: &File) -> f64 {
        if !self.path_matcher.matches_file(file) {
            return 0.0;
        }

        let Some(frequencies) =
            ChangeFrequencies::compute(&self.path_matcher, std::iter::once(file)) else { return 0.0 };

        // here we treat the frequencies as vectors in `[0.0, 1.0]^n` where `n` is the number of
        // frequencies recorded
        //
        // the match score is then the normalized magnitude of the difference between the two computed frequencies
        //
        // since we want close files to be close to `1.0`, we also compute `1.0` minus that number
        let difference_magnitude = self
            .frequencies
            .as_array()
            .into_iter()
            .zip(frequencies.as_array())
            .map(|((_, val1), (_, val2))| (val1 - val2).powi(2))
            .sum::<f64>()
            .sqrt();
        let max_difference_magnitude = (self.frequencies.as_array().len() as f64).sqrt();

        1.0 - (difference_magnitude / max_difference_magnitude)
    }
}

/// Contains all frequencies of changes at the given paths.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct ChangeFrequencies {
    /// The frequency with which files at the given paths are added.
    added: f64,
    /// The frequency with which files at the given paths are deleted.
    deleted: f64,
    /// The frequency with which the file content is changed at the given paths.
    content_changed: f64,
    /// The frequency with which symlinks are changed at the given paths.
    symlink_changed: f64,
    /// The frequency with which the file inode is changed.
    inode_changed: f64,
    /// The frequency with which the file size increased.
    size_increase: f64,
    /// The frequency with which the file size decreased.
    size_decrease: f64,
    /// The frequency with which the inode modification timestamp of the file is changed.
    inode_timestamp: f64,
    /// The frequency with which the created timestamp of the file is changed.
    created_timestamp: f64,
    /// The frequency with which the modified timestamp of the file is changed.
    modified_timestamp: f64,
    /// The frequency with which the accessed timestamp of the file is changed.
    accessed_timestamp: f64,
}

impl std::hash::Hash for ChangeFrequencies {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_array().map(|(_, val)| val as u64).hash(state);
    }
}

impl ChangeFrequencies {
    /// Compute the change frequencies for the given files.
    ///
    /// Gives up on complex changes that are not easily represented by the model.
    fn compute<'a>(
        path_matcher: &'a PathMatcher,
        files: impl Iterator<Item = &'a File>,
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

        for file in files {
            if !path_matcher.matches_file(file) {
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
    pub(crate) fn as_array(&self) -> [(&'static str, f64); FREQUENCY_NUM] {
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
    }
}
