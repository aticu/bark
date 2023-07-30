//! Includes code for learning rules and matching them.

mod storage;

use inlinable_string::InlinableString;
use smallvec::SmallVec;
pub(crate) use storage::{MatchCountCache, RuleStorage};

use crate::{
    change_event::ChangeDistribution,
    file::{File, Files},
    path_matcher::PathMatcher,
};

/// A rule to describe one facet of the system behavior.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct Rule {
    /// What paths this rules applies to.
    path_matcher: PathMatcher,
    /// The observed change events for this file.
    pub(crate) change_events: ChangeDistribution,
    /// The sources that are already part of the change events for this rule.
    file_sources: SmallVec<[u64; 4]>,
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
        let change_events = ChangeDistribution::from_files(
            files
                .chronological_order()
                .files()
                .filter(|file| path_matcher.matches_file(file)),
        )?;
        let file_sources = smallvec::smallvec![files.datasource_id()];

        Some(Rule {
            path_matcher,
            change_events,
            file_sources,
            tags: Default::default(),
        })
    }

    /// Adds the data from the given source to the rule
    pub(crate) fn add_data_source(&mut self, files: &Files) {
        let datasource_id = files.datasource_id();

        if self.file_sources.contains(&datasource_id) {
            return;
        }

        let Some(change_events) = ChangeDistribution::from_files(
            files
                .chronological_order()
                .files()
                .filter(|file| self.path_matcher.matches_file(file)),
        ) else { return };

        if !change_events.changes.is_empty() {
            for (change_event, count) in &change_events.changes {
                *self.change_events.changes.entry(*change_event).or_default() += count;
            }
            self.file_sources.push(datasource_id);
        }
    }

    /// Returns the path matcher that this rule uses.
    pub(crate) fn path_matcher(&self) -> &PathMatcher {
        &self.path_matcher
    }

    /// Tags the rule with the given tag.
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

        let Some(change_events) = ChangeDistribution::from_files(std::iter::once(file)) else { return 0.0 };

        self.change_events
            .same_distribution_probabilitity(change_events)
    }
}
