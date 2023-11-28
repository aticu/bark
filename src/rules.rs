//! Includes code for learning rules and matching them.

mod storage;

use std::collections::BTreeSet;

use smallvec::SmallVec;
pub(crate) use storage::{MatchCountCache, RuleStorage};

use crate::{
    file::{DatasourceId, File, Files},
    fs_change_distribution::{self, FsChangeDistribution},
    fs_changes::FsChangeCounts,
    path_matcher::PathMatcher,
    provenance::Tracked,
};

/// A rule to describe one facet of the system behavior.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct Rule {
    /// What paths this rules applies to.
    path_matcher: PathMatcher,
    /// The observed data for this rule.
    observations: SmallVec<[Tracked<FsChangeCounts>; 1]>,
    /// The combined distributions for this rule.
    ///
    /// Since this is only dependent on the other data, it is not stored on disk and recomputed
    /// each time.
    #[serde(skip)]
    distributions: SmallVec<[Tracked<FsChangeDistribution>; 1]>,
}

impl Rule {
    /// Creates a rule with the given path matcher.
    pub(crate) fn from_matcher(path_matcher: PathMatcher, files: &Files) -> Option<Rule> {
        let mut this = Rule {
            path_matcher,
            observations: SmallVec::new(),
            distributions: SmallVec::new(),
        };

        this.add_data_source(files);

        if this.observations.is_empty() {
            None
        } else {
            Some(this)
        }
    }

    /// Adds the data from the given source to the rule.
    ///
    /// Returns whether anything about the data changed.
    pub(crate) fn add_data_source(&mut self, files: &Files) -> bool {
        let datasource_id = files.datasource_id();

        if self
            .observations
            .iter()
            .any(|dist| dist.is_from_source(datasource_id))
        {
            return false;
        }

        let mut updated = false;

        for file in files
            .chronological_order()
            .files()
            .filter(|file| self.path_matcher.matches_file(file))
        {
            if let Some(distribution) = FsChangeCounts::from_file(file) {
                updated = true;
                if let Some(dist) = self
                    .observations
                    .iter_mut()
                    .find(|dist| ***dist == distribution)
                {
                    dist.add_source(datasource_id);
                } else {
                    self.observations
                        .push(Tracked::from_single_source(distribution, datasource_id));
                }
            }
        }

        self.ensure_consistency();

        updated
    }

    /// Ensures that that the internal state of the rule is consistent.
    pub(crate) fn ensure_consistency(&mut self) {
        self.compute_distributions()
    }

    /// Returns the path matcher that this rule uses.
    pub(crate) fn path_matcher(&self) -> &PathMatcher {
        &self.path_matcher
    }

    /// Returns an iterator over the sources that this rule is derived from.
    pub(crate) fn sources(&self) -> impl Iterator<Item = DatasourceId> {
        self.observations
            .iter()
            .flat_map(|observation| observation.provenance.sources.keys())
            .copied()
            .collect::<BTreeSet<_>>()
            .into_iter()
    }

    /// Returns the distributions of this rule.
    pub(crate) fn distributions(&self) -> &[Tracked<FsChangeDistribution>] {
        &self.distributions[..]
    }

    /// The number of original samples.
    pub(crate) fn sample_count(&self) -> u32 {
        self.observations
            .iter()
            .map(|dist| dist.occurrence_count())
            .sum()
    }

    /// Computes the distributions for this rule.
    fn compute_distributions(&mut self) {
        self.distributions =
            fs_change_distribution::merge_counts_into_distributions(&self.observations);
    }

    /// Returns a score between `0.0` and `1.0` for how much the rule matches the file.
    ///
    /// This will be `1.0` if the file behavior can be deterministically explained by the rule and
    /// `0.0` if it cannot be explained at all.
    pub(crate) fn match_score(&self, file: &File) -> f64 {
        self.best_matching_distribution(file)
            .map(|(_, score)| score)
            .unwrap_or(0.0)
    }

    /// Returns the distribution in this rule that best matches the given file.
    pub(crate) fn best_matching_distribution(
        &self,
        file: &File,
    ) -> Option<(&Tracked<FsChangeDistribution>, f64)> {
        if !self.path_matcher.matches_file(file) {
            return None;
        }

        let change_counts = FsChangeCounts::from_file(file)?;

        self.distributions
            .iter()
            .filter_map(|distribution| {
                let score = distribution.match_probability(&change_counts);

                score.map(|score| (distribution, score))
            })
            .max_by(|(_, score1), (_, score2)| {
                score1.partial_cmp(score2).unwrap() // the probabilities are always between 0 and 1
                                                    // and can thus always be compared
            })
    }
}
