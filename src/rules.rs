//! Includes code for learning rules and matching them.

mod kmeans;
mod storage;

use std::{collections::BTreeMap, ops::AddAssign};

use inlinable_string::InlinableString;
use smallvec::SmallVec;
pub(crate) use storage::{MatchCountCache, RuleStorage};

use crate::{
    change_distribution::{ChangeDistribution, DistributionFamily},
    file::{DatasourceId, File, Files},
    path_matcher::PathMatcher,
};

/// A rule to describe one facet of the system behavior.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct Rule {
    /// What paths this rules applies to.
    path_matcher: PathMatcher,
    /// The observed distributions for this rule.
    distributions: SmallVec<[ObservedDistribution; 1]>,
    /// The combined distributions for this rule.
    ///
    /// Since this is only dependent on the other data, it is not stored on disk and recomputed
    /// each time.
    #[serde(skip)]
    combined_distributions: SmallVec<[ObservedDistribution; 1]>,
    /// Tags that can classify the rule.
    ///
    /// Tags are just small pieces of text associated with a rule that can be used for a number of
    /// purposes.
    /// For example to explain where a rule originates (e.g. `win10`, `shutdown_button`).
    tags: SmallVec<[InlinableString; 2]>,
}

impl Rule {
    /// Creates a rule with the given path matcher.
    pub(crate) fn from_matcher(path_matcher: PathMatcher, files: &Files) -> Option<Rule> {
        let mut this = Rule {
            path_matcher,
            distributions: SmallVec::new(),
            combined_distributions: SmallVec::new(),
            tags: Default::default(),
        };

        this.add_data_source(files);

        if this.distributions.is_empty() {
            None
        } else {
            Some(this)
        }
    }

    /// Adds the data from the given source to the rule
    pub(crate) fn add_data_source(&mut self, files: &Files) {
        let datasource_id = files.datasource_id();

        if self
            .distributions
            .iter()
            .any(|dist| dist.occurrences.contains_key(&datasource_id))
        {
            return;
        }

        for file in files
            .chronological_order()
            .files()
            .filter(|file| self.path_matcher.matches_file(file))
        {
            if let Some(distribution) = ChangeDistribution::from_file(file) {
                if let Some(dist) = self
                    .distributions
                    .iter_mut()
                    .find(|dist| dist.distribution == distribution)
                {
                    *dist.occurrences.entry(datasource_id).or_default() += 1;
                } else {
                    let mut occurrences = BTreeMap::new();
                    occurrences.insert(datasource_id, 1);

                    self.distributions.push(ObservedDistribution {
                        distribution,
                        occurrences,
                    });
                }
            }
        }

        self.compute_combined_distributions();
    }

    /// Ensures that that the internal state of the rule is consistent.
    pub(crate) fn ensure_consistency(&mut self) {
        self.compute_combined_distributions()
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

    /// Returns the tags of this rule.
    pub(crate) fn tags(&self) -> &[InlinableString] {
        &self.tags[..]
    }

    /// Returns the distributions of this rule.
    pub(crate) fn distributions(&self) -> &[ObservedDistribution] {
        &self.combined_distributions[..]
    }

    /// The number of original samples.
    pub(crate) fn sample_count(&self) -> u32 {
        self.distributions
            .iter()
            .map(|dist| dist.occurrences.values().sum::<u32>())
            .sum::<u32>()
    }

    /// Computes the combined distributions for this rule.
    fn compute_combined_distributions(&mut self) {
        //self.combined_distributions = self.distributions.clone();
        //return;

        /// The threshhold at which two distributions are considered close enough to be the same.
        const SAME_DISTRIBUTION_THRESHHOLD: f64 = 0.05;

        let mut families: Vec<(DistributionFamily, Vec<&ObservedDistribution>)> = Vec::new();
        for dist in &self.distributions {
            match families
                .iter()
                .position(|(family, _)| dist.distribution.belongs_to(family))
            {
                Some(i) => families[i].1.push(dist),
                None => {
                    let family = dist.distribution.get_family();
                    families.push((family, vec![dist]));
                }
            }
        }

        self.combined_distributions = SmallVec::new();

        'outer: for (family, distributions) in &families {
            // here we use the k means algorithm with increasing numbers of k until we find a
            // result we're satisfied with
            for k in 1.. {
                if k == distributions.len() {
                    self.combined_distributions
                        .extend(distributions.iter().copied().cloned());
                    continue 'outer;
                }

                let (centers, closest_center) = kmeans::kmeans(k, family, distributions);

                let good_solution = distributions.iter().enumerate().all(|(i, distribution)| {
                    let center = &centers[closest_center[i]];
                    let pvalue = center
                        .distribution
                        .same_distribution_probability(&distribution.distribution)
                        .unwrap_or(0.0);

                    pvalue > SAME_DISTRIBUTION_THRESHHOLD
                });

                if good_solution {
                    self.combined_distributions.extend(centers);
                    continue 'outer;
                }
            }
        }

        self.combined_distributions
            .sort_by(|dist1, dist2| dist1.distribution.cmp(&dist2.distribution));
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
    ) -> Option<(&ObservedDistribution, f64)> {
        if !self.path_matcher.matches_file(file) {
            return None;
        }

        let change_events = ChangeDistribution::from_file(file)?;

        self.combined_distributions
            .iter()
            .filter_map(|distribution| {
                let score = distribution
                    .distribution
                    .same_distribution_probability(&change_events);

                score.map(|score| (distribution, score))
            })
            .max_by(|(_, score1), (_, score2)| {
                score1.partial_cmp(score2).unwrap() // the probabilities are always between 0 and 1
                                                    // and can thus always be compared
            })
    }
}

/// Stores an distribution along with where it occurred.
#[derive(Debug, Clone, Hash, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) struct ObservedDistribution {
    /// The distribution.
    pub(crate) distribution: ChangeDistribution,
    /// All occurrences of the distribution.
    pub(crate) occurrences: BTreeMap<DatasourceId, u32>,
}

impl AddAssign<&Self> for ObservedDistribution {
    fn add_assign(&mut self, rhs: &Self) {
        self.distribution += &rhs.distribution;

        for (&source, &num) in &rhs.occurrences {
            *self.occurrences.entry(source).or_default() += num;
        }
    }
}
