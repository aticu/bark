//! Implements distributions of changes.

mod gui;
mod merge;

use std::collections::BTreeMap;

use ordered_float::NotNan;

use crate::fs_changes::{FsChangeCounts, FsChanges};

pub(crate) use merge::merge_counts_into_distributions;

/// Stores a distribution of file system changes that can occur to a file.
#[derive(
    Debug, Clone, Hash, PartialEq, PartialOrd, Ord, Eq, serde::Deserialize, serde::Serialize,
)]
pub(crate) struct FsChangeDistribution {
    /// Whether a single addition is permitted in the distribution.
    permits_addition: bool,
    /// Whether a single deletion is permitted in the distribution.
    permits_deletion: bool,
    /// The kind of distribution that we have here.
    kind: DistributionKind,
}

impl FsChangeDistribution {
    /// Returns a naive change distribution from the given counts for display purposes.
    pub(crate) fn naive_from_counts(counts: &FsChangeCounts) -> Self {
        Self {
            permits_addition: false,
            permits_deletion: false,
            kind: DistributionKind::Complex {
                changes: counts
                    .iter()
                    .map(|(changes, count)| {
                        (
                            changes,
                            NotNan::try_from(count as f64 / counts.total_count() as f64).unwrap(),
                        )
                    })
                    .collect(),
            },
        }
    }

    /// Returns the probability that the given counts match this distribution.
    pub(crate) fn match_probability(&self, counts: &FsChangeCounts) -> Option<f64> {
        match &self.kind {
            DistributionKind::Empty => {
                let mut total_count = counts.total_count();
                if self.permits_addition {
                    if let Some(count) = counts.get(FsChanges::ADDED) {
                        total_count -= count;
                    }
                }
                if self.permits_deletion {
                    if let Some(count) = counts.get(FsChanges::DELETED) {
                        total_count -= count;
                    }
                }
                if total_count == 0 {
                    Some(1.0)
                } else {
                    None
                }
            }
            DistributionKind::Deterministic { changes } => {
                for (count_changes, _) in counts {
                    if self.permits_addition && count_changes == FsChanges::ADDED {
                        continue;
                    }
                    if self.permits_deletion && count_changes == FsChanges::DELETED {
                        continue;
                    }
                    if count_changes == *changes {
                        continue;
                    }
                    return None;
                }

                Some(1.0)
            }
            DistributionKind::Either {
                first,
                second,
                min_probability,
                max_probability,
            } => {
                let min_probability = **min_probability;
                let max_probability = **max_probability;
                for (count_changes, _) in counts {
                    if self.permits_addition && count_changes == FsChanges::ADDED {
                        continue;
                    }
                    if self.permits_deletion && count_changes == FsChanges::DELETED {
                        continue;
                    }
                    if count_changes == *first {
                        continue;
                    }
                    if count_changes == *second {
                        continue;
                    }
                    return None;
                }

                let first_count = counts.get(*first).unwrap_or_default();
                let second_count = counts.get(*second).unwrap_or_default();
                let total = first_count + second_count;
                let first_prob = first_count as f64 / total as f64;

                if min_probability <= first_prob && first_prob <= max_probability {
                    Some(1.0)
                } else if first_prob <= min_probability {
                    g_test(
                        [(first, min_probability), (second, 1.0 - min_probability)].into_iter(),
                        |count| counts.get(*count),
                        total,
                    )
                } else {
                    g_test(
                        [(first, max_probability), (second, 1.0 - max_probability)].into_iter(),
                        |count| counts.get(*count),
                        total,
                    )
                }
            }
            DistributionKind::Complex { changes } => {
                for (count_changes, _) in counts {
                    if self.permits_addition && count_changes == FsChanges::ADDED {
                        continue;
                    }
                    if self.permits_deletion && count_changes == FsChanges::DELETED {
                        continue;
                    }
                    if changes.contains_key(&count_changes) {
                        continue;
                    }
                    return None;
                }
                g_test(
                    changes
                        .iter()
                        .map(|(&changes, &probability)| (changes, *probability)),
                    |changes| {
                        let result = counts.get(changes);
                        if self.permits_addition && changes.added() {
                            panic!("querying for addition when its part of the distribution");
                        }
                        if self.permits_deletion && changes.deleted() {
                            panic!("querying for deletion when its part of the distribution");
                        }
                        result
                    },
                    {
                        let mut result = counts.total_count();
                        if self.permits_addition {
                            if let Some(count) = counts.get(FsChanges::ADDED) {
                                result -= count;
                            }
                        }
                        if self.permits_deletion {
                            if let Some(count) = counts.get(FsChanges::DELETED) {
                                result -= count;
                            }
                        }
                        result
                    },
                )
            }
        }
    }
}

/// The possible kinds of file system change distributions.
#[derive(
    Debug, Clone, Hash, PartialEq, PartialOrd, Ord, Eq, serde::Deserialize, serde::Serialize,
)]
enum DistributionKind {
    /// No changes have been observed.
    Empty,
    /// The changes that have occurred have always been deterministic.
    Deterministic {
        /// The changes that occurred every time.
        changes: FsChanges,
    },
    /// Either the first or the second change have been observed with possibly different
    /// probabilities.
    Either {
        /// The first observed changes.
        first: FsChanges,
        /// The second observed changes.
        second: FsChanges,
        /// The minimum observed probability for the first changes.
        min_probability: NotNan<f64>,
        /// The maximum observed probability for the first changes.
        max_probability: NotNan<f64>,
    },
    /// The set of changes seen was more complex than any of the other variants capture.
    Complex {
        /// Maps the changes to their probabilities of occurring.
        changes: BTreeMap<FsChanges, NotNan<f64>>,
    },
}

/// Performs the G test to determine how likely `counts` follows `distribution`.
///
/// If there are fundamentally different events that were measured, `None` is returned instead of
/// `0.0` to differentiate between a distribution that does not match and one that cannot match.
fn g_test<Item: Eq>(
    distribution: impl Iterator<Item = (Item, f64)> + ExactSizeIterator,
    mut counts: impl FnMut(Item) -> Option<u32>,
    total_count: u32,
) -> Option<f64> {
    let dist_size = distribution.len();
    let degrees_of_freedom = dist_size - 1;

    assert!(
        degrees_of_freedom >= 1,
        "don't call g test with a deterministic or empty distribution"
    );

    let total_count = total_count as f64;

    let mut covered_items = 0;

    let test_statistic = 2.0
        * distribution
            .map(|(item, probability)| {
                if let Some(observed) = counts(item) {
                    if observed > 0 {
                        covered_items += 1;

                        let observed = observed as f64;
                        let expected = probability * total_count;

                        observed * (observed / expected).ln()
                    } else {
                        0.0
                    }
                } else {
                    0.0
                }
            })
            .sum::<f64>();

    // since the contingency table is one-dimensional, the degrees of freedom is the number
    // of total events - 1
    let chi_squared = statrs::distribution::ChiSquared::new(degrees_of_freedom as f64).unwrap();

    // the p value is the probability that the test statistic is greater than the chi squared
    // distribution
    let pvalue = 1.0 - statrs::distribution::ContinuousCDF::cdf(&chi_squared, test_statistic);

    Some(pvalue)
}
