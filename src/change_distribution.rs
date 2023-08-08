//! Implements distributions of changes.

use std::{collections::BTreeMap, ops::AddAssign};

use crate::{change_event::ChangeEvent, file::File};

/// Stores all the changes that occurred for a single file or multiple files.
#[derive(
    Debug,
    Default,
    Clone,
    Hash,
    PartialEq,
    PartialOrd,
    Ord,
    Eq,
    serde::Deserialize,
    serde::Serialize,
)]
pub(crate) struct ChangeDistribution {
    /// Whether this distribution had a single `ADDED` event before normalization.
    pub(crate) has_single_addition: bool,
    /// Whether this distribution had a single `DELETED` event before normalization.
    pub(crate) has_single_deletion: bool,
    /// The different change events that occurred, together with the number of times they occurred.
    #[serde(flatten)]
    pub(crate) changes: BTreeMap<ChangeEvent, u32>,
}

impl ChangeDistribution {
    /// Computes all the changes that occurred in the given files.
    pub(crate) fn from_file(file: &File) -> Option<Self> {
        use sniff::MetaEntryDiff::*;

        let mut changes = BTreeMap::new();

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
            let existed_before = exists;

            let event = ChangeEvent::measure(change)?;

            exists |= event.contains(ChangeEvent::ADDED);
            exists &= !event.contains(ChangeEvent::DELETED);

            // we only count this change if the file exists now or existed before
            if existed_before || exists {
                *changes.entry(event).or_default() += 1;
            }
        }

        if changes.is_empty() {
            return None;
        }

        let has_single_addition = match changes.get(&ChangeEvent::ADDED) {
            Some(1) => {
                changes.remove(&ChangeEvent::ADDED);
                true
            }
            _ => false,
        };
        let has_single_deletion = match changes.get(&ChangeEvent::DELETED) {
            Some(1) => {
                changes.remove(&ChangeEvent::DELETED);
                true
            }
            _ => false,
        };

        Some(ChangeDistribution {
            has_single_addition,
            has_single_deletion,
            changes,
        })
    }

    /// Creates a new empty distribution that belongs to the given family.
    pub(crate) fn empty_from_family(family: &DistributionFamily) -> Self {
        ChangeDistribution {
            has_single_addition: family.has_single_addition,
            has_single_deletion: family.has_single_deletion,
            changes: Default::default(),
        }
    }

    /// Returns the "family" of this distribution.
    pub(crate) fn get_family(&self) -> DistributionFamily {
        use std::{
            collections::hash_map::DefaultHasher,
            hash::{Hash as _, Hasher as _},
        };

        let mut hasher = DefaultHasher::default();
        self.changes.len().hash(&mut hasher);
        for event in self.changes.keys() {
            event.hash(&mut hasher);
        }

        DistributionFamily {
            has_single_addition: self.has_single_addition,
            has_single_deletion: self.has_single_deletion,
            distribution_hash: hasher.finish(),
        }
    }

    /// Returns `true` if this distribution belongs to the given distribution family.
    pub(crate) fn belongs_to(&self, family: &DistributionFamily) -> bool {
        let this = self.get_family();

        this.has_single_addition == family.has_single_addition
            && this.has_single_deletion == family.has_single_deletion
            && (self.changes.is_empty() || this.distribution_hash == family.distribution_hash)
    }

    /// Returns the probability that the given other distribution is the same as this one.
    ///
    /// This function uses the G test to determine if the samples are from the same
    /// distribution.
    /// `self` is used as the reference distribution.
    pub(crate) fn same_distribution_probability(&self, other: &Self) -> Option<f64> {
        if !self.has_single_addition && other.has_single_addition
            || !self.has_single_deletion && other.has_single_deletion
        {
            return None;
        }

        // no need to test anything if there aren't any samples to compare
        // no samples should always match
        if other.changes.is_empty() {
            return Some(1.0);
        }

        // the G test doesn't make sense if the degrees of freedom is zero or less
        if self.changes.len() <= 1 {
            let other_is_event_subset = other
                .changes
                .keys()
                .all(|event| self.changes.contains_key(event));

            return if other_is_event_subset {
                Some(1.0)
            } else {
                None
            };
        }

        let total_self = self.changes.values().sum::<u32>();
        let total_other = other.changes.values().sum::<u32>();
        let expected_factor = total_other as f64 / total_self as f64;
        let mut found_other = 0;

        // computes the test statistic for the G test
        let statistic = 2.0
            * self
                .changes
                .iter()
                .map(|(&event, &num)| {
                    if let Some(&observed) = other.changes.get(&event) {
                        found_other += 1;

                        let observed = observed as f64;
                        let expected = num as f64 * expected_factor;

                        observed * (observed / expected).ln()
                    } else {
                        0.0
                    }
                })
                .sum::<f64>();

        if found_other != other.changes.len() {
            return None;
        }

        // since the contingency table is one-dimensional, the degrees of freedom is the number
        // of total events - 1
        let chi_squared =
            statrs::distribution::ChiSquared::new((self.changes.len() - 1) as f64).unwrap();

        // the p value is the probability that the test statistic is greater than the chi squared
        // distribution with the above described degrees of freedom
        let pvalue = 1.0 - statrs::distribution::ContinuousCDF::cdf(&chi_squared, statistic);

        Some(pvalue)
    }
}

/// Describes a family of change distributions.
#[derive(Debug)]
pub(crate) struct DistributionFamily {
    /// Whether a single addition is part of the family.
    has_single_addition: bool,
    /// Whether a single deletion is part of the family.
    has_single_deletion: bool,
    /// The hash of all other elements that make up the family.
    distribution_hash: u64,
}

impl AddAssign<&Self> for ChangeDistribution {
    fn add_assign(&mut self, rhs: &Self) {
        assert!(
            rhs.belongs_to(&self.get_family())
                || (self.changes.is_empty() && self.belongs_to(&rhs.get_family()))
        );

        for (&event, &num) in rhs.changes.iter() {
            *self.changes.entry(event).or_default() += num;
        }
    }
}
