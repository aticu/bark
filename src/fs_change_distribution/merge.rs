//! Merging of change counts into change distributions.

use std::collections::BTreeMap;

use ordered_float::NotNan;
use smallvec::SmallVec;

use crate::{
    fs_changes::{FsChangeCounts, FsChanges},
    provenance::{Provenance, Tracked},
};

use super::{DistributionKind, FsChangeDistribution};

/// Merges many change counts into possibly fewer change distributions.
pub(crate) fn merge_counts_into_distributions(
    observations: &[Tracked<FsChangeCounts>],
) -> SmallVec<[Tracked<FsChangeDistribution>; 1]> {
    let permits_addition = permits_x(FsChanges::ADDED, observations);
    let permits_deletion = permits_x(FsChanges::DELETED, observations);

    // filters out additions and deletions if they should be filtered
    let mut filtered_observations = observations
        .iter()
        .map(|observation| {
            Tracked::new(
                observation.filtered(|changes, _| {
                    !(permits_addition && changes == FsChanges::ADDED
                        || permits_deletion && changes == FsChanges::DELETED)
                }),
                observation.provenance.clone(),
            )
        })
        .filter(|observation| observation.total_distinct_changes() > 0)
        .collect::<Vec<_>>();

    if filtered_observations.is_empty() {
        return smallvec::smallvec![Tracked::new(
            FsChangeDistribution {
                permits_addition,
                permits_deletion,
                kind: DistributionKind::Empty,
            },
            Provenance::joined_from(observations)
        )];
    }

    // sort so observations with more distinct change combinations come later
    // this allows them to be handled first and thus to merge others into them
    filtered_observations.sort_unstable_by_key(|observation| observation.total_distinct_changes());

    let mut result = SmallVec::new();

    while let Some(observation) = filtered_observations.pop() {
        result.push(merge_into_single(
            permits_addition,
            permits_deletion,
            observation,
            &mut filtered_observations,
        ));
    }

    result
}

/// Determine whether a single addition or deletion is permitted from the given observations.
fn permits_x(x: FsChanges, observations: &[Tracked<FsChangeCounts>]) -> bool {
    observations
        .iter()
        .any(|observation| observation.get(x).map(|count| count > 0).unwrap_or(false))
}

/// Merges the given observation along with possibly other observations into a distribution.
fn merge_into_single(
    permits_addition: bool,
    permits_deletion: bool,
    observation: Tracked<FsChangeCounts>,
    others: &mut Vec<Tracked<FsChangeCounts>>,
) -> Tracked<FsChangeDistribution> {
    if let Some(distribution) =
        try_merge_deterministic(permits_addition, permits_deletion, &observation, others)
    {
        return distribution;
    }

    merge_clustering(permits_addition, permits_deletion, &observation, others)
}

/// Tries to merge the given observation into a deterministic distribution.
fn try_merge_deterministic(
    permits_addition: bool,
    permits_deletion: bool,
    observation: &Tracked<FsChangeCounts>,
    others: &mut Vec<Tracked<FsChangeCounts>>,
) -> Option<Tracked<FsChangeDistribution>> {
    if observation.total_distinct_changes() != 1 {
        return None;
    }

    let changes = observation.iter().next().unwrap().0;
    let mut provenance = observation.provenance.clone();

    let mut i = 0;
    while let Some(other) = others.get(i) {
        if other.total_distinct_changes() == 1 && other.get(changes).is_some() {
            provenance.join(&other.provenance);
            others.remove(i);
            continue;
        }

        i += 1;
    }

    Some(Tracked::new(
        FsChangeDistribution {
            permits_addition,
            permits_deletion,
            kind: DistributionKind::Deterministic { changes },
        },
        provenance,
    ))
}

/// Tries to merge the given observation into other distributions similar to it.
fn merge_clustering(
    permits_addition: bool,
    permits_deletion: bool,
    observation: &Tracked<FsChangeCounts>,
    others: &mut Vec<Tracked<FsChangeCounts>>,
) -> Tracked<FsChangeDistribution> {
    /// Computes the distribution at the center of all given observations.
    fn compute_center<'a>(
        iter: impl Iterator<Item = &'a Tracked<FsChangeCounts>>,
        permits_addition: bool,
        permits_deletion: bool,
    ) -> Option<FsChangeDistribution> {
        let mut total_counts = BTreeMap::<_, u32>::new();

        for counts in iter {
            for (change, count) in counts.iter() {
                *total_counts.entry(change).or_default() += count;
            }
        }

        let total_count = total_counts.values().sum::<u32>() as usize as f64;

        if total_count == 0.0 {
            None
        } else {
            let probability_map = total_counts
                .iter()
                .map(|(change, count)| (*change, NotNan::new(*count as f64 / total_count).unwrap()))
                .collect();

            Some(FsChangeDistribution {
                permits_addition,
                permits_deletion,
                kind: DistributionKind::Complex {
                    changes: probability_map,
                },
            })
        }
    }

    /// Determines if the given observation matches the given center.
    fn matches_center(
        observation: &Tracked<FsChangeCounts>,
        center: &FsChangeDistribution,
    ) -> bool {
        let Some(score) = center.match_probability(observation) else {
            return false;
        };

        score > 0.20
    }

    let mut center = compute_center(
        std::iter::once(observation),
        permits_addition,
        permits_deletion,
    )
    .unwrap();
    let mut prev_num_matched = 1;

    loop {
        let mut matched_observations = vec![observation];

        for other in &*others {
            if matches_center(other, &center) {
                matched_observations.push(other);
            }
        }

        if matched_observations.len() <= prev_num_matched {
            break;
        }

        prev_num_matched = matched_observations.len();

        let new_center = compute_center(
            matched_observations.iter().copied(),
            permits_addition,
            permits_deletion,
        )
        .unwrap();

        if !matches_center(observation, &new_center) {
            break;
        }

        center = new_center;
    }

    let mut provenance = observation.provenance.clone();

    let mut i = 0;
    while let Some(other) = others.get(i) {
        if !matches_center(&others[i], &center) {
            i += 1;
            continue;
        }

        provenance.join(&other.provenance);
        others.remove(i);
    }

    Tracked::new(center, provenance)
}
