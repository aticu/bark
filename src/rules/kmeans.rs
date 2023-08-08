//! Implements the k-means algorithm to merge probability distributions.

use rand::{seq::SliceRandom as _, Rng, SeedableRng as _};

use crate::change_distribution::{ChangeDistribution, DistributionFamily};

use super::ObservedDistribution;

/// Runs the k means++ algorithm, returning a list of k cluster centers along with the mapping of the
/// distributions.
pub(crate) fn kmeans(
    k: usize,
    family: &DistributionFamily,
    distributions: &[&ObservedDistribution],
) -> (Vec<ObservedDistribution>, Vec<usize>) {
    assert!(distributions.len() > k, "k is too large");

    // choose a fixed seed so that we get deterministic behavior
    let seed = u64::from_be_bytes(std::f64::consts::PI.to_be_bytes());
    let mut rng = rand::rngs::SmallRng::seed_from_u64(seed);

    let mut centers = initialize_centers(k, distributions, &mut rng);

    // since the closest centers are only important after the loop, we can initialize them in a
    // wrong way here
    let mut closest_center = vec![0; distributions.len()];
    loop {
        let mut new_centers = vec![ObservedDistribution::empty_from_family(family); k];

        for (dist_idx, &distribution) in distributions.iter().enumerate() {
            // we have at least one distribution, so the closest center exists
            let (center_idx, _) = find_closest_center(distribution, &centers).unwrap();

            closest_center[dist_idx] = center_idx;
            new_centers[center_idx] += distribution;
        }

        if new_centers == centers {
            break;
        } else {
            centers = new_centers;
        }
    }

    (centers, closest_center)
}

/// Initializes the starting k means centers according to the k means++ algorithm.
fn initialize_centers(
    k: usize,
    distributions: &[&ObservedDistribution],
    rng: &mut impl Rng,
) -> Vec<ObservedDistribution> {
    let mut centers: Vec<ObservedDistribution> = Vec::with_capacity(k);
    centers.push((*distributions.choose(rng).unwrap()).clone());

    while centers.len() < k {
        let dists_squared: Vec<f64> = distributions
            .iter()
            .map(|dist| {
                find_closest_center(dist, &centers)
                    .map(|(_, dist)| dist.powi(2))
                    .unwrap()
            })
            .collect();

        let dist_sum = dists_squared.iter().sum::<f64>();
        let choice = rng.gen_range(0.0..dist_sum);
        let mut current_sum = 0.0;
        let next_center = 'choose_center: {
            for (i, dist) in dists_squared.iter().enumerate() {
                current_sum += dist;
                if current_sum >= choice {
                    break 'choose_center i;
                }
            }
            dists_squared.len() - 1
        };

        centers.push(distributions[next_center].clone());
    }

    centers
}

/// Finds the closest center from the given list of centers.
fn find_closest_center(
    distribution: &ObservedDistribution,
    centers: &[ObservedDistribution],
) -> Option<(usize, f64)> {
    centers
        .iter()
        .enumerate()
        .map(|(i, center)| (i, distance(center, distribution)))
        .min_by(|(_, dist1), (_, dist2)| {
            // the distance is never NaN, so always comparable
            dist1.partial_cmp(dist2).unwrap()
        })
}

/// Measures the euclidean distance between two distributions.
fn distance(dist1: &ObservedDistribution, dist2: &ObservedDistribution) -> f64 {
    if dist1.distribution.changes.len() != dist2.distribution.changes.len() {
        return f64::INFINITY;
    }

    let total1 = dist1.distribution.changes.values().sum::<u32>() as f64;
    let total2 = dist2.distribution.changes.values().sum::<u32>() as f64;
    let mut distance: f64 = 0.0;

    for (event, &num1) in &dist1.distribution.changes {
        let Some(&num2) = dist2.distribution.changes.get(event) else { return f64::INFINITY };

        distance += (num1 as f64 / total1 - num2 as f64 / total2).powi(2);
    }

    distance.sqrt()
}

impl ObservedDistribution {
    /// Creates an empty distribution.
    ///
    /// This is useless by itself, but can be used to build up average distributions.
    fn empty_from_family(family: &DistributionFamily) -> Self {
        Self {
            distribution: ChangeDistribution::empty_from_family(family),
            occurrences: Default::default(),
        }
    }
}
