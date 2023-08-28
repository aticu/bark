//! Contains types to describe the origin of data derived from sources.

use std::{
    collections::BTreeMap,
    ops::{Deref, DerefMut},
};

use crate::file::DatasourceId;

/// Describes the origin of data derived from sources.
#[derive(Debug, Default, Clone, Hash, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub(crate) struct Provenance {
    /// Maps each data source to the number of occurrences that the data had in that source.
    #[serde(flatten)]
    sources: BTreeMap<DatasourceId, u32>,
}

impl Provenance {
    /// Joins the provenance information from all tracked items into a single provenance.
    pub(crate) fn joined_from<'a, T: 'a>(
        iter: impl IntoIterator<Item = &'a Tracked<T>>,
    ) -> Provenance {
        let mut provenance = Provenance::default();

        for entry in iter {
            provenance.join(&entry.provenance);
        }

        provenance
    }

    /// Joins the other provenance information into this one.
    pub(crate) fn join(&mut self, other: &Provenance) {
        for (&source, count) in &other.sources {
            *self.sources.entry(source).or_default() += count;
        }
    }
}

/// Tracks the source of the contained `T`.
#[derive(Debug, Default, Clone, Hash, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub(crate) struct Tracked<T> {
    /// The data that is being tracked.
    pub(crate) data: T,
    /// The provenance of the data.
    pub(crate) provenance: Provenance,
}

impl<T> Tracked<T> {
    /// Creates new tracked data with the given provenance.
    pub(crate) fn new(data: T, provenance: Provenance) -> Self {
        Tracked { data, provenance }
    }

    /// Creates new tracked data from the given source.
    pub(crate) fn from_single_source(data: T, source: DatasourceId) -> Self {
        let mut this = Tracked {
            data,
            provenance: Provenance {
                sources: BTreeMap::new(),
            },
        };

        this.add_source(source);

        this
    }

    /// Returns true, if some or all of the tracked data came from the given source.
    pub(crate) fn is_from_source(&self, source: DatasourceId) -> bool {
        self.provenance.sources.contains_key(&source)
    }

    /// Adds one occurrence of the given source to the data sources.
    pub(crate) fn add_source(&mut self, source: DatasourceId) {
        *self.provenance.sources.entry(source).or_default() += 1;
    }

    /// Counts the total occurrences of this data within its sources.
    pub(crate) fn occurrence_count(&self) -> u32 {
        self.provenance.sources.values().sum()
    }
}

impl<T> Deref for Tracked<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> DerefMut for Tracked<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}
