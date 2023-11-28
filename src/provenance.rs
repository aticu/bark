//! Contains types to describe the origin of data derived from sources.

use std::{
    collections::{BTreeMap, BTreeSet},
    ops::{Deref, DerefMut},
};

use crate::{file::DatasourceId, rules::RuleStorage};

/// Describes the origin of data derived from sources.
#[derive(Debug, Default, Clone, Hash, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
pub(crate) struct Provenance {
    /// Maps each data source to the number of occurrences that the data had in that source.
    #[serde(flatten)]
    pub(crate) sources: BTreeMap<DatasourceId, u32>,
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

    /// Returns all tags that match the tracked object.
    pub(crate) fn tags<'storage>(&self, storage: &'storage RuleStorage) -> BTreeSet<&'storage str> {
        let mut first = true;
        let mut tags = BTreeSet::new();

        for &source in self.provenance.sources.keys() {
            let Some(source) = storage.source(source) else { continue };
            let source_tags = source.tags.iter().map(|s| &**s).collect();

            if first {
                first = false;
                tags = source_tags;
            } else {
                tags = tags.intersection(&source_tags).copied().collect();
            }
        }

        tags
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

/// Stored information about a data source.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct Source {
    /// The ID of the source.
    pub(crate) id: DatasourceId,
    /// The name of the source.
    pub(crate) name: Option<String>,
    /// A description of the source.
    pub(crate) description: Option<String>,
    /// Some tags that are associated with the source.
    pub(crate) tags: Vec<String>,
}

impl Source {
    /// Creates a new Source from the given ID.
    pub(crate) fn new(id: DatasourceId) -> Source {
        Source {
            id,
            name: None,
            description: None,
            tags: Vec::new(),
        }
    }

    /// Creates a new Source from the given ID.
    pub(crate) fn new_with_name(id: DatasourceId, name: String) -> Source {
        Source {
            id,
            name: Some(name),
            description: None,
            tags: Vec::new(),
        }
    }
}
