//! Provides a container for rules to efficiently match on them.

use std::{fmt, path::Path};

use radix_trie::{Trie, TrieCommon};
use smallvec::SmallVec;

use crate::file::File;

use super::Rule;

/// A container to efficiently match multiple rules.
#[derive(Clone)]
pub(crate) struct RuleStorage {
    /// Stores the rules.
    rules: Vec<Rule>,
    /// The inner trie that stores the indices of the rules.
    ///
    /// This is used so that lookup of rules by their path is more efficient.
    trie: Trie<String, SmallVec<[usize; 1]>>,
}

impl RuleStorage {
    /// Creates a new empty rule storage.
    pub(crate) fn new() -> RuleStorage {
        RuleStorage {
            rules: Vec::new(),
            trie: Trie::new(),
        }
    }

    /// Inserts the given rule into the storeag.
    pub(crate) fn insert(&mut self, rule: Rule) {
        let prefix = rule.path_matcher.literal_prefix().to_string();

        let idx = self.rules.len();
        self.rules.push(rule);

        if let Some(vec) = self.trie.get_mut(&*prefix) {
            vec.push(idx);
        } else {
            self.trie
                .insert(prefix.to_string(), smallvec::smallvec![idx]);
        }
    }

    /// Returns an iterator over all matching rules for the given file.
    pub(crate) fn rules_matching<'trie>(
        &'trie self,
        file: &'trie File,
    ) -> impl Iterator<Item = &'trie Rule> {
        file.paths.iter().flat_map(|path| self.rules_for_path(path))
    }

    /// Returns `true` if there is any rule matching the given file.
    pub(crate) fn is_matched(&self, file: &File) -> bool {
        self.rules_matching(file).next().is_some()
    }

    /// Returns the highest match score for a file, if there are any matching rules.
    pub(crate) fn match_score(&self, file: &File) -> Option<f64> {
        self.rules_matching(file)
            .map(move |rule| rule.match_score(file))
            .reduce(f64::max)
    }

    /// Returns an iterator over all rules matching the given path.
    fn rules_for_path<'storage>(
        &'storage self,
        path: &'storage str,
    ) -> impl Iterator<Item = &'storage Rule> {
        RulesFor {
            storage: self,
            full_path: path,
            slice: &[],
            slice_idx: 0,
            path: Some(path),
        }
    }

    /// Returns a mutable iterator over the contained rules.
    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut Rule> {
        self.into_iter()
    }

    /// Returns a number indicative of the current version of this rule storage.
    ///
    /// This number changes with very high probability if the stored rules change.
    pub(crate) fn version(&self) -> u64 {
        use std::{
            collections::hash_map::DefaultHasher,
            hash::{Hash as _, Hasher as _},
        };

        let mut hasher = DefaultHasher::new();
        self.rules.hash(&mut hasher);
        hasher.finish()
    }

    /// Saves the rule storage to the given rule file.
    pub(crate) fn save(&self, rule_file: impl AsRef<Path>) -> anyhow::Result<()> {
        let path = rule_file.as_ref();

        if let Some(dir) = path.parent() {
            let temp = tempfile::NamedTempFile::new_in(dir)?;
            serde_json::to_writer_pretty(&temp, self)?;
            temp.persist(path)?;
        } else {
            anyhow::bail!("file path does not have a parent")
        }

        Ok(())
    }

    /// Loads the rule storage from the given rule file.
    pub(crate) fn load(rule_file: impl AsRef<Path>) -> anyhow::Result<Self> {
        Ok(serde_json::from_reader(std::fs::File::open(rule_file)?)?)
    }
}

impl<'storage> IntoIterator for &'storage RuleStorage {
    type Item = &'storage Rule;
    type IntoIter = std::slice::Iter<'storage, Rule>;

    fn into_iter(self) -> Self::IntoIter {
        self.rules.iter()
    }
}

impl<'storage> IntoIterator for &'storage mut RuleStorage {
    type Item = &'storage mut Rule;
    type IntoIter = std::slice::IterMut<'storage, Rule>;

    fn into_iter(self) -> Self::IntoIter {
        self.rules.iter_mut()
    }
}

/// An iterator over all matching rules in `trie` for the given `full_path`.
struct RulesFor<'storage> {
    /// The trie that's the source of the rules.
    storage: &'storage RuleStorage,
    /// The full path to search for rules for.
    full_path: &'storage str,
    /// The current rule slice being iterated over.
    slice: &'storage [usize],
    /// The index into the current rule slice being iterated over.
    slice_idx: usize,
    /// The current sub-path that is being used to search for parent nodes in the trie.
    path: Option<&'storage str>,
}

impl<'storage> Iterator for RulesFor<'storage> {
    type Item = &'storage Rule;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            while let Some(rule_idx) = self.slice.get(self.slice_idx) {
                self.slice_idx += 1;
                let rule = &self.storage.rules[*rule_idx];
                if rule.path_matcher.matches_path(self.full_path) {
                    return Some(rule);
                }
            }

            let Some(path) = self.path else { return None };
            let Some(subtrie) = self.storage.trie.get_ancestor(path) else { return None };

            let path = subtrie.key().unwrap();
            if let Some((last_idx, _)) = path.char_indices().next_back() {
                self.path = Some(&path[..last_idx]);
            } else {
                self.path = None;
            }
            self.slice = subtrie.value().unwrap();
            self.slice_idx = 0;
        }
    }
}

impl fmt::Debug for RuleStorage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut list = f.debug_list();

        for rule in self.trie.values() {
            list.entry(rule);
        }

        list.finish()
    }
}

impl serde::Serialize for RuleStorage {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeSeq as _;

        let mut seq = serializer.serialize_seq(Some(self.rules.len()))?;
        for rule in self {
            seq.serialize_element(rule)?;
        }
        seq.end()
    }
}

impl<'de> serde::Deserialize<'de> for RuleStorage {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor;

        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = RuleStorage;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("rule storage")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let mut storage = RuleStorage::new();

                while let Some(rule) = seq.next_element()? {
                    storage.insert(rule)
                }

                Ok(storage)
            }
        }

        deserializer.deserialize_seq(Visitor)
    }
}
