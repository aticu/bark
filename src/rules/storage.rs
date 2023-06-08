//! Provides a container for rules to efficiently match on them.

use std::{fmt, path::Path};

use radix_trie::{Trie, TrieCommon};

use crate::file::File;

use super::Rule;

/// A container to efficiently match multiple rules.
pub(crate) struct RuleStorage {
    /// The inner trie that stores the rules.
    map: Trie<String, Vec<Rule>>,
    /// The number of rules in the rule storage.
    len: usize,
}

impl RuleStorage {
    /// Creates a new empty rule storage.
    pub(crate) fn new() -> RuleStorage {
        RuleStorage {
            map: Trie::new(),
            len: 0,
        }
    }

    /// Inserts the given rule into the storeag.
    pub(crate) fn insert(&mut self, rule: Rule) {
        let prefix = rule.path_matcher.literal_prefix();
        if let Some(vec) = self.map.get_mut(&*prefix) {
            vec.push(rule);
        } else {
            self.map.insert(prefix.to_string(), vec![rule]);
        }
        self.len += 1;
    }

    /// Returns an iterator over all matching rules for the given file.
    pub(crate) fn all_rules_matching<'trie>(
        &'trie self,
        file: &'trie File,
    ) -> impl Iterator<Item = &'trie Rule> {
        file.paths.iter().flat_map(|path| self.rules_for_path(path))
    }

    /// Returns `true` if there is any rule matching the given file.
    pub(crate) fn is_matched(&self, file: &File) -> bool {
        self.all_rules_matching(file).next().is_some()
    }

    /// Returns the highest match score for a file, if there are any matching rules.
    pub(crate) fn match_score(&self, file: &File) -> Option<f64> {
        self.all_rules_matching(file)
            .map(move |rule| rule.match_score(file))
            .reduce(f64::max)
    }

    /// Returns an iterator over all rules matching the given path.
    fn rules_for_path<'trie>(&'trie self, path: &'trie str) -> impl Iterator<Item = &'trie Rule> {
        RulesFor {
            trie: &self.map,
            full_path: path,
            slice: &[],
            slice_idx: 0,
            path: Some(path),
        }
    }

    /// Saves the rule storage to the given rule file.
    pub(crate) fn save(&self, rule_file: impl AsRef<Path>) -> anyhow::Result<()> {
        serde_json::to_writer_pretty(std::fs::File::create(rule_file)?, self)?;

        Ok(())
    }

    /// Loads the rule storage from the given rule file.
    pub(crate) fn load(rule_file: impl AsRef<Path>) -> anyhow::Result<Self> {
        Ok(serde_json::from_reader(std::fs::File::open(rule_file)?)?)
    }
}

/// An iterator over all matching rules in `trie` for the given `full_path`.
struct RulesFor<'trie> {
    /// The trie that's the source of the rules.
    trie: &'trie Trie<String, Vec<Rule>>,
    /// The full path to search for rules for.
    full_path: &'trie str,
    /// The current rule slice being iterated over.
    slice: &'trie [Rule],
    /// The index into the current rule slice being iterated over.
    slice_idx: usize,
    /// The current sub-path that is being used to search for parent nodes in the trie.
    path: Option<&'trie str>,
}

impl<'trie> Iterator for RulesFor<'trie> {
    type Item = &'trie Rule;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            while let Some(rule) = self.slice.get(self.slice_idx) {
                self.slice_idx += 1;
                if rule.path_matcher.matches_path(self.full_path) {
                    return Some(rule);
                }
            }

            let Some(path) = self.path else { return None };
            let Some(subtrie) = self.trie.get_ancestor(path) else { return None };

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

        for rule in self.map.values() {
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

        let mut seq = serializer.serialize_seq(Some(self.len))?;
        for vec in self.map.values() {
            for rule in vec {
                seq.serialize_element(rule)?;
            }
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
