//! Provides a container for rules to efficiently match on them.

use radix_trie::{Trie, TrieCommon};

use crate::{
    file::File,
    glob::{glob_matches, is_glob_special_char},
};

use super::Rule;

/// A container to efficiently match multiple rules.
#[derive(Debug)]
pub(crate) struct RuleStorage {
    /// The inner map to
    map: Trie<String, Vec<Rule>>,
}

impl RuleStorage {
    /// Creates a new empty rule storage.
    pub(crate) fn new() -> RuleStorage {
        RuleStorage { map: Trie::new() }
    }

    /// Creates a new rule storage containing the given rule.
    pub(crate) fn from_rule(rule: Rule) -> RuleStorage {
        let mut this = Self::new();
        this.insert(rule);
        this
    }

    /// Inserts the given rule into the storeag.
    pub(crate) fn insert(&mut self, rule: Rule) {
        let prefix = literal_match_prefix(&rule.glob);
        if let Some(vec) = self.map.get_mut(prefix) {
            vec.push(rule);
        } else {
            self.map.insert(prefix.to_string(), vec![rule]);
        }
    }

    /// Returns an iterator over all matching rules for the given file.
    fn all_rules_matching<'trie>(
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
                if glob_matches(&rule.glob, self.full_path) {
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

/// Returns the prefix of the string that only contains characters that are literally matches in
/// globs.
fn literal_match_prefix(s: &str) -> &str {
    s.split(is_glob_special_char).next().unwrap()
}
