//! Provides a container for rules to efficiently match on them.

use radix_trie::{Trie, TrieCommon};

use super::{Rule, RuleMatcher};

/// A container to efficiently match multiple rules.
#[derive(Debug)]
pub(crate) struct RuleStorage {
    /// The inner map to
    map: Trie<String, Vec<Rule>>,
    /// The amount of rules stored in the storage.
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
        let RuleMatcher::Glob(glob) = &rule.matcher;
        let prefix = literal_match_prefix(glob);
        if let Some(vec) = self.map.get_mut(prefix) {
            vec.push(rule);
        } else {
            self.map.insert(prefix.to_string(), vec![rule]);
        }
        self.len += 1;
    }

    /// Returns an iterator over all rules matching the given path.
    pub(crate) fn rules_for<'trie>(
        &'trie self,
        path: &'trie str,
    ) -> impl Iterator<Item = &'trie Rule> {
        RulesFor {
            trie: &self.map,
            full_path: path,
            slice: &[],
            slice_idx: 0,
            path: Some(path),
        }
    }

    /// The number of rules in the storage.
    pub(crate) fn len(&self) -> usize {
        self.len
    }
}

/// An iterator over all matching rules in `trie` for the given `full_path`.
struct RulesFor<'trie> {
    trie: &'trie Trie<String, Vec<Rule>>,
    full_path: &'trie str,
    slice: &'trie [Rule],
    slice_idx: usize,
    path: Option<&'trie str>,
}

impl<'trie> Iterator for RulesFor<'trie> {
    type Item = &'trie Rule;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            while let Some(rule) = self.slice.get(self.slice_idx) {
                self.slice_idx += 1;
                if rule.matcher.matches(self.full_path) {
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
    s.split(|c| matches!(c, '?' | '*' | '[' | ']' | '{' | '}' | '!' | '^' | '\\'))
        .next()
        .unwrap()
}
