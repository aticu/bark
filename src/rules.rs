//! Includes code for learning rules and matching them.

mod storage;

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
    str::FromStr,
};

use sniff::Changeset;

pub(crate) use storage::RuleStorage;

/// The threshhold for how frequent a change must be in order for it to become a rule.
const FREQUENCY_TRESHHOLD: f64 = 0.6;

/// A rule to describe one facet of the system behavior.
#[derive(Debug)]
pub(crate) struct Rule {
    /// The frequency of cases that this rule is expected to match.
    frequency: f64,
    /// What this rule applies to.
    matcher: RuleMatcher,
    /// The kind of behavior this rule describes.
    kind: RuleKind,
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {:}% of the time at {}",
            self.kind,
            self.frequency * 100.0,
            self.matcher,
        )
    }
}

impl FromStr for Rule {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        let first_num = s
            .find(|c: char| c.is_ascii_digit())
            .ok_or("expected to find a number")?;
        let kind = s[..first_num].trim().parse()?;

        let s = &s[first_num..];
        let percent = s.find('%').ok_or("expected % sign")?;
        let frequency = s[..percent]
            .parse::<f64>()
            .map_err(|_| "expected float number")?
            / 100.0;

        let s = &s[percent..];
        if let Some(matcher) = s.strip_prefix("% of the time at ") {
            let matcher = matcher.parse()?;

            Ok(Self {
                frequency,
                matcher,
                kind,
            })
        } else {
            Err("expected the string `% of the time at `")
        }
    }
}

impl Rule {
    /// Whether the rule matches the given change.
    fn matches<Timestamp>(&self, path: &str, diff: &sniff::MetaEntryDiff<Timestamp>) -> bool {
        self.matcher.matches(path) && self.kind.matches(diff)
    }

    /// The frequency of rule matches in the given diff set.
    pub(crate) fn match_frequency_in<'a, T>(&self, path: &str, diffs: &'a [T]) -> f64
    where
        ChangeKind: From<&'a T>,
    {
        if self.matcher.matches(path) {
            let match_count = diffs.iter().filter(|&diff| self.kind.matches(diff)).count();

            match_count as f64 / diffs.len() as f64
        } else {
            0.0
        }
    }
}

/// Describes what a rule applies to.
#[derive(Debug)]
pub(crate) enum RuleMatcher {
    /// The rule applies to all paths matching the given globby path.
    Glob(String),
}

impl fmt::Display for RuleMatcher {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Glob(glob) => write!(f, "{glob}"),
        }
    }
}

impl FromStr for RuleMatcher {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(RuleMatcher::Glob(s.to_string()))
    }
}

impl RuleMatcher {
    /// Whether the rule matcher applies to the given path.
    fn matches(&self, path: &str) -> bool {
        match self {
            Self::Glob(glob) => glob_match::glob_match(glob, path),
        }
    }
}

/// Describes the kind of behavior that a rule describes.
#[derive(Debug)]
pub(crate) enum RuleKind {
    /// The rule only matches the single given change kind.
    SingleChange(ChangeKind),
    /// The rule only matches both of the given change kinds.
    TwoChangeKinds(ChangeKind, ChangeKind),
    /// The rule matches additions and optionally the given change kind.
    Addition(Option<ChangeKind>),
    /// The rule matches deletions and optionally the given change kind.
    Deletion(Option<ChangeKind>),
    /// The rule matches additions, deletions and optionally the given change kind.
    AdditionAndDeletion(Option<ChangeKind>),
}

impl fmt::Display for RuleKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SingleChange(change) => write!(f, "{change}"),
            Self::TwoChangeKinds(change1, change2) => write!(f, "{change1} or {change2}"),
            Self::Addition(Some(change)) => write!(f, "addition or {change}"),
            Self::Addition(None) => write!(f, "addition"),
            Self::Deletion(Some(change)) => write!(f, "deletion or {change}"),
            Self::Deletion(None) => write!(f, "deletion"),
            Self::AdditionAndDeletion(Some(change)) => write!(f, "addition, deletion or {change}"),
            Self::AdditionAndDeletion(None) => write!(f, "addition or deletion"),
        }
    }
}

impl FromStr for RuleKind {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("addition") {
            if let Some(kind) = s.strip_prefix("addition or ") {
                Ok(RuleKind::Addition(Some(kind.parse()?)))
            } else if let Some(kind) = s.strip_prefix("addition, deletion or ") {
                Ok(RuleKind::AdditionAndDeletion(Some(kind.parse()?)))
            } else if s == "addition" {
                Ok(RuleKind::Addition(None))
            } else if s == "addition or deletion" {
                Ok(RuleKind::AdditionAndDeletion(None))
            } else {
                Err("unexpected text starting with `addition`")
            }
        } else if s.starts_with("deletion") {
            if let Some(kind) = s.strip_prefix("deletion or ") {
                Ok(RuleKind::Deletion(Some(kind.parse()?)))
            } else if s == "deletion" {
                Ok(RuleKind::Deletion(None))
            } else {
                Err("unexpected text starting with `deletion`")
            }
        } else if let Some(or_index) = s.find(" or ") {
            let first_kind = s[..or_index].parse()?;
            let second_kind = s[or_index..].strip_prefix(" or ").unwrap().parse()?;

            Ok(RuleKind::TwoChangeKinds(first_kind, second_kind))
        } else {
            Ok(RuleKind::SingleChange(s.parse()?))
        }
    }
}

impl RuleKind {
    /// Whether the rule kind matches the given behavior.
    fn matches<'a, T>(&self, diff: &'a T) -> bool
    where
        ChangeKind: From<&'a T>,
    {
        let kind = ChangeKind::from(diff);
        match self {
            Self::SingleChange(change_kind) => *change_kind == kind,
            Self::TwoChangeKinds(change_kind1, change_kind2) => {
                *change_kind1 == kind || *change_kind2 == kind
            }
            Self::Addition(change_kind) => kind == ChangeKind::Added || change_kind == &Some(kind),
            Self::Deletion(change_kind) => {
                kind == ChangeKind::Deleted || change_kind == &Some(kind)
            }
            Self::AdditionAndDeletion(change_kind) => {
                kind == ChangeKind::Added
                    || kind == ChangeKind::Deleted
                    || change_kind == &Some(kind)
            }
        }
    }
}

/// Describes the kinds of changes that can be made.
///
/// The change kinds are ordered so that more significant changes will compare as greater.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum ChangeKind {
    /// The entry was unchanged.
    Unchanged,
    /// The inode modification timestamp of the entry was changed.
    InodeModifiedTimestamp,
    /// The accessed timestamp of the entry was changed.
    AccessedTimestamp,
    /// The modified timestamp of the entry was changed.
    ModifiedTimestamp,
    /// The created timestamp of the entry was changed.
    CreatedTimestamp,
    /// The size of the entry was changed.
    SizeChange,
    /// The inode of the entry changed.
    InodeChange,
    /// The entry metadata was changed in a more complex manner.
    Complex,
    /// The content of the entry was changed.
    ///
    /// Only applies to files.
    ContentModified,
    /// The entry was deleted.
    Deleted,
    /// The entry was added.
    Added,
}

impl fmt::Display for ChangeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.string_description())
    }
}

impl FromStr for ChangeKind {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        for kind in Self::all_kinds() {
            if kind.string_description() == s {
                return Ok(kind);
            }
        }
        Err("didn't find a known change kind")
    }
}

impl<Timestamp> From<&sniff::MetaEntryDiff<Timestamp>> for ChangeKind {
    fn from(value: &sniff::MetaEntryDiff<Timestamp>) -> ChangeKind {
        match value {
            sniff::MetaEntryDiff::Added(_) => ChangeKind::Added,
            sniff::MetaEntryDiff::Deleted(_) => ChangeKind::Deleted,
            sniff::MetaEntryDiff::MetaOnlyChange(meta) => {
                if matches!(&meta.changes[..], [sniff::MetadataChange::Size(_)]) {
                    ChangeKind::SizeChange
                } else if !meta.changes.is_empty() {
                    ChangeKind::Complex
                } else if meta.inode.is_changed() {
                    ChangeKind::InodeChange
                } else if meta.created.is_changed() {
                    ChangeKind::CreatedTimestamp
                } else if meta.modified.is_changed() {
                    ChangeKind::ModifiedTimestamp
                } else if meta.accessed.is_changed() {
                    ChangeKind::AccessedTimestamp
                } else if meta.inode_modified.is_changed() {
                    ChangeKind::InodeModifiedTimestamp
                } else {
                    ChangeKind::Unchanged
                }
            }
            sniff::MetaEntryDiff::EntryChange(_, _) => ChangeKind::ContentModified,
        }
    }
}

impl From<&ChangeKind> for ChangeKind {
    fn from(value: &ChangeKind) -> Self {
        *value
    }
}

impl ChangeKind {
    /// Returns a short string description of the change.
    fn string_description(&self) -> &'static str {
        use ChangeKind::*;
        match self {
            Unchanged => "no change",
            InodeModifiedTimestamp => "inode timestamp change",
            AccessedTimestamp => "access timestamp change",
            ModifiedTimestamp => "modification timestamp change",
            CreatedTimestamp => "creation timestamp change",
            SizeChange => "size change",
            InodeChange => "inode change",
            Complex => "complex metadata change",
            ContentModified => "content change",
            Deleted => "deletion",
            Added => "addition",
        }
    }

    /// Returns an iterator over all possible change kinds.
    fn all_kinds() -> impl Iterator<Item = Self> {
        use ChangeKind::*;
        [
            Unchanged,
            InodeModifiedTimestamp,
            AccessedTimestamp,
            ModifiedTimestamp,
            CreatedTimestamp,
            SizeChange,
            InodeChange,
            Complex,
            ContentModified,
            Deleted,
            Added,
        ]
        .into_iter()
    }
}

/// Learns simple rules that only concern a single path.
pub(crate) fn learn_basic_rules<Timestamp>(
    rules: &mut Vec<Rule>,
    all_paths: &BTreeSet<&str>,
    input: &[Changeset<Timestamp>],
) {
    for path in all_paths {
        let mut change_kinds = std::collections::BTreeMap::new();
        for changeset in input {
            let change_kind = if let Some(change) = changeset.changes.get(*path) {
                ChangeKind::from(change)
            } else {
                continue;
            };

            *change_kinds.entry(change_kind).or_insert(0) += 1;
        }

        let rule = 'rule: {
            if let Some((first_kind, first_count)) = change_kinds.pop_last() {
                if first_kind == ChangeKind::Complex {
                    // we shouldn't create rules for complex changes
                    break 'rule None;
                }

                if let Some((second_kind, second_count)) = change_kinds.pop_last() {
                    if !change_kinds.is_empty() {
                        break 'rule None;
                    }
                    if second_kind == ChangeKind::Complex {
                        // we shouldn't create rules for complex changes
                        break 'rule None;
                    }

                    let frequency = (first_count + second_count) as f64 / input.len() as f64;
                    if frequency < FREQUENCY_TRESHHOLD {
                        break 'rule None;
                    }
                    break 'rule Some(Rule {
                        frequency,
                        matcher: RuleMatcher::Glob(crate::path::glob_escape(path)),
                        kind: RuleKind::TwoChangeKinds(first_kind, second_kind),
                    });
                } else {
                    let frequency = first_count as f64 / input.len() as f64;
                    if frequency < FREQUENCY_TRESHHOLD {
                        break 'rule None;
                    }

                    break 'rule Some(Rule {
                        frequency,
                        matcher: RuleMatcher::Glob(crate::path::glob_escape(path)),
                        kind: RuleKind::SingleChange(first_kind),
                    });
                }
            }

            None
        };

        if let Some(rule) = rule {
            rules.push(rule);
        }
    }
}

/// Learns glob rules that affect multiple paths at once.
pub(crate) fn learn_glob_rules<Timestamp>(
    rules: &mut Vec<Rule>,
    all_paths: &BTreeSet<&str>,
    input: &[Changeset<Timestamp>],
) {
    let mut candidates = BTreeMap::new();

    for path in all_paths {
        if input.iter().any(|changeset| {
            changeset
                .changes
                .get(*path)
                .map(|change| {
                    let kind = ChangeKind::from(change);
                    kind == ChangeKind::Added || kind == ChangeKind::Deleted
                })
                .unwrap_or(false)
        }) {
            candidates
                .entry(path.chars().count())
                .or_insert_with(Vec::new)
                .push(*path);
        }
    }

    let mut prefix_groups = Vec::new();

    for paths_of_len in candidates.values() {
        let mut start = 0;

        'outer: while start < paths_of_len.len() {
            if paths_of_len.len() - start == 1 {
                prefix_groups.push((paths_of_len[start], &paths_of_len[start..]));
                start += 1;
                continue;
            }

            let mut prefix =
                crate::path::longest_common_prefix(paths_of_len[start], paths_of_len[start + 1]);
            for i in start + 2..paths_of_len.len() {
                let new_prefix = crate::path::longest_common_prefix(prefix, paths_of_len[i]);

                // Allow shrinking the prefix by a little bit
                if prefix.len() - new_prefix.len() < 5 {
                    prefix = new_prefix;
                } else {
                    prefix_groups.push((prefix, &paths_of_len[start..i]));
                    start = i;
                    continue 'outer;
                }
            }

            prefix_groups.push((prefix, &paths_of_len[start..paths_of_len.len()]));
            start = paths_of_len.len();
        }
    }

    'outer: for (_prefix, group) in &prefix_groups {
        for path in *group {
            if !path.match_indices('/').eq(group[0].match_indices('/')) {
                // if the path segment separators don't line up, this likely isn't a group
                continue 'outer;
            }
        }

        let mut glob = String::new();
        for (i, c) in group[0].chars().enumerate() {
            if group.iter().all(|path| path.chars().nth(i).unwrap() == c) {
                let (escape, c) = crate::path::glob_escape_char(c);
                glob.extend(escape);
                glob.push(c);
            } else {
                glob.push('?');
            }
        }

        let matcher = RuleMatcher::Glob(glob);

        let mut change_kinds = std::collections::BTreeMap::new();
        let mut num_paths = 0;
        for path in all_paths {
            if !matcher.matches(path) {
                continue;
            }
            num_paths += 1;

            for changeset in input {
                let change_kind = if let Some(change) = changeset.changes.get(*path) {
                    ChangeKind::from(change)
                } else {
                    continue;
                };

                *change_kinds.entry(change_kind).or_insert(0) += 1;
            }
        }

        let addition = change_kinds.remove(&ChangeKind::Added).is_some();
        let deletion = change_kinds.remove(&ChangeKind::Deleted).is_some();

        if change_kinds.len() > 1 {
            // for additions and deletions, we only have rules with one different change kind for
            // now
            continue;
        }

        let inner_kind = change_kinds.pop_last().map(|(kind, _)| kind);

        if inner_kind == Some(ChangeKind::Complex) {
            // complex changes should not simply be categorized together in a rule
            continue;
        }

        let kind = match (addition, deletion) {
            (false, false) => unreachable!("should have been sorted out earlier"),
            (false, true) => RuleKind::Deletion(inner_kind),
            (true, false) => RuleKind::Addition(inner_kind),
            (true, true) => RuleKind::AdditionAndDeletion(inner_kind),
        };

        let rule = Rule {
            // since no change is pretty much meaningless when the file may not exist (because of
            // additions and deletions) and all other cases are handled, we just set the frequency
            // like this
            frequency: 1.0,
            matcher,
            kind,
        };

        println!("{rule}");
        println!("# matched {} entries:", num_paths);
        for path in all_paths {
            if !rule.matcher.matches(path) {
                continue;
            }
            println!("# {path}");
        }
    }
}
