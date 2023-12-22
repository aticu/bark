//! Defines the change event type, used to describe changes to a single file.

pub(crate) mod gui;

use std::{collections::BTreeMap, fmt};

use inlinable_string::InlinableString;
use sniff::MetaEntryDiff;

use crate::file::File;

bitflags::bitflags! {
    /// A description of all possible change combinations between two instances of the same file system entry.
    #[repr(transparent)]
    #[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, serde::Serialize, serde::Deserialize)]
    #[serde(try_from = "InlinableString")]
    #[serde(into = "InlinableString")]
    pub(crate) struct FsChanges: u16 {
        /// The entry was added.
        const ADDED = 1 << 0;
        /// The entry was deleted.
        const DELETED = 1 << 1;
        /// The content of the entry was changed.
        const CONTENT_CHANGED = 1 << 2;
        /// The size of the entry according to the metadata increased.
        const SIZE_INCREASE = 1 << 3;
        /// The size of the entry according to the metadata decreased.
        const SIZE_DECREASE = 1 << 4;
        /// The inode of the entry changed.
        const INODE_CHANGED = 1 << 5;
        /// The inode modification timestamp of the entry changed.
        const INODE_TIMESTAMP = 1 << 6;
        /// The created timestamp of the entry changed.
        const CREATED_TIMESTAMP = 1 << 7;
        /// The modified timestamp of the entry changed.
        const MODIFIED_TIMESTAMP = 1 << 8;
        /// The accessed timestamp of the entry changed.
        const ACCESSED_TIMESTAMP = 1 << 9;
    }
}

impl FsChanges {
    /// The number of possible changes between two instances of the same file system entry.
    pub(crate) const NUM_FIELDS: usize = 10;

    /// The characters used to represent each field.
    const FIELD_CHARS: [(char, FsChanges); Self::NUM_FIELDS] = [
        ('+', FsChanges::ADDED),
        ('-', FsChanges::DELETED),
        ('±', FsChanges::CONTENT_CHANGED),
        ('G', FsChanges::SIZE_INCREASE),
        ('s', FsChanges::SIZE_DECREASE),
        ('I', FsChanges::INODE_CHANGED),
        ('m', FsChanges::INODE_TIMESTAMP),
        ('C', FsChanges::CREATED_TIMESTAMP),
        ('M', FsChanges::MODIFIED_TIMESTAMP),
        ('A', FsChanges::ACCESSED_TIMESTAMP),
    ];

    /// The character used for a flag that is not present.
    const NON_PRESENT_CHAR: char = '.';

    /// Measures the changes for the given single diff.
    pub(crate) fn measure(changes: &Option<MetaEntryDiff<time::Duration>>) -> Option<Self> {
        let mut events = Self::empty();

        if let Some(changes) = changes {
            let meta = changes.meta_info();

            use sniff::{EntryDiff::*, MetaEntryDiff::*};

            match changes {
                Added(_) => {
                    events.insert(Self::ADDED);
                }
                Deleted(_) => {
                    events.insert(Self::DELETED);
                }
                MetaOnlyChange(_) => (),
                EntryChange(change, _) => match change {
                    FileChanged { .. } => events.insert(Self::CONTENT_CHANGED),
                    SymlinkChanged { .. } | TypeChange(_) | OtherChange => return None,
                },
            }

            if meta.inode.is_changed() {
                events.insert(Self::INODE_CHANGED);
            }
            match &meta.changes[..] {
                [sniff::MetadataChange::Size(change)] => match change.cmp() {
                    std::cmp::Ordering::Less => events.insert(Self::SIZE_INCREASE),
                    std::cmp::Ordering::Equal => (),
                    std::cmp::Ordering::Greater => events.insert(Self::SIZE_DECREASE),
                },
                [] => (),
                _ => return None,
            }
            if meta.inode_modified.is_changed() {
                events.insert(Self::INODE_TIMESTAMP);
            }
            if meta.created.is_changed() {
                events.insert(Self::CREATED_TIMESTAMP);
            }
            if meta.modified.is_changed() {
                events.insert(Self::MODIFIED_TIMESTAMP);
            }
            if meta.accessed.is_changed() {
                events.insert(Self::ACCESSED_TIMESTAMP);
            }
        }

        Some(events)
    }

    /// Returns `true` if the entry was added.
    pub(crate) fn added(&self) -> bool {
        self.contains(FsChanges::ADDED)
    }

    /// Returns `true` if the entry was deleted.
    pub(crate) fn deleted(&self) -> bool {
        self.contains(FsChanges::DELETED)
    }

    /// Returns `true` if the entry content was changed.
    pub(crate) fn content_changed(&self) -> bool {
        self.contains(FsChanges::CONTENT_CHANGED)
    }

    /// Returns `true` if the inode of the entry was changed.
    pub(crate) fn inode_changed(&self) -> bool {
        self.contains(FsChanges::INODE_CHANGED)
    }

    /// Returns `true` if the size of the entry increased.
    pub(crate) fn size_increased(&self) -> bool {
        self.contains(FsChanges::SIZE_INCREASE)
    }

    /// Returns `true` if the size of the entry decreased.
    pub(crate) fn size_decreased(&self) -> bool {
        self.contains(FsChanges::SIZE_DECREASE)
    }

    /// Returns `true` if the inode modification timestamp of the entry changed.
    pub(crate) fn inode_timestamp(&self) -> bool {
        self.contains(FsChanges::INODE_TIMESTAMP)
    }

    /// Returns `true` if the created timestamp of the entry changed.
    pub(crate) fn created_timestamp(&self) -> bool {
        self.contains(FsChanges::CREATED_TIMESTAMP)
    }

    /// Returns `true` if the modified timestamp of the entry changed.
    pub(crate) fn modified_timestamp(&self) -> bool {
        self.contains(FsChanges::MODIFIED_TIMESTAMP)
    }

    /// Returns `true` if the accessed timestamp of the entry changed.
    pub(crate) fn accessed_timestamp(&self) -> bool {
        self.contains(FsChanges::ACCESSED_TIMESTAMP)
    }

    /// Returns `true` if the size of the entry changed.
    pub(crate) fn size_changed(&self) -> bool {
        self.intersects(FsChanges::SIZE_INCREASE | FsChanges::SIZE_DECREASE)
    }
}

impl fmt::Display for FsChanges {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", InlinableString::from(*self))
    }
}

impl fmt::Debug for FsChanges {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", InlinableString::from(*self))
    }
}

impl From<FsChanges> for InlinableString {
    fn from(value: FsChanges) -> Self {
        FsChanges::FIELD_CHARS
            .into_iter()
            .map(|(c, field)| {
                if value.contains(field) {
                    c
                } else {
                    FsChanges::NON_PRESENT_CHAR
                }
            })
            .collect()
    }
}

impl TryFrom<InlinableString> for FsChanges {
    type Error = &'static str;

    fn try_from(value: InlinableString) -> Result<Self, Self::Error> {
        let mut iter = value.chars();
        let mut event = FsChanges::empty();

        for (expected, field) in FsChanges::FIELD_CHARS {
            let Some(c) = iter.next() else {
                return Err("input string is too short");
            };

            if c == expected {
                event.insert(field);
            } else if c != FsChanges::NON_PRESENT_CHAR {
                return Err("unexpected character in input string");
            }
        }

        if iter.next().is_some() {
            return Err("input string is too long");
        }

        Ok(event)
    }
}

/// A count of which file system change combinations occurred how often.
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
pub(crate) struct FsChangeCounts {
    /// Maps the file system change combinations to the count of how often the occurred.
    #[serde(flatten)]
    changes: BTreeMap<FsChanges, u32>,
}

impl FsChangeCounts {
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

            let event = FsChanges::measure(change)?;

            exists |= event.contains(FsChanges::ADDED);
            exists &= !event.contains(FsChanges::DELETED);

            // we only count this change if the file exists now or existed before
            if existed_before || exists {
                *changes.entry(event).or_default() += 1;
            }
        }

        if changes.is_empty() {
            return None;
        }

        Some(FsChangeCounts { changes })
    }

    /// Returns the same counts, but only with changes for which the filter returned `true`.
    pub(crate) fn filtered(&self, mut filter: impl FnMut(FsChanges, u32) -> bool) -> Self {
        Self {
            changes: self
                .iter()
                .filter(|&(changes, count)| filter(changes, count))
                .collect(),
        }
    }

    /// Returns the count for the given changes.
    pub(crate) fn get(&self, changes: FsChanges) -> Option<u32> {
        self.changes.get(&changes).copied()
    }

    /// Returns an iterator over all changes along with their counts.
    pub(crate) fn iter(&self) -> FsChangeCountsIter {
        FsChangeCountsIter {
            iter: self.changes.iter(),
        }
    }

    /// Computes the total number distinct observed changes.
    pub(crate) fn total_distinct_changes(&self) -> usize {
        self.changes.len()
    }

    /// Computes the total count of observed changes.
    pub(crate) fn total_count(&self) -> u32 {
        self.changes.values().sum()
    }
}

/// An iterator over change counts.
pub(crate) struct FsChangeCountsIter<'counts> {
    /// The internal iterator over the map.
    iter: std::collections::btree_map::Iter<'counts, FsChanges, u32>,
}

impl Iterator for FsChangeCountsIter<'_> {
    type Item = (FsChanges, u32);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(&changes, &count)| (changes, count))
    }
}

impl<'counts> IntoIterator for &'counts FsChangeCounts {
    type Item = (FsChanges, u32);

    type IntoIter = FsChangeCountsIter<'counts>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::FsChanges;

    #[test]
    fn fs_changes_formattting() {
        let events = [
            (FsChanges::all(), "+-±GsImCMA"),
            (
                FsChanges::MODIFIED_TIMESTAMP | FsChanges::ACCESSED_TIMESTAMP,
                "........MA",
            ),
            (
                FsChanges::CONTENT_CHANGED
                    | FsChanges::MODIFIED_TIMESTAMP
                    | FsChanges::ACCESSED_TIMESTAMP
                    | FsChanges::SIZE_INCREASE,
                "..±G....MA",
            ),
        ];

        for (event, expected) in events {
            let as_str = inlinable_string::InlinableString::from(event);

            assert_eq!(as_str, expected);
            assert_eq!(as_str, format!("{event}"));
            assert_eq!(as_str, format!("{event:?}"));
            assert_eq!(as_str, format!("{event:#?}"));

            assert_eq!(TryFrom::try_from(as_str), Ok(event));
        }
    }
}
