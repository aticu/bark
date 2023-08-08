//! Defines the change event type, used to describe changes to a single file.

use std::fmt;

use inlinable_string::InlinableString;
use sniff::MetaEntryDiff;

bitflags::bitflags! {
    /// A description of a single difference between two instances of a file.
    #[repr(transparent)]
    #[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, serde::Serialize, serde::Deserialize)]
    #[serde(try_from = "InlinableString")]
    #[serde(into = "InlinableString")]
    pub(crate) struct ChangeEvent: u16 {
        /// The file was added.
        const ADDED = 1 << 0;
        /// The file was deleted.
        const DELETED = 1 << 1;
        /// The content of the file was changed.
        const CONTENT_CHANGED = 1 << 2;
        /// The size according to the metadata increased.
        const SIZE_INCREASE = 1 << 3;
        /// The size according to the metadata decreased.
        const SIZE_DECREASE = 1 << 4;
        /// The inode of the file changed.
        const INODE_CHANGED = 1 << 5;
        /// The inode modification timestamp of the file changed.
        const INODE_TIMESTAMP = 1 << 6;
        /// The created timestamp of the file changed.
        const CREATED_TIMESTAMP = 1 << 7;
        /// The modified timestamp of the file changed.
        const MODIFIED_TIMESTAMP = 1 << 8;
        /// The accessed timestamp of the file changed.
        const ACCESSED_TIMESTAMP = 1 << 9;
    }
}

impl ChangeEvent {
    /// The number of files that are present in a change event.
    pub(crate) const NUM_FIELDS: usize = 10;

    /// The characters used to represent each field.
    const FIELD_CHARS: [(char, ChangeEvent); Self::NUM_FIELDS] = [
        ('+', ChangeEvent::ADDED),
        ('-', ChangeEvent::DELETED),
        ('±', ChangeEvent::CONTENT_CHANGED),
        ('G', ChangeEvent::SIZE_INCREASE),
        ('s', ChangeEvent::SIZE_DECREASE),
        ('I', ChangeEvent::INODE_CHANGED),
        ('m', ChangeEvent::INODE_TIMESTAMP),
        ('C', ChangeEvent::CREATED_TIMESTAMP),
        ('M', ChangeEvent::MODIFIED_TIMESTAMP),
        ('A', ChangeEvent::ACCESSED_TIMESTAMP),
    ];

    /// The character used for a flag that is not present.
    const NON_PRESENT_CHAR: char = '.';

    /// Measures the changes for the given single change.
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

    /// Returns `true` if the file was added.
    pub(crate) fn added(&self) -> bool {
        self.contains(ChangeEvent::ADDED)
    }

    /// Returns `true` if the file was deleted.
    pub(crate) fn deleted(&self) -> bool {
        self.contains(ChangeEvent::DELETED)
    }

    /// Returns `true` if the file content was changed.
    pub(crate) fn content_changed(&self) -> bool {
        self.contains(ChangeEvent::CONTENT_CHANGED)
    }

    /// Returns `true` if the inode was changed.
    pub(crate) fn inode_changed(&self) -> bool {
        self.contains(ChangeEvent::INODE_CHANGED)
    }

    /// Returns `true` if the size increased.
    pub(crate) fn size_increased(&self) -> bool {
        self.contains(ChangeEvent::SIZE_INCREASE)
    }

    /// Returns `true` if the size decreased.
    pub(crate) fn size_decreased(&self) -> bool {
        self.contains(ChangeEvent::SIZE_DECREASE)
    }

    /// Returns `true` if the inode modification timestamp changed.
    pub(crate) fn inode_timestamp(&self) -> bool {
        self.contains(ChangeEvent::INODE_TIMESTAMP)
    }

    /// Returns `true` if the created timestamp changed.
    pub(crate) fn created_timestamp(&self) -> bool {
        self.contains(ChangeEvent::CREATED_TIMESTAMP)
    }

    /// Returns `true` if the modified timestamp changed.
    pub(crate) fn modified_timestamp(&self) -> bool {
        self.contains(ChangeEvent::MODIFIED_TIMESTAMP)
    }

    /// Returns `true` if the accessed timestamp changed.
    pub(crate) fn accessed_timestamp(&self) -> bool {
        self.contains(ChangeEvent::ACCESSED_TIMESTAMP)
    }

    /// Returns `true` if the size changed.
    pub(crate) fn size_changed(&self) -> bool {
        self.intersects(ChangeEvent::SIZE_INCREASE | ChangeEvent::SIZE_DECREASE)
    }
}

impl fmt::Display for ChangeEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", InlinableString::from(*self))
    }
}

impl fmt::Debug for ChangeEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", InlinableString::from(*self))
    }
}

impl From<ChangeEvent> for InlinableString {
    fn from(value: ChangeEvent) -> Self {
        ChangeEvent::FIELD_CHARS
            .into_iter()
            .map(|(c, field)| {
                if value.contains(field) {
                    c
                } else {
                    ChangeEvent::NON_PRESENT_CHAR
                }
            })
            .collect()
    }
}

impl TryFrom<InlinableString> for ChangeEvent {
    type Error = &'static str;

    fn try_from(value: InlinableString) -> Result<Self, Self::Error> {
        let mut iter = value.chars();
        let mut event = ChangeEvent::empty();

        for (expected, field) in ChangeEvent::FIELD_CHARS {
            let Some(c) = iter.next() else { return Err("input string is too short") };

            if c == expected {
                event.insert(field);
            } else if c != ChangeEvent::NON_PRESENT_CHAR {
                return Err("unexpected character in input string");
            }
        }

        if iter.next().is_some() {
            return Err("input string is too long");
        }

        Ok(event)
    }
}

#[cfg(test)]
mod tests {
    use super::ChangeEvent;

    #[test]
    fn change_event_formatting() {
        let events = [
            (ChangeEvent::all(), "+-±GsImCMA"),
            (
                ChangeEvent::MODIFIED_TIMESTAMP | ChangeEvent::ACCESSED_TIMESTAMP,
                "........MA",
            ),
            (
                ChangeEvent::CONTENT_CHANGED
                    | ChangeEvent::MODIFIED_TIMESTAMP
                    | ChangeEvent::ACCESSED_TIMESTAMP
                    | ChangeEvent::SIZE_INCREASE,
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
