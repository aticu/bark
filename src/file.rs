//! Implements information about a single file across possibly multiple runs.

use std::fmt;

use smallvec::SmallVec;
use sniff::MetaEntryDiff;

use crate::input;

/// Information about timestamp changes for a single file.
#[derive(Debug, Default)]
pub(crate) struct ChangeTime {
    /// The average timestamp since the boot.
    pub(crate) avg: time::Duration,
    /// The standard deviation of the change time since boot, if calculation requested.
    pub(crate) std_dev: time::Duration,
}

impl PartialEq for ChangeTime {
    fn eq(&self, other: &Self) -> bool {
        self.avg.eq(&other.avg)
    }
}

impl Eq for ChangeTime {}

impl PartialOrd for ChangeTime {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.avg.partial_cmp(&other.avg)
    }
}

impl Ord for ChangeTime {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.avg.cmp(&other.avg)
    }
}

impl fmt::Display for ChangeTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let avg = self.avg.as_seconds_f64();
        if avg >= 10000.0 {
            write!(f, "long")?;
        } else if avg >= 100.0 {
            write!(f, "{:4.0}", avg)?;
        } else if avg >= 10.0 {
            write!(f, "{:4.1}", avg)?;
        } else {
            write!(f, "{:4.2}", avg)?;
        }

        write!(f, "±")?;

        let std_dev = self.std_dev.as_seconds_f64();
        if std_dev >= 1000.0 {
            write!(f, "BIG",)?;
        } else if std_dev >= 10.0 {
            write!(f, "{:3.0}", std_dev)?;
        } else {
            write!(f, "{:3.1}", std_dev)?;
        }

        write!(f, "s")
    }
}

/// Represents a single file and its changes across possibly multiple runs.
#[derive(Debug)]
pub(crate) struct File {
    /// The paths that point to this file.
    ///
    /// Two paths are considered the same if their diffs (including) inodes are all the same.
    pub(crate) paths: SmallVec<[String; 1]>,
    /// All diffs recorded of this file.
    ///
    /// At least one of these will be `Some(_)`.
    pub(crate) changes: SmallVec<[Option<MetaEntryDiff<time::Duration>>; 1]>,
    /// The average time since boot of the latest change to this file.
    ///
    /// This could be computed from the changes, but is stored here for efficiency and simplicity.
    pub(crate) change_time: Option<ChangeTime>,
}

impl File {
    /// Returns a representative path for the file.
    ///
    /// The current implementation returns the shortest path.
    pub(crate) fn path(&self) -> &str {
        // the unwrap here is safe since every file must have at least one path
        self.paths.iter().min_by_key(|path| path.len()).unwrap()
    }
}

/// An index to reference a path within the `Files` struct.
#[derive(Debug)]
pub(crate) struct PathIdx {
    /// The index into the file list.
    file_idx: usize,
    /// The index into the path list within the file.
    path_idx: usize,
}

/// Represents a collection of files.
#[derive(Debug)]
pub(crate) struct Files {
    /// The list of files.
    files: Vec<File>,
    /// The indexes of the files in alphabetical order.
    ///
    /// Note that since files may have multiple paths, there could be more than one entry for each
    /// file, thus making this vector longer than the file list and the chronological order.
    alphabetical_order: Vec<PathIdx>,
    /// The indexes of the files in chronological order.
    chronological_order: Vec<usize>,
}

impl Files {
    /// Transforms the changesets into the `Files`.
    pub(crate) fn from_changesets(changesets: &[sniff::Changeset<sniff::Timestamp>]) -> Files {
        let mut path_changes = std::collections::BTreeMap::new();

        // first gather all changes for each path together and transform the timestamps
        for (i, changeset) in changesets.iter().enumerate() {
            let time_of_first_change = input::compute_time_of_first_change(changeset)
                .unwrap_or_else(|| time::OffsetDateTime::UNIX_EPOCH.into());

            for (path, change) in &changeset.changes {
                path_changes
                    .entry(path.as_str())
                    .or_insert_with(|| smallvec::smallvec![None; changesets.len()])[i] =
                    Some(change.transform_timestamps(|ts| **ts - *time_of_first_change));
            }
        }

        let mut files = Vec::<File>::new();
        let mut alphabetical_order = Vec::new();
        let mut chronological_order = Vec::<usize>::new();

        // this allows de-duplication of files that have multiple paths
        let mut changes_to_file_idx = std::collections::HashMap::<_, usize>::new();

        for (path, changes) in path_changes {
            let idx = if let Some(&idx) = changes_to_file_idx.get(&changes) {
                files[idx].paths.push(path.to_string());

                idx
            } else {
                let idx = files.len();

                let change_time = input::compute_change_time(&changes);
                let chron_idx = chronological_order
                    .partition_point(|&idx| files[idx].change_time <= change_time);
                chronological_order.insert(chron_idx, idx);

                files.push(File {
                    paths: smallvec::smallvec![path.to_string()],
                    changes: changes.clone(),
                    change_time,
                });

                changes_to_file_idx.insert(changes, idx);

                idx
            };

            // the paths are already sorted, because we're iterating over a `BTreeMap`
            alphabetical_order.push(crate::file::PathIdx {
                file_idx: idx,
                path_idx: files[idx].paths.len() - 1,
            })
        }

        Files {
            files,
            alphabetical_order,
            chronological_order,
        }
    }

    /// Iterates over the files in chronological order.
    pub(crate) fn chronological_order(&self) -> FileIter {
        self.iter(FileOrder::Chronological)
    }

    /// Iterates over the files with the given order.
    pub(crate) fn iter(&self, order_type: FileOrder) -> FileIter {
        match order_type {
            FileOrder::Alphabetical => FileIter::Alphabetical {
                files: self,
                iter: self.alphabetical_order.iter(),
            },
            FileOrder::Chronological => FileIter::Chronological {
                files: self,
                iter: self.chronological_order.iter(),
            },
        }
    }
}

/// The different available sorting orders for files.
#[derive(Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub(crate) enum FileOrder {
    /// The files are sorted alphabetically.
    ///
    /// A file will be visited once for each path that points to it.
    Alphabetical,
    /// The files are sorted chronologically.
    ///
    /// Each file will be visited only once.
    #[default]
    Chronological,
}

/// An iterator over all files.
pub(crate) enum FileIter<'files> {
    /// An iterator in alphabetical order.
    ///
    /// A file will be visited once for each path that points to it.
    Alphabetical {
        /// The files being iterated over.
        files: &'files Files,
        /// The iterator over the file indices.
        iter: std::slice::Iter<'files, PathIdx>,
    },
    /// An iterator in chronological order.
    ///
    /// Each file will be visited only once.
    Chronological {
        /// The files being iterated over.
        files: &'files Files,
        /// The iterator over the file indices.
        iter: std::slice::Iter<'files, usize>,
    },
}

impl<'files> Iterator for FileIter<'files> {
    type Item = (&'files str, &'files File);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Alphabetical { files, iter } => iter.next().map(|idx| {
                let file = &files.files[idx.file_idx];

                (file.paths[idx.path_idx].as_str(), file)
            }),
            Self::Chronological { files, iter } => iter.next().map(|&idx| {
                let file = &files.files[idx];

                (file.path(), file)
            }),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Alphabetical { iter, .. } => iter.size_hint(),
            Self::Chronological { iter, .. } => iter.size_hint(),
        }
    }

    fn count(self) -> usize
    where
        Self: Sized,
    {
        match self {
            Self::Alphabetical { iter, .. } => iter.count(),
            Self::Chronological { iter, .. } => iter.count(),
        }
    }

    fn last(self) -> Option<Self::Item> {
        match self {
            Self::Alphabetical { files, iter } => iter.last().map(|idx| {
                let file = &files.files[idx.file_idx];

                (file.paths[idx.path_idx].as_str(), file)
            }),
            Self::Chronological { files, iter } => iter.last().map(|&idx| {
                let file = &files.files[idx];

                (file.path(), file)
            }),
        }
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        match self {
            Self::Alphabetical { files, iter } => iter.nth(n).map(|idx| {
                let file = &files.files[idx.file_idx];

                (file.paths[idx.path_idx].as_str(), file)
            }),
            Self::Chronological { files, iter } => iter.nth(n).map(|&idx| {
                let file = &files.files[idx];

                (file.path(), file)
            }),
        }
    }
}
