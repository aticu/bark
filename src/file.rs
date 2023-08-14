//! Implements information about a single file across possibly multiple runs.

use std::{collections::HashMap, fmt};

use inlinable_string::InlinableString;
use radix_trie::{Trie, TrieCommon};
use smallvec::SmallVec;
use sniff::MetaEntryDiff;

use crate::{future_value::ComputableValue, input, path_matcher::PathMatcher, rules::RuleStorage};

/// Information about timestamp changes for a single file.
#[derive(Debug, Default, PartialEq, Eq, Hash)]
pub(crate) struct ChangeTime {
    /// The average timestamp since the boot.
    pub(crate) avg: time::Duration,
    /// The standard deviation of the change time since boot, if calculation requested.
    pub(crate) std_dev: time::Duration,
}

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
#[derive(Debug, Hash)]
pub(crate) struct File {
    /// The paths that point to this file.
    ///
    /// Two paths are considered the same if their diffs (including) inodes are all the same.
    pub(crate) paths: SmallVec<[Box<str>; 1]>,
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
#[derive(Debug, Clone, Copy)]
pub(crate) struct PathIdx {
    /// The index into the file list.
    file_idx: usize,
    /// The index into the path list within the file.
    path_idx: usize,
}

/// The ID of a file.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub(crate) struct FileId {
    /// The index into the file list.
    index: usize,
}

/// A cache for the computed match scores of files.
pub(crate) struct FileScoreCache {
    /// The match scores of files, sorted by
    match_scores: HashMap<FileId, Option<f64>>,
    /// The version of the rule storage that this filter was generated with.
    rule_version: u64,
}

impl FileScoreCache {
    /// Gets the cached match score for the given file id.
    pub(crate) fn get_score(&self, id: FileId) -> Option<Option<f64>> {
        self.match_scores.get(&id).copied()
    }
}

impl ComputableValue for FileScoreCache {
    type CheckCtx<'check> = &'check RuleStorage;

    type ComputeCtx = (RuleStorage, &'static Files);

    fn is_current(&self, ctx: Self::CheckCtx<'_>) -> bool {
        self.rule_version == ctx.version()
    }

    fn compute(ctx: Self::ComputeCtx) -> Self {
        let (storage, files) = &ctx;

        Self {
            match_scores: files
                .chronological_order()
                .map(|file| (file.file_id, storage.match_score(file.file)))
                .collect(),
            rule_version: storage.version(),
        }
    }
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
    /// The trie of all paths in the list.
    path_trie: Trie<String, ()>,
    /// The number of changes contained within each file.
    width: usize,
    /// The ID of this data source.
    ///
    /// With a very high probability this will be different for each data source.
    datasource_id: DatasourceId,
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
        let mut path_trie = Trie::new();

        // this allows de-duplication of files that have multiple paths
        let mut changes_to_file_idx = std::collections::HashMap::<_, usize>::new();

        for (path, changes) in path_changes {
            let idx = if let Some(&idx) = changes_to_file_idx.get(&changes) {
                files[idx].paths.push(path.into());

                idx
            } else {
                let idx = files.len();

                let change_time = input::compute_change_time(&changes);
                let chron_idx = chronological_order
                    .partition_point(|&idx| files[idx].change_time <= change_time);
                chronological_order.insert(chron_idx, idx);

                files.push(File {
                    paths: smallvec::smallvec![path.into()],
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
            });

            path_trie.insert(path.to_string(), ());
        }

        let width = files.get(0).map(|file| file.changes.len()).unwrap_or(0);

        let mut hasher = hashers::fnv::FNV1aHasher64::default();
        std::hash::Hash::hash(&files, &mut hasher);
        let datasource_id = DatasourceId(std::hash::Hasher::finish(&hasher));

        Files {
            files,
            alphabetical_order,
            chronological_order,
            path_trie,
            width,
            datasource_id,
        }
    }

    /// The number of changes contained within each file.
    pub(crate) fn width(&self) -> usize {
        self.width
    }

    /// Returns the file for the given `id`.
    pub(crate) fn get(&self, id: FileId) -> Option<&File> {
        self.files.get(id.index)
    }

    /// The ID of this data source.
    ///
    /// With a very high probability this will be different for each data source.
    pub(crate) fn datasource_id(&self) -> DatasourceId {
        self.datasource_id
    }

    /// Iterates over the files in chronological order.
    pub(crate) fn chronological_order(&self) -> FileIter {
        self.iter(FileOrder::Chronological)
    }

    /// Iterates over the files in alphabetical order.
    pub(crate) fn alphabetical_order(&self) -> FileIter {
        self.iter(FileOrder::Alphabetical)
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

    /// The number of paths that the given path matcher matches.
    pub(crate) fn match_count(&self, path_matcher: &PathMatcher) -> usize {
        if let Some(subtrie) = self.path_trie.get_ancestor(&*path_matcher.literal_prefix()) {
            subtrie
                .keys()
                .filter(|path| path_matcher.matches_path(path))
                .count()
        } else {
            self.path_trie
                .keys()
                .filter(|path| path_matcher.matches_path(path))
                .count()
        }
    }
}

/// The different available sorting orders for files.
#[derive(Default, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
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

/// Resolves an alphabetical index.
fn resolve_alphabetical_idx(idx: PathIdx, files: &Files) -> IteratedFile {
    let file = &files.files[idx.file_idx];

    IteratedFile {
        file_id: FileId {
            index: idx.file_idx,
        },
        path: &file.paths[idx.path_idx],
        file,
    }
}

/// Resolves a chronological index.
fn resolve_chronological_idx(idx: usize, files: &Files) -> IteratedFile {
    let file = &files.files[idx];

    IteratedFile {
        file_id: FileId { index: idx },
        path: file.path(),
        file,
    }
}

impl<'files> Iterator for FileIter<'files> {
    type Item = IteratedFile<'files>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Alphabetical { files, iter } => {
                iter.next().map(|&idx| resolve_alphabetical_idx(idx, files))
            }
            Self::Chronological { files, iter } => iter
                .next()
                .map(|&idx| resolve_chronological_idx(idx, files)),
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
            Self::Alphabetical { files, iter } => {
                iter.last().map(|&idx| resolve_alphabetical_idx(idx, files))
            }
            Self::Chronological { files, iter } => iter
                .last()
                .map(|&idx| resolve_chronological_idx(idx, files)),
        }
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        match self {
            Self::Alphabetical { files, iter } => {
                iter.nth(n).map(|&idx| resolve_alphabetical_idx(idx, files))
            }
            Self::Chronological { files, iter } => iter
                .nth(n)
                .map(|&idx| resolve_chronological_idx(idx, files)),
        }
    }
}

impl<'files> FileIter<'files> {
    /// Returns just the files of this file iterator.
    pub(crate) fn files(self) -> impl Iterator<Item = &'files File> {
        self.map(|iter_file| iter_file.file)
    }
}

/// The result of iterating over files.
pub(crate) struct IteratedFile<'files> {
    /// The id of the returned file.
    pub(crate) file_id: FileId,
    /// The current path of the returned file.
    pub(crate) path: &'files str,
    /// The file itself.
    pub(crate) file: &'files File,
}

/// The ID of a data source.
#[derive(
    Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
#[serde(try_from = "InlinableString")]
#[serde(into = "InlinableString")]
pub(crate) struct DatasourceId(u64);

impl fmt::Display for DatasourceId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:08x}", self.0)
    }
}

impl TryFrom<InlinableString> for DatasourceId {
    type Error = std::num::ParseIntError;

    fn try_from(value: InlinableString) -> Result<Self, Self::Error> {
        u64::from_str_radix(&value, 16).map(DatasourceId)
    }
}

impl From<DatasourceId> for InlinableString {
    fn from(value: DatasourceId) -> Self {
        format!("{value}").into()
    }
}
