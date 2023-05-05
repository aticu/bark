//! Implements information about a single file across possibly multiple runs.

use smallvec::SmallVec;
use sniff::MetaEntryDiff;

/// Represents a single file and its changes across possibly multiple runs.
pub(crate) struct File {
    /// The paths that point to this file.
    ///
    /// Two paths are considered the same if their diffs (including) inodes are all the same.
    pub(crate) paths: SmallVec<[String; 1]>,
    /// All diffs recorded of this file.
    ///
    /// At least one of these fill be `Some(_)`.
    pub(crate) changes: SmallVec<[Option<MetaEntryDiff<time::Duration>>; 1]>,
    /// The average time since boot of the latest change to this file.
    ///
    /// This could be computed from the changes, but is stored here for efficiency and simplicity.
    pub(crate) change_time: super::ChangeTime,
}
