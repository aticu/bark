//! Reads the input data into memory.

use std::ops::{Range, RangeFrom};

use sniff::{Changeset, MetaEntryDiff, Timestamp};

use crate::{file::ChangeTime, path_matcher::PathMatcher};

/// Reads the input from the given path.
pub(crate) fn read(path: impl AsRef<std::path::Path>) -> anyhow::Result<Vec<Changeset<Timestamp>>> {
    let path = path.as_ref();

    let mut buf = Vec::new();
    use std::io::Read as _;
    std::fs::File::open(path)?.read_to_end(&mut buf)?;

    if path.extension().map(|ext| ext == "json").unwrap_or(false) {
        let changeset = serde_json::from_slice(&buf)?;
        Ok(vec![changeset])
    } else {
        Ok(postcard::from_bytes(&buf)?)
    }
}

/// Computes the timestamp of the first change in the given changeset.
///
/// This is bounded by the `earliest_timestamp` of the changeset (adjusted by estimated timezone
/// difference).
/// Earlier changes are ignored.
pub(crate) fn compute_time_of_first_change(changeset: &Changeset<Timestamp>) -> Option<Timestamp> {
    let (diff_sum, n) = changeset
        .changes
        .values()
        .map(|diff| diff.meta_info())
        .filter_map(|meta| {
            [
                &meta.created,
                &meta.modified,
                &meta.accessed,
                &meta.inode_modified,
            ]
            .into_iter()
            .filter(|ts| ts.is_changed())
            .filter_map(|ts| ts.new_val().clone())
            .filter(|ts| ts > &changeset.earliest_timestamp)
            .min()
        })
        .fold((time::Duration::ZERO, 0), |(sum, n), ts| {
            (sum + (*ts - *changeset.earliest_timestamp), n + 1)
        });
    let avg_diff: time::Duration = diff_sum / n;
    let hour_offset =
        (avg_diff.as_seconds_f64() / time::Duration::HOUR.as_seconds_f64()).round() as i32;

    let tz_offset = time::Duration::HOUR * hour_offset;
    let earliest_timestamp = Timestamp::from(*changeset.earliest_timestamp + tz_offset);

    let time_of_first_change = changeset
        .changes
        .values()
        .map(|diff| diff.meta_info())
        .filter_map(|meta| {
            [
                &meta.created,
                &meta.modified,
                &meta.accessed,
                &meta.inode_modified,
            ]
            .into_iter()
            .filter(|ts| ts.is_changed())
            .filter_map(|ts| ts.new_val().clone())
            .filter(|ts| ts > &earliest_timestamp)
            .min()
        })
        .min()?;

    Some(time_of_first_change)
}

/// Computes the change time of a list of changes.
pub(crate) fn compute_change_time(
    changes: &[Option<MetaEntryDiff<time::Duration>>],
) -> Option<ChangeTime> {
    let mut total = time::Duration::ZERO;
    let mut n = 0;

    let max_ts = |change: &sniff::MetaEntryDiff<time::Duration>| {
        let meta = change.meta_info();
        [
            meta.created.new_val(),
            meta.modified.new_val(),
            meta.accessed.new_val(),
            meta.inode_modified.new_val(),
        ]
        .into_iter()
        .filter_map(|ts| *ts)
        .filter(|ts| ts.is_positive())
        .max()
    };

    for diff in changes {
        let Some(diff) = diff else { continue };
        let Some(ts) = max_ts(diff) else { continue };

        total += ts;
        n += 1;
    }

    if n == 0 {
        return None;
    }
    let avg = total / n;

    let mut std_dev_total = 0.0;

    for diff in changes {
        let Some(diff) = diff else { continue };
        let Some(ts) = max_ts(diff) else { continue };
        let ts_diff: time::Duration = avg - ts;

        std_dev_total += ts_diff.as_seconds_f64().powi(2);
    }

    let std_dev = time::Duration::seconds_f64((std_dev_total / n as f64).sqrt());

    Some(ChangeTime { avg, std_dev })
}

/// A list of paths.
pub(crate) struct PathList {
    /// The ordered list of paths not starting with `/Users/<username>`.
    ///
    /// The range should always be `0..` here.
    normal_paths: Vec<(String, RangeFrom<usize>)>,
    /// The paths starting with `/Users/<username>`.
    ///
    /// The range refers to the part of the string after the match.
    user_paths: Vec<(String, RangeFrom<usize>)>,
}

impl PathList {
    /// Creates a new path list from the given list.
    pub(crate) fn new(mut list: Vec<String>) -> Self {
        let mut normal_paths = Vec::new();
        let mut user_paths = Vec::new();

        list.sort_unstable();

        let user_matcher = PathMatcher::from("/Users/<username>");
        for path in list {
            if let Some(len) = user_matcher.match_len(&path) {
                user_paths.push((path, (len..)));
            } else {
                normal_paths.push((path, (0..)));
            }
        }

        normal_paths.sort_by(|(path1, _), (path2, _)| path1.cmp(path2));
        user_paths.sort_by(|(path1, _), (path2, _)| path1.cmp(path2));

        PathList {
            normal_paths,
            user_paths,
        }
    }

    /// Returns all paths matching the given matcher.
    pub(crate) fn matching_paths(&self, matcher: PathMatcher) -> Vec<&str> {
        let user_matcher = PathMatcher::from("/Users/<username>");
        let full_matcher = matcher.clone();

        let (list, matcher) = if let Some(after_user_matcher) = matcher.strip_prefix(&user_matcher)
        {
            (&self.user_paths, after_user_matcher)
        } else {
            (&self.normal_paths, matcher)
        };

        let prefix = matcher.literal_prefix();
        let range = search_sorted_list(
            list,
            &prefix.as_ref(),
            |(path, range)| &path[range.clone()],
            |(path, range)| path[range.clone()].starts_with(&*prefix),
        );

        list[range]
            .iter()
            .filter(|(path, _)| full_matcher.matches_path(path))
            .map(|(path, _)| path.as_str())
            .collect()
    }
}

/// Searches the given sorted list for a section of needles, returning the beginning and end of it.
///
/// This function assumes that all needles will be sorted together in the list, the list looks like
/// this:
///
/// ```
/// [s_1, ..., s_i, n_1, ..., n_j, g_1, ..., g_k]
/// ```
///
/// where `s_1`, ..., `s_i` are the elements comparing smaller than the needles, `n_1`, ..., `n_j`
/// are the needles and `g_1`, ..., `g_k` are the elements comparing greater than the needles.
///
/// The result would then be `i..i + j` in this case.
fn search_sorted_list<'a, T: 'a, B: Ord + 'a>(
    list: &'a [T],
    needle: &'a B,
    map_to_needle: impl FnMut(&'a T) -> B,
    mut is_needle: impl FnMut(&T) -> bool,
) -> Range<usize> {
    // the paths are sorted, so we can do binary search for paths
    // where at least the literal prefix of the glob matches to speed
    // up checking paths
    let (Ok(middle_idx) | Err(middle_idx)) = list.binary_search_by_key(needle, map_to_needle);

    // the binary search just returns any index into the middle
    // part
    // now we use `partition_point` to find the edges of the part
    let start = list[..middle_idx].partition_point(|elem| !is_needle(elem));
    let end = middle_idx + list[middle_idx..].partition_point(|elem| is_needle(elem));

    start..end
}
