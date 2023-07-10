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
///
/// The range is the substring of the path that comes after the corresponding matcher for this
/// inner list matches.
type InnerPathList = Vec<(String, RangeFrom<usize>)>;

/// A list of paths.
pub(crate) struct PathList {
    /// The sub-path lists separated by prefixes in the form of matchers.
    ///
    /// For each matcher there will be a list of paths where the matcher matches a prefix of that
    /// path.
    /// Each part is also accompanied by the range of the path that is not matched by the prefix
    /// matcher.
    /// The list is sorted by that part of the path.
    path_lists: Vec<(PathMatcher, InnerPathList)>,
}

impl PathList {
    /// Creates a new path list from the given list.
    pub(crate) fn new(mut list: Vec<String>) -> Self {
        list.sort();

        // Since the matchers are tried in order, it's important to order them from most specific
        // to least specific.
        let matchers = [
            PathMatcher::from("/Users/<username>"),
            PathMatcher::from("/Windows/servicing/<locale>"),
            PathMatcher::from(""),
        ];

        let mut list_map = std::collections::HashMap::new();

        for matcher in &matchers {
            list_map.insert(matcher, Vec::new());
        }

        for path in list {
            for matcher in &matchers {
                if let Some(len) = matcher.match_len(&path) {
                    list_map.get_mut(matcher).unwrap().push((path, len..));
                    break;
                }
            }
        }

        // use this method instead of a simple collect to ensure that the order is correct
        let mut path_lists = Vec::new();
        for matcher in &matchers {
            path_lists.push((matcher.clone(), list_map.remove(matcher).unwrap()));
        }

        PathList { path_lists }
    }

    /// Returns all paths matching the given matcher.
    pub(crate) fn matching_paths(&self, matcher: PathMatcher) -> impl Iterator<Item = &str> {
        for (list_matcher, list) in &self.path_lists {
            let Some(after_prefix_matcher) = matcher.strip_prefix(list_matcher) else { continue };

            let needle = after_prefix_matcher.literal_prefix();
            let range = search_sorted_list(
                &list[..],
                &needle.as_ref(),
                |(path, range)| &path[range.clone()],
                |(path, range)| path[range.clone()].starts_with(&*needle),
            );

            return list[range]
                .iter()
                .filter(move |(path, _)| matcher.matches_path(path))
                .map(|(path, _)| path.as_str());
        }

        unreachable!("there should always be the empty matcher in the list")
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
/// The `seed_needle` argument can be any element which compares within the needle region, but it
/// may or may not be present within the region itself.
///
/// The result would then be `i..i + j` in this case.
fn search_sorted_list<'a, T: 'a, Needle: Ord + 'a>(
    list: &'a [T],
    seed_needle: &'a Needle,
    map_to_needle: impl FnMut(&'a T) -> Needle,
    mut is_needle: impl FnMut(&T) -> bool,
) -> Range<usize> {
    // the paths are sorted, so we can do binary search for paths
    // where at least the literal prefix of the glob matches to speed
    // up checking paths
    let (Ok(middle_idx) | Err(middle_idx)) = list.binary_search_by_key(seed_needle, map_to_needle);

    // the binary search just returns any index into the middle
    // part
    // now we use `partition_point` to find the edges of the part
    let start = list[..middle_idx].partition_point(|elem| !is_needle(elem));
    let end = middle_idx + list[middle_idx..].partition_point(|elem| is_needle(elem));

    start..end
}
