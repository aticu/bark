//! Reads the input data into memory.

use sniff::{Changeset, Timestamp};

/// Reads the input from the given path.
pub(crate) fn read_many(
    path: impl AsRef<std::path::Path>,
) -> anyhow::Result<Vec<Changeset<Timestamp>>> {
    let path = path.as_ref();

    let mut buf = Vec::new();
    use std::io::Read as _;
    std::fs::File::open(path)?.read_to_end(&mut buf)?;

    if path.extension().map(|ext| ext == "json").unwrap_or(false) {
        Ok(serde_json::from_slice(&buf)?)
    } else {
        Ok(postcard::from_bytes(&buf)?)
    }
}

/// Transforms the given changeset to relative time since the first change.
///
/// The time of the first change is returned, along with the updated changeset.
pub(crate) fn transform_to_relative_time(
    changeset: &Changeset<Timestamp>,
) -> Option<(Timestamp, Changeset<time::Duration>)> {
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

    let new_changeset = changeset.transform_timestamps(|ts| **ts - *time_of_first_change);

    Some((time_of_first_change, new_changeset))
}
