//! Reads the input data into memory.

use sniff::{Changeset, MetaEntryDiff, Timestamp};

use crate::file::{ChangeTime, File, Files};

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
