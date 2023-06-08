use inlinable_string::{InlinableString, StringExt as _};
use smallvec::SmallVec;

use crate::{file::Files, input::PathList, rules::RuleStorage};

use super::{case::Case, PathMatcher, PathMatcherPart};

/// A measurement for the viability of a matcher.
#[derive(Debug, Clone)]
struct MatcherViability {
    /// The number of matches in the selected path list.
    ///
    /// Ideally this should always be 1.
    selected_matches: usize,
    /// The number of path lists with exactly one match.
    one_match: usize,
    /// The number of path lists with more than one match.
    multi_match: usize,
    /// The number of static files matching in the input files.
    ///
    /// Static in this case refers to files which are never created or deleted in the input data.
    static_files_match: usize,
    /// The number of dynamic files matching in the input files.
    ///
    /// Dynamic in this case refers to files which are created or deleted at least once in the
    /// input data.
    dynamic_files_match: usize,
}

impl Ord for MatcherViability {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Reverse;

        /// Creates a tuple for both `self` and `other` that is then used for the comparison.
        ///
        /// The ordering here is lexicographic, so this first element has the most "weight" in the
        /// comparison and the last element has the least weight.
        macro_rules! tuple {
            ($val:ident) => {
                (
                    $val.selected_matches == 1,
                    $val.static_files_match <= 1,
                    $val.dynamic_files_match,
                    $val.one_match,
                    Reverse($val.multi_match),
                )
            };
        }

        tuple!(self).cmp(&tuple!(other))
    }
}

impl PartialOrd for MatcherViability {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for MatcherViability {}

impl PartialEq for MatcherViability {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}

impl MatcherViability {
    /// Computes the viability of the given matcher in the given environment.
    fn compute(
        matcher: &PathMatcher,
        files: &Files,
        selected_path_list: Option<&PathList>,
        path_lists: &[&PathList],
    ) -> Self {
        let selected_matches = selected_path_list
            .map(|list| list.matching_paths(matcher.clone()).len())
            .unwrap_or(1);

        let mut one_match = 0;
        let mut multi_match = 0;
        for list in path_lists {
            match list.matching_paths(matcher.clone()).len() {
                0 => (),
                1 => one_match += 1,
                _ => multi_match += 1,
            }
        }

        let mut static_files_match = 0;
        let mut dynamic_files_match = 0;
        for file in files.chronological_order().files() {
            if matcher.matches_file(file) {
                if file.changes.iter().any(|change| {
                    matches!(
                        change,
                        Some(sniff::MetaEntryDiff::Added(_) | sniff::MetaEntryDiff::Deleted(_))
                    )
                }) {
                    dynamic_files_match += 1;
                } else {
                    static_files_match += 1;
                }
            }
        }

        MatcherViability {
            selected_matches,
            one_match,
            multi_match,
            static_files_match,
            dynamic_files_match,
        }
    }
}

/// Computes the best variant of the matcher.
fn best_variant(
    path: &str,
    files: &Files,
    selected_path_list: Option<&PathList>,
    path_lists: &[&PathList],
) -> PathMatcher {
    let construct_partial = |parts: &[PathMatcherPart], lit: &str, part, rest: &str| {
        let mut result = SmallVec::from(parts);
        if !lit.is_empty() {
            result.push(PathMatcherPart::Literal(lit.into()));
        }
        result.push(part);
        result.push(PathMatcherPart::Literal(rest.into()));
        PathMatcher { parts: result }
    };

    let mut lit = InlinableString::new();
    let mut parts = SmallVec::new();
    let mut i = 0;

    let username = if let Some(maybe_user) = path.strip_prefix("/Users/") {
        let username: InlinableString = maybe_user.split('/').next().unwrap().into();

        parts.push(PathMatcherPart::Literal("/Users/".into()));
        parts.push(PathMatcherPart::Username);
        i += "/Users/".len() + username.len();

        Some(username)
    } else {
        None
    };

    while i < path.len() {
        let subpath = &path[i..];

        let leave_as_lit = {
            let mut result = parts.clone();
            result.push(PathMatcherPart::Literal(lit.clone() + subpath));
            PathMatcher { parts: result }
        };

        let mut max_part = None;
        let mut max_viability =
            MatcherViability::compute(&leave_as_lit, files, selected_path_list, path_lists);

        for (part, len) in possible_replacements(subpath, false, true, &username) {
            let matcher = construct_partial(&parts, &lit, part.clone(), &subpath[len..]);
            let viability =
                MatcherViability::compute(&matcher, files, selected_path_list, path_lists);
            if viability > max_viability {
                max_part = Some((part, len));
                max_viability = viability;
            }
        }

        if let Some((part, len)) = max_part {
            if !lit.is_empty() {
                parts.push(PathMatcherPart::Literal(lit));
                lit = InlinableString::new();
            }

            parts.push(part);
            i += len;
            continue;
        }

        lit.push(subpath.chars().next().unwrap());
        if let Some((char_width, _)) = subpath.char_indices().nth(1) {
            i += char_width;
        } else {
            i = path.len();
        }
    }

    if !lit.is_empty() {
        parts.push(PathMatcherPart::Literal(lit));
    }

    PathMatcher { parts }
}

/// Returns possible replacements for the given path part along with their matching length.
///
/// If `full_match` is `true`, the full path part must be matched by the replacements.
///
/// If `automatic` is `true`, then certain replacements that are only useful for manual operation
/// are not returned.
pub(crate) fn possible_replacements(
    path_part: &str,
    full_match: bool,
    automatic: bool,
    username: &Option<InlinableString>,
) -> Vec<(PathMatcherPart, usize)> {
    let mut possible_parts = vec![
        PathMatcherPart::Locale,
        PathMatcherPart::AssemblyVersion,
        PathMatcherPart::Uuid(Case::Upper),
        PathMatcherPart::Uuid(Case::Lower),
        PathMatcherPart::Digit {
            min_len: Some(1),
            max_len: None,
        },
        PathMatcherPart::HexDigit {
            case: Case::Upper,
            min_len: Some(1),
            max_len: None,
        },
        PathMatcherPart::HexDigit {
            case: Case::Lower,
            min_len: Some(1),
            max_len: None,
        },
    ];
    if !automatic {
        for case in [Case::Upper, Case::Lower, Case::Mixed] {
            for contains_digits in [false, true] {
                possible_parts.push(PathMatcherPart::AlphaOrAlphanumeric {
                    contains_digits,
                    case,
                    min_len: Some(1),
                    max_len: None,
                });
            }
        }
    }
    if username.is_some() {
        possible_parts.push(PathMatcherPart::Username);
    }

    let mut result = Vec::new();

    for part in possible_parts {
        let Some(match_len) = part.match_len(path_part, username) else { continue };
        if full_match && match_len != path_part.len() {
            continue;
        }

        result.extend(
            part.variants_with_match_len(match_len)
                .into_iter()
                .map(|part| (part, match_len)),
        );
    }

    result.sort_by_key(|(part, _)| part.flexibility());

    result
}

/// Suggests a path matcher.
pub(crate) fn suggest_matcher(
    files: &Files,
    rules: &RuleStorage,
    selected_path_list: Option<&PathList>,
    path_lists: &[&PathList],
    skip: usize,
) -> Option<PathMatcher> {
    files
        .alphabetical_order()
        .files()
        .filter(|file| !rules.is_matched(file))
        .nth(skip)
        .map(|file| best_variant(file.path(), files, selected_path_list, path_lists))
}
