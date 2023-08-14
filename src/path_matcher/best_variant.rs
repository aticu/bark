use std::{cmp::Ordering, collections::BTreeSet};

use inlinable_string::{InlinableString, StringExt as _};

use crate::{
    file::Files,
    input::PathList,
    rules::{Rule, RuleStorage},
};

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
    /// The total number of all matches anywhere.
    total_matches: usize,
}

impl MatcherViability {
    /// Compares the viability of two matches, taking into account whether strict rules should
    /// apply.
    fn cmp(&self, other: &Self, strict: bool) -> Ordering {
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
                    $val.one_match + $val.multi_match,
                    Reverse($val.multi_match),
                )
            };
            (allow_multi: $val:ident) => {
                (
                    $val.selected_matches >= 1,
                    $val.total_matches,
                    Reverse($val.selected_matches),
                    Reverse($val.multi_match),
                )
            };
        }

        if strict {
            tuple!(self).cmp(&tuple!(other))
        } else {
            tuple!(allow_multi: self).cmp(&tuple!(allow_multi: other))
        }
    }

    /// Computes the viability of the given matcher in the given environment.
    fn compute(
        matcher: &PathMatcher,
        files: &Files,
        selected_path_list: Option<&PathList>,
        path_lists: &[&PathList],
    ) -> Self {
        let selected_matches = selected_path_list
            .map(|list| list.matching_paths(matcher.clone()).take(2).count())
            .unwrap_or(1);

        let mut total_matches = selected_matches;
        let mut one_match = 0;
        let mut multi_match = 0;
        for list in path_lists {
            match list.matching_paths(matcher.clone()).take(2).count() {
                0 => (),
                1 => {
                    one_match += 1;
                    total_matches += 1;
                }
                n => {
                    multi_match += 1;
                    total_matches += n;
                }
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

        total_matches += static_files_match;
        total_matches += dynamic_files_match;

        MatcherViability {
            selected_matches,
            one_match,
            multi_match,
            static_files_match,
            dynamic_files_match,
            total_matches,
        }
    }
}

/// Finds possible replacements for a given path.
fn find_replacements(path: &str) -> Vec<Vec<PathMatcherPart>> {
    let mut replacements = Vec::new();
    let mut i = 0;

    let username = if let Some(maybe_user) = path.strip_prefix("/Users/") {
        let username: InlinableString = maybe_user.split('/').next().unwrap().into();

        if PathMatcher::from("<username>").matches_path(&username) {
            replacements.push(vec![PathMatcherPart::Literal("/Users/".into())]);
            replacements.push(vec![PathMatcherPart::Username]);
            i += "/Users/".len() + username.len();

            Some(username)
        } else {
            None
        }
    } else {
        None
    };

    let mut lit_start = i;
    while i < path.len() {
        let prefix = &path[..i];
        let subpath = &path[i..];

        // we haven't found good replacements starting at the current position, so search at the
        // next one
        macro_rules! bail {
            () => {
                i += subpath
                    .char_indices()
                    .nth(1)
                    .map(|(char_width, _)| char_width)
                    .unwrap_or(path.len() - i);

                continue;
            };
        }

        let possible_replacements = possible_replacements(subpath, false, true, &username);
        if possible_replacements.is_empty() {
            bail!();
        }

        let len = possible_replacements
            .iter()
            .map(|(_, len)| *len)
            .max()
            .unwrap();

        let prev_char_is_text = prefix
            .chars()
            .last()
            .map(|c| c.is_ascii_alphabetic())
            .unwrap_or(false);
        let next_char_is_text = path[i + len..]
            .chars()
            .next()
            .map(|c| c.is_ascii_alphabetic())
            .unwrap_or(false);

        let bordered_by_alpha = prev_char_is_text || next_char_is_text;

        let mut possible_parts: Vec<_> = possible_replacements
            .into_iter()
            .filter(|(_, part_len)| *part_len == len)
            .filter(|(part, len)| {
                // a 2-letter locale that is bordered by other alphabetic characters is very likely not a locale
                !(bordered_by_alpha && matches!(part, PathMatcherPart::Locale(_)) && *len == 2)
            })
            .filter(|(part, _)| {
                // hex digits bordered by other alphabetic characters are likely not hex digits
                !(bordered_by_alpha && matches!(part, PathMatcherPart::HexDigit { .. }))
            })
            .map(|(part, _)| part)
            .collect();

        if possible_parts.is_empty() {
            bail!();
        }

        if lit_start != i {
            replacements.push(vec![PathMatcherPart::Literal(path[lit_start..i].into())]);
        }

        possible_parts.push(PathMatcherPart::Literal(subpath[..len].into()));
        possible_parts.sort_by_key(|part| part.flexibility());

        replacements.push(possible_parts);

        i += len;
        lit_start = i;
    }

    if lit_start < path.len() {
        replacements.push(vec![PathMatcherPart::Literal(path[lit_start..].into())]);
    }

    replacements
}

/// Finds the best matcher for the given path.
fn best_matcher_for_path(
    path: &str,
    files: &Files,
    selected_path_list: Option<&PathList>,
    path_lists: &[&PathList],
) -> (PathMatcher, Option<PathMatcher>) {
    let replacements = find_replacements(path);

    // generate all permutations of matchers for a given path
    let part_len: Vec<_> = replacements.iter().map(|vec| vec.len()).collect();
    let mut part_pos = vec![0; part_len.len()];

    if part_len.iter().product::<usize>() > 2500 {
        // too many combinations to evaluate
        // its probably faster to just let the user do it manually

        // the least flexible part should be at the start, so we just take the first part
        let mut matcher = PathMatcher {
            parts: replacements
                .iter()
                .map(|repl| repl.first().unwrap().clone())
                .collect(),
        };
        matcher.canonicalize();
        return (matcher, None);
    }

    let mut max_matcher_strict = None;
    let mut max_matcher = None;

    'outer: loop {
        let matcher = PathMatcher {
            parts: replacements
                .iter()
                .zip(part_pos.iter().copied())
                .map(|(vec, idx)| vec[idx].clone())
                .collect(),
        };
        let flexibility: u128 = matcher
            .parts
            .iter()
            .map(|part| part.flexibility() as u128)
            .sum();
        let viability = MatcherViability::compute(&matcher, files, selected_path_list, path_lists);

        let update_max = |current_max: &mut Option<(PathMatcher, u128, MatcherViability)>,
                          allow_multiple_selected| {
            if let Some((_, max_flexibility, max_viability)) = current_max.as_mut() {
                match viability.cmp(max_viability, allow_multiple_selected) {
                    Ordering::Less => (),
                    Ordering::Equal => {
                        if *max_flexibility > flexibility {
                            *current_max = Some((matcher.clone(), flexibility, viability.clone()));
                        }
                    }
                    Ordering::Greater => {
                        *current_max = Some((matcher.clone(), flexibility, viability.clone()));
                    }
                }
            } else {
                *current_max = Some((matcher.clone(), flexibility, viability.clone()));
            }
        };

        update_max(&mut max_matcher_strict, true);
        update_max(&mut max_matcher, false);

        for (pos, len) in part_pos.iter_mut().zip(part_len.iter().copied()) {
            if *pos + 1 >= len {
                *pos = 0;
            } else {
                *pos += 1;
                continue 'outer;
            }
        }
        break;
    }

    let mut max_matcher = max_matcher.unwrap().0;
    max_matcher.canonicalize();
    let mut max_matcher_strict = max_matcher_strict.unwrap().0;
    max_matcher_strict.canonicalize();

    let lenient = (max_matcher != max_matcher_strict).then_some(max_matcher);

    (max_matcher_strict, lenient)
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
        PathMatcherPart::Locale(Case::Mixed),
        PathMatcherPart::Locale(Case::Lower),
        PathMatcherPart::Locale(Case::Upper),
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
        let extra_allowed_chars = path_part
            .chars()
            .filter(|c| !c.is_alphanumeric())
            .collect::<BTreeSet<char>>()
            .into_iter()
            .collect::<Vec<char>>();

        for case in [Case::Upper, Case::Lower, Case::Mixed] {
            for contains_digits in [false, true] {
                possible_parts.push(PathMatcherPart::AlphaOrAlphanumeric {
                    contains_digits,
                    case,
                    extra_allowed_chars: extra_allowed_chars.clone(),
                    min_len: Some(1),
                    max_len: None,
                });
            }
        }
    }
    if username.is_some() || !automatic {
        possible_parts.push(PathMatcherPart::Username);
    }

    let mut result = Vec::new();

    for part in possible_parts {
        let Some(match_len) = part.match_len(path_part, username, None) else { continue };
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
) -> Option<(PathMatcher, Option<PathMatcher>)> {
    files
        .alphabetical_order()
        .files()
        .filter(|file| !rules.is_matched(file))
        .map(|file| best_matcher_for_path(file.path(), files, selected_path_list, path_lists))
        .filter(|(matcher, _)| Rule::from_matcher(matcher.clone(), files).is_some())
        .nth(skip)
}
