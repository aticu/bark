//! Support for handling paths and globs.

mod best_variant;
mod case;
mod part;

use std::{borrow::Cow, fmt};

use inlinable_string::{InlinableString, StringExt as _};
use smallvec::SmallVec;

use crate::file::File;

use self::case::Case;

pub(crate) use self::best_variant::{possible_replacements, suggest_matcher};
pub(crate) use self::part::PathMatcherPart;

/// A data structure that may either match a path or not.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[serde(from = "String")]
#[serde(into = "String")]
pub(crate) struct PathMatcher {
    /// The parts of the path matcher.
    pub(crate) parts: SmallVec<[PathMatcherPart; 5]>,
}

impl fmt::Display for PathMatcher {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for part in &self.parts {
            write!(f, "{part}")?;
        }

        Ok(())
    }
}

impl From<&str> for PathMatcher {
    fn from(value: &str) -> Self {
        fn parse_min_max(value: &str) -> Option<(Option<u16>, Option<u16>)> {
            match value {
                "*" => Some((None, None)),
                "+" => Some((Some(1), None)),
                "" => Some((Some(1), Some(1))),
                _ => {
                    if value.contains('-') {
                        let mut iter = value.split('-');
                        let min = iter.next()?;
                        let max = iter.next()?;

                        if iter.next().is_some() {
                            return None;
                        }

                        let min = if min.is_empty() {
                            None
                        } else {
                            Some(min.parse().ok()?)
                        };
                        let max = if max.is_empty() {
                            None
                        } else {
                            Some(max.parse().ok()?)
                        };

                        Some((min, max))
                    } else {
                        let both = value.parse().ok()?;
                        Some((Some(both), Some(both)))
                    }
                }
            }
        }

        fn parse_extra_chars(value: &str) -> Option<(Vec<char>, &str)> {
            if !value.starts_with('[') {
                return Some((Vec::new(), value));
            }

            let mut result = Vec::new();
            let mut escape = false;

            for (i, c) in value.char_indices().skip(1) {
                if escape {
                    result.push(c);
                    escape = false;
                    continue;
                }
                if c == '\\' {
                    escape = true;
                    continue;
                }
                if c == ']' {
                    return Some((result, &value[i + 1..]));
                }

                result.push(c);
            }

            None
        }

        fn parse_pattern(value: &str) -> Option<(PathMatcherPart, usize)> {
            let (pattern, _) = value.split_once('>')?;
            let after_pattern = pattern.len() + 1;

            if pattern.eq_ignore_ascii_case("uuid") {
                let case = pattern
                    .chars()
                    .map(|c| Case::try_from(c).unwrap())
                    .reduce(|case1, case2| case1 + case2)
                    .unwrap();

                Some((PathMatcherPart::Uuid(case), after_pattern))
            } else if pattern.len() >= 3 && pattern[..3].eq_ignore_ascii_case("hex") {
                let case = pattern[..3]
                    .chars()
                    .map(|c| Case::try_from(c).unwrap())
                    .reduce(|case1, case2| case1 + case2)
                    .unwrap();
                let (min, max) = parse_min_max(&pattern[3..])?;

                Some((
                    PathMatcherPart::HexDigit {
                        case,
                        min_len: min,
                        max_len: max,
                    },
                    after_pattern,
                ))
            } else if pattern.len() >= 8 && pattern[..8].eq_ignore_ascii_case("alphanum") {
                let case = pattern[..8]
                    .chars()
                    .map(|c| Case::try_from(c).unwrap())
                    .reduce(|case1, case2| case1 + case2)
                    .unwrap();
                let (extra_allowed_chars, rest) = parse_extra_chars(&pattern[8..])?;
                let (min, max) = parse_min_max(rest)?;

                Some((
                    PathMatcherPart::AlphaOrAlphanumeric {
                        contains_digits: true,
                        case,
                        extra_allowed_chars,
                        min_len: min,
                        max_len: max,
                    },
                    after_pattern,
                ))
            } else if pattern.len() >= 5 && pattern[..5].eq_ignore_ascii_case("alpha") {
                let case = pattern[..5]
                    .chars()
                    .map(|c| Case::try_from(c).unwrap())
                    .reduce(|case1, case2| case1 + case2)
                    .unwrap();
                let (extra_allowed_chars, rest) = parse_extra_chars(&pattern[5..])?;
                let (min, max) = parse_min_max(rest)?;

                Some((
                    PathMatcherPart::AlphaOrAlphanumeric {
                        contains_digits: false,
                        case,
                        extra_allowed_chars,
                        min_len: min,
                        max_len: max,
                    },
                    after_pattern,
                ))
            } else if pattern.len() >= 5 && pattern[..5].eq_ignore_ascii_case("digit") {
                let (min, max) = parse_min_max(&pattern[5..])?;

                Some((
                    PathMatcherPart::Digit {
                        min_len: min,
                        max_len: max,
                    },
                    after_pattern,
                ))
            } else if pattern.eq_ignore_ascii_case("assemblyver") {
                Some((PathMatcherPart::AssemblyVersion, after_pattern))
            } else if pattern.eq_ignore_ascii_case("locale") {
                let case = pattern
                    .chars()
                    .map(|c| Case::try_from(c).unwrap())
                    .reduce(|case1, case2| case1 + case2)
                    .unwrap();
                Some((PathMatcherPart::Locale(case), after_pattern))
            } else if pattern.eq_ignore_ascii_case("username") {
                Some((PathMatcherPart::Username, after_pattern))
            } else {
                None
            }
        }

        let mut parts = SmallVec::new();

        let mut iter_start_idx = 0;
        let mut iter = value.char_indices();
        let mut lit = InlinableString::new();
        let mut escape = false;

        while let Some((i, c)) = iter.next() {
            if escape {
                lit.push(c);
                escape = false;
            } else if c == '\\' {
                escape = true;
            } else if c == '<' {
                let inside_pattern = iter_start_idx + i + 1;
                if let Some((pattern, after_pattern)) = parse_pattern(&value[inside_pattern..]) {
                    if !lit.is_empty() {
                        parts.push(PathMatcherPart::Literal(lit));
                        lit = InlinableString::new();
                    }
                    parts.push(pattern);
                    iter = value[inside_pattern + after_pattern..].char_indices();
                    iter_start_idx = inside_pattern + after_pattern;
                } else {
                    lit.push(c);
                }
            } else {
                lit.push(c);
            }
        }

        if !lit.is_empty() {
            parts.push(PathMatcherPart::Literal(lit));
        }

        Self { parts }
    }
}

impl From<String> for PathMatcher {
    fn from(value: String) -> Self {
        PathMatcher::from(value.as_str())
    }
}

impl From<PathMatcher> for String {
    fn from(value: PathMatcher) -> Self {
        format!("{value}")
    }
}

impl PathMatcher {
    /// Determines how many characters of the given path are matched by this matcher.
    pub(crate) fn match_len(&self, path: &str) -> Option<usize> {
        let mut iter = path.chars().peekable();
        let mut username = None;
        let mut parts = self.parts.iter().peekable();

        while let Some(part) = parts.next() {
            let next_lit = if let Some(PathMatcherPart::Literal(lit)) = parts.peek() {
                Some(&**lit)
            } else {
                None
            };

            if !part.matches(&mut iter, &mut username, next_lit) {
                return None;
            }
        }

        Some(path.len() - iter.count())
    }

    /// Determines if the given path is matched by the path matcher.
    pub(crate) fn matches_path(&self, path: &str) -> bool {
        self.match_len(path) == Some(path.len())
    }

    /// Determines if the given file is matched by the path matcher.
    pub(crate) fn matches_file(&self, file: &File) -> bool {
        file.paths.iter().any(|path| self.matches_path(path))
    }

    /// Returns `true` if this path matcher only consists of literals.
    pub(crate) fn is_literal(&self) -> bool {
        self.parts.iter().all(|part| part.is_literal())
    }

    /// The prefix of the matcher that matches literally.
    pub(crate) fn literal_prefix(&self) -> Cow<str> {
        match &self.parts[..] {
            [PathMatcherPart::Literal(lit)] => Cow::Borrowed(lit),
            [PathMatcherPart::Literal(lit), part, ..] if !part.is_literal() => Cow::Borrowed(lit),
            [PathMatcherPart::Literal(_), ..] => Cow::Owned(
                self.parts
                    .iter()
                    .map_while(|part| match part {
                        PathMatcherPart::Literal(lit) => Some(&lit[..]),
                        _ => None,
                    })
                    .collect(),
            ),
            _ => Cow::Borrowed(""),
        }
    }

    /// Strips the given prefix from the matcher if it is a prefix.
    pub(crate) fn strip_prefix(&self, prefix: &PathMatcher) -> Option<PathMatcher> {
        if self
            .parts
            .iter()
            .take(prefix.parts.len())
            .eq(prefix.parts.iter())
        {
            Some(PathMatcher {
                parts: self
                    .parts
                    .iter()
                    .skip(prefix.parts.len())
                    .cloned()
                    .collect(),
            })
        } else {
            None
        }
    }

    /// Makes sure that the path matcher is canonical by merging together parts that can be merged.
    fn canonicalize(&mut self) {
        if self
            .parts
            .windows(2)
            .any(|win| win[0].is_literal() && win[1].is_literal())
        {
            let mut new_parts = SmallVec::new();

            for part in std::mem::take(&mut self.parts) {
                if let PathMatcherPart::Literal(new_lit) = &part &&
                    let Some(PathMatcherPart::Literal(lit)) = new_parts.last_mut() {
                        lit.push_str(new_lit);
                } else {
                    new_parts.push(part);
                }
            }

            self.parts = new_parts;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{case::Case, PathMatcher, PathMatcherPart};

    #[test]
    fn display_parse() {
        let matcher = PathMatcher {
            parts: smallvec::smallvec![
                PathMatcherPart::Literal("/".into()),
                PathMatcherPart::Username,
                PathMatcherPart::Literal("/".into()),
                PathMatcherPart::Uuid(Case::Lower),
                PathMatcherPart::Locale(Case::Mixed),
                PathMatcherPart::AssemblyVersion,
                PathMatcherPart::Digit {
                    min_len: Some(1),
                    max_len: None,
                },
                PathMatcherPart::Digit {
                    min_len: Some(1),
                    max_len: Some(3),
                },
                PathMatcherPart::Digit {
                    min_len: None,
                    max_len: None,
                },
                PathMatcherPart::Digit {
                    min_len: Some(2),
                    max_len: None,
                },
                PathMatcherPart::Digit {
                    min_len: None,
                    max_len: Some(2),
                },
                PathMatcherPart::Digit {
                    min_len: Some(1),
                    max_len: Some(1),
                },
                PathMatcherPart::Digit {
                    min_len: Some(2),
                    max_len: Some(2),
                },
                PathMatcherPart::HexDigit {
                    case: Case::Upper,
                    min_len: Some(2),
                    max_len: Some(2),
                },
                PathMatcherPart::AlphaOrAlphanumeric {
                    contains_digits: false,
                    case: Case::Lower,
                    extra_allowed_chars: Vec::new(),
                    min_len: Some(1),
                    max_len: Some(1),
                },
                PathMatcherPart::AlphaOrAlphanumeric {
                    contains_digits: true,
                    case: Case::Lower,
                    extra_allowed_chars: Vec::new(),
                    min_len: Some(1),
                    max_len: Some(1),
                },
                PathMatcherPart::AlphaOrAlphanumeric {
                    contains_digits: true,
                    case: Case::Mixed,
                    extra_allowed_chars: Vec::new(),
                    min_len: Some(5),
                    max_len: Some(5),
                },
                PathMatcherPart::AlphaOrAlphanumeric {
                    contains_digits: true,
                    case: Case::Upper,
                    extra_allowed_chars: vec!['\\', '_', ']', '-'],
                    min_len: Some(1),
                    max_len: None,
                },
            ],
        };

        let displayed = format!("{matcher}");

        assert_eq!(displayed, "/<username>/<uuid><Locale><assemblyver><digit+><digit1-3><digit*><digit2-><digit-2><digit><digit2><HEX2><alpha><alphanum><Alphanum5><ALPHANUM[\\\\_\\]-]+>");

        let parsed = PathMatcher::from(displayed.as_str());
        assert_eq!(matcher, parsed);

        let matcher = PathMatcher {
            parts: smallvec::smallvec![PathMatcherPart::Literal("\\abc<yay".into()),],
        };

        let displayed = format!("{matcher}");

        assert_eq!(displayed, "\\\\abc\\<yay");
        assert!(matcher.matches_path("\\abc<yay"));

        let parsed = PathMatcher::from(displayed.as_str());
        assert_eq!(matcher, parsed);

        let matcher = PathMatcher {
            parts: smallvec::smallvec![PathMatcherPart::Literal("<Alphanum[\\]>".into()),],
        };

        let displayed = format!("{matcher}");

        assert_eq!(displayed, "\\<Alphanum[\\\\]>");

        let parsed = PathMatcher::from(displayed.as_str());
        assert_eq!(matcher, parsed);
    }

    #[test]
    fn some_path() {
        let matcher = PathMatcher::from(
            "/Users/<username>/AppData/Local/ConnectedDevicesPlatform/L.<username>",
        );

        assert!(matcher.matches_path("/Users/user/AppData/Local/ConnectedDevicesPlatform/L.user"));
        assert!(!matcher.matches_path("/Users/user/AppData/Local/ConnectedDevicesPlatform/L.user2"));
        assert!(!matcher.matches_path("/Users/user/AppData/Local/ConnectedDevicesPlatform/L.oser"));
        assert!(!matcher.matches_path("/Users/user/AppData/Local/ConnectedDevicesPlatform/L.use"));
        assert!(!matcher.matches_path("/Users/user/AppData/Local/ConnectedPlatform/L.user"));

        let matcher = PathMatcher::from(
            "/Windows/ServiceProfiles/LocalService/AppData/Local/ConnectedDevicesPlatform/L.<username>.cdp"
        );

        assert!(matcher.matches_path("/Windows/ServiceProfiles/LocalService/AppData/Local/ConnectedDevicesPlatform/L.user.cdp"));
        assert!(matcher.matches_path("/Windows/ServiceProfiles/LocalService/AppData/Local/ConnectedDevicesPlatform/L.another-user.cdp"));
        assert!(matcher.matches_path("/Windows/ServiceProfiles/LocalService/AppData/Local/ConnectedDevicesPlatform/L.max.cdp"));
        assert!(!matcher.matches_path("/Windows/ServiceProfiles/LocalService/AppData/Local/ConnectedDevicesPlatform/L.Public.cdp"));

        let matcher = PathMatcher::from("/Program Files (x86)/Microsoft/EdgeUpdate");

        assert!(matcher.matches_path("/Program Files (x86)/Microsoft/EdgeUpdate"));
    }

    #[test]
    fn parse() {
        let matcher = PathMatcher::from("/\\<username>/<username>/<username>");

        assert!(matcher.matches_path("/<username>/bla/bla"));
        assert!(!matcher.matches_path("/bla/bla/bla"));

        let matcher = PathMatcher::from("/<hEx>/<HEX>/<hex>");

        assert!(matcher.matches_path("/a/A/a"));
        assert!(matcher.matches_path("/A/A/a"));
        assert!(!matcher.matches_path("/a/a/a"));
        assert!(!matcher.matches_path("/A/A/A"));

        let matcher = PathMatcher::from("/only-lits");

        assert!(matcher.matches_path("/only-lits"));
        assert!(!matcher.matches_path("/only-lads"));

        let matcher = PathMatcher::from("/<Alphanum[-_]+>");

        assert!(matcher.matches_path("/only-lits"));
        assert!(matcher.matches_path("/aNaebBE6-o1aEOL6__37"));
        assert!(!matcher.matches_path("/aNaebBE6+o1aEOL6__37"));
    }

    #[test]
    fn empty_len() {
        let matcher = PathMatcher::from("");

        assert_eq!(matcher.match_len("/test/path"), Some(0));
    }
}
