//! Implements a single part within a part matcher.

use std::fmt;

use inlinable_string::{InlinableString, StringExt as _};
use smallvec::SmallVec;

use super::case::Case;

/// One part of a path matcher.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum PathMatcherPart {
    /// The path part matches a literal string.
    Literal(InlinableString),
    /// The path part matches a UUID of the given case.
    Uuid(Case),
    /// The path part matches a locale specification.
    ///
    /// Note that the case here is not as flexible as for other matcher parts.
    ///
    /// Examples of
    /// - mixed case are `en-US`, `de-DE` or `it`
    /// - lower case are `en-us`, `de-de` or `it`
    /// - upper case are `EN-US`, `DE-DE` or `IT`
    Locale(Case),
    /// The path matches an assembly version.
    ///
    /// An example of this would be `1.5.1254.0`.
    /// See [here](https://learn.microsoft.com/en-us/dotnet/standard/assembly/versioning) for more
    /// details.
    AssemblyVersion,
    /// The path part matches a number of digits.
    Digit {
        /// The minimum length of the digit chain.
        min_len: Option<u16>,
        /// The maximum length of the digit chain.
        max_len: Option<u16>,
    },
    /// The path part matches a number of hexadecimal digits.
    HexDigit {
        /// The case of the hexadecimal digits.
        case: Case,
        /// The minimum length of the hexadecimal digit chain.
        min_len: Option<u16>,
        /// The maximum length of the hexadecimal digit chain.
        max_len: Option<u16>,
    },
    /// The path part matches a number of alphabetic or alphanumeric characters.
    AlphaOrAlphanumeric {
        /// Whether digits are allowed or not.
        ///
        /// If this is `true`, this matcher matches alphanumeric characters, otherwise it only
        /// matches alphabetic characters.
        contains_digits: bool,
        /// Other characters that are additionally allowed.
        extra_allowed_chars: Vec<char>,
        /// The case of the alphabetic characters.
        case: Case,
        /// The minimum length of the alphabetic or alphanumeric digit characters.
        min_len: Option<u16>,
        /// The maximum length of the alphabetic or alphanumeric digit characters.
        max_len: Option<u16>,
    },
    /// The path part matches a user name.
    Username,
}

impl fmt::Display for PathMatcherPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let write_min_max = |f: &mut fmt::Formatter<'_>, min, max| match (min, max) {
            (None, None) => write!(f, "*"),
            (Some(1), None) => write!(f, "+"),
            (Some(1), Some(1)) => Ok(()),
            (Some(min), None) => write!(f, "{min}-"),
            (None, Some(max)) => write!(f, "-{max}"),
            (Some(min), Some(max)) => {
                if min == max {
                    write!(f, "{max}")
                } else {
                    write!(f, "{min}-{max}")
                }
            }
        };
        match self {
            PathMatcherPart::Literal(lit) => {
                for part in lit.split_inclusive(&['\\', '<']) {
                    if let Some(part) = part.strip_suffix('\\') {
                        write!(f, "{part}\\\\")?;
                    } else if let Some(part) = part.strip_suffix('<') {
                        write!(f, "{part}\\<")?;
                    } else {
                        write!(f, "{part}")?;
                    }
                }

                Ok(())
            }
            PathMatcherPart::Uuid(case) => write!(
                f,
                "<{}>",
                match case {
                    Case::Upper => "UUID",
                    Case::Lower => "uuid",
                    Case::Mixed => "Uuid",
                }
            ),
            PathMatcherPart::Locale(case) => write!(
                f,
                "<{}>",
                match case {
                    Case::Upper => "LOCALE",
                    Case::Lower => "locale",
                    Case::Mixed => "Locale",
                }
            ),
            PathMatcherPart::AssemblyVersion => write!(f, "<assemblyver>"),
            PathMatcherPart::Digit { min_len, max_len } => {
                write!(f, "<digit")?;
                write_min_max(f, *min_len, *max_len)?;
                write!(f, ">")
            }
            PathMatcherPart::HexDigit {
                case,
                min_len,
                max_len,
            } => {
                match case {
                    Case::Upper => write!(f, "<HEX")?,
                    Case::Lower => write!(f, "<hex")?,
                    Case::Mixed => write!(f, "<Hex")?,
                }
                write_min_max(f, *min_len, *max_len)?;
                write!(f, ">")
            }
            PathMatcherPart::AlphaOrAlphanumeric {
                contains_digits,
                extra_allowed_chars,
                case,
                min_len,
                max_len,
            } => {
                if *contains_digits {
                    match case {
                        Case::Upper => write!(f, "<ALPHANUM")?,
                        Case::Lower => write!(f, "<alphanum")?,
                        Case::Mixed => write!(f, "<Alphanum")?,
                    }
                } else {
                    match case {
                        Case::Upper => write!(f, "<ALPHA")?,
                        Case::Lower => write!(f, "<alpha")?,
                        Case::Mixed => write!(f, "<Alpha")?,
                    }
                }
                if !extra_allowed_chars.is_empty() {
                    write!(f, "[")?;
                    for &c in extra_allowed_chars {
                        if c == '\\' || c == ']' {
                            write!(f, "\\")?;
                        }
                        write!(f, "{c}")?;
                    }
                    write!(f, "]")?;
                }
                write_min_max(f, *min_len, *max_len)?;
                write!(f, ">")
            }
            PathMatcherPart::Username => write!(f, "<username>"),
        }
    }
}

/// The usernames that are expected to be present on a Windows system and thus should not match a
/// username matcher.
pub(crate) const DEFAULT_USERS: [&str; 6] = [
    "All Users",
    "Default User",
    "desktop.ini",
    "Public",
    "Default",
    "defaultuser0",
];

impl PathMatcherPart {
    /// Determines if the path part is matched by the given iterator.
    ///
    /// If this function returns `true` the iterator is advanced such that the next returned
    /// character is the first character not part of the match.
    /// If it returns `false`, the state of the iterator is unspecified.
    pub(super) fn matches(
        &self,
        iter: &mut std::iter::Peekable<impl Iterator<Item = char> + Clone>,
        username: &mut Option<InlinableString>,
        next_lit: Option<&str>,
    ) -> bool {
        match self {
            PathMatcherPart::Literal(lit) => iter.take(lit.len()).eq(lit.chars()),
            PathMatcherPart::Uuid(case) => {
                for part_len in [8, 4, 4, 4, 12] {
                    let mut len = 0;
                    for c in iter.take(part_len) {
                        len += 1;
                        if !case.matches_char(c) || !c.is_ascii_hexdigit() {
                            return false;
                        }
                    }
                    if len != part_len {
                        return false;
                    }

                    if part_len != 12 && iter.next() != Some('-') {
                        return false;
                    }
                }

                true
            }
            PathMatcherPart::Locale(case) => {
                let mut len = 0;

                for c in iter.take(2) {
                    len += 1;
                    match case {
                        Case::Lower | Case::Mixed if !c.is_ascii_lowercase() => return false,
                        Case::Upper if !c.is_ascii_uppercase() => return false,
                        _ => (),
                    }
                }
                if len != 2 {
                    return false;
                }

                if iter.peek() == Some(&'-') {
                    iter.next();
                    len += 1;

                    for c in iter.take(2) {
                        len += 1;
                        match case {
                            Case::Upper | Case::Mixed if !c.is_ascii_uppercase() => return false,
                            Case::Lower if !c.is_ascii_lowercase() => return false,
                            _ => (),
                        }
                    }
                    if len != 5 {
                        return false;
                    }
                }

                true
            }
            PathMatcherPart::AssemblyVersion => {
                for i in 0..4 {
                    if !match_part_min_max(iter, |c| c.is_ascii_digit(), Some(1), None) {
                        return false;
                    }

                    if i != 3 && iter.next() != Some('.') {
                        return false;
                    }
                }

                true
            }
            PathMatcherPart::Digit { min_len, max_len } => {
                match_part_min_max(iter, |c| c.is_ascii_digit(), *min_len, *max_len)
            }
            PathMatcherPart::HexDigit {
                case,
                min_len,
                max_len,
            } => match_part_min_max(
                iter,
                |c| c.is_ascii_hexdigit() && case.matches_char(c),
                *min_len,
                *max_len,
            ),
            PathMatcherPart::AlphaOrAlphanumeric {
                contains_digits,
                case,
                extra_allowed_chars,
                min_len,
                max_len,
            } => match_part_min_max(
                iter,
                |c| {
                    (case.matches_char(c)
                        && if *contains_digits {
                            c.is_alphanumeric()
                        } else {
                            c.is_alphabetic()
                        })
                        || extra_allowed_chars.contains(&c)
                },
                *min_len,
                *max_len,
            ),
            PathMatcherPart::Username => {
                if let Some(username) = username {
                    iter.take(username.len()).eq(username.chars())
                } else {
                    let mut new_username = InlinableString::new();
                    let mut non_user_iters = DEFAULT_USERS.map(|user| Some(user.chars()));

                    let any_non_user_iter_matched =
                        |non_user_iters: &mut [Option<std::str::Chars>]| {
                            non_user_iters
                                .iter_mut()
                                .filter_map(|non_user_iter| non_user_iter.as_mut())
                                .any(|non_user_iter| non_user_iter.next().is_none())
                        };

                    loop {
                        let Some(&c) = iter.peek() else {
                            if any_non_user_iter_matched(&mut non_user_iters) {
                                return false;
                            }

                            break
                        };
                        if c == '/' {
                            if any_non_user_iter_matched(&mut non_user_iters) {
                                return false;
                            }

                            break;
                        }
                        if let Some(next_lit) = next_lit {
                            if iter.clone().take(next_lit.len()).eq(next_lit.chars()) {
                                if any_non_user_iter_matched(&mut non_user_iters) {
                                    return false;
                                }

                                break;
                            }
                        }

                        iter.next();
                        for opt_non_user_iter in &mut non_user_iters {
                            if let Some(non_user_iter) = opt_non_user_iter {
                                if Some(c) != non_user_iter.next() {
                                    *opt_non_user_iter = None;
                                }
                            }
                        }

                        new_username.push(c);
                    }

                    *username = Some(new_username);
                    true
                }
            }
        }
    }

    /// Determines the length that the matcher matches on the given input, if it matches.
    pub(super) fn match_len(
        &self,
        input: &str,
        username: &Option<InlinableString>,
        next_lit: Option<&str>,
    ) -> Option<usize> {
        let mut username = username.clone();
        let total_len = input.chars().count();
        let mut iter = input.chars().peekable();

        if self.matches(iter.by_ref(), &mut username, next_lit) {
            Some(total_len - iter.count())
        } else {
            None
        }
    }

    /// Returns `true`, if this part is a literal.
    pub(super) fn is_literal(&self) -> bool {
        matches!(self, PathMatcherPart::Literal(_))
    }

    /// Returns all the variants of this part that could match the given length.
    pub(super) fn variants_with_match_len(&self, len: usize) -> SmallVec<[PathMatcherPart; 2]> {
        if len == 0 {
            return SmallVec::new();
        }

        let mut results = SmallVec::new();

        match self {
            PathMatcherPart::Literal(lit) => {
                if lit.len() == len {
                    results.push(self.clone());
                }
            }
            PathMatcherPart::Uuid(_) => {
                if len == 36 {
                    results.push(self.clone());
                }
            }
            PathMatcherPart::Locale(_) => {
                if len == 2 || len == 5 {
                    results.push(self.clone());
                }
            }
            PathMatcherPart::AssemblyVersion => {
                if len >= 7 {
                    results.push(self.clone());
                }
            }
            PathMatcherPart::Digit { .. } => {
                if let Ok(len) = len.try_into() {
                    results.push(PathMatcherPart::Digit {
                        min_len: Some(len),
                        max_len: Some(len),
                    });
                }
                results.push(PathMatcherPart::Digit {
                    min_len: Some(1),
                    max_len: None,
                });
            }
            PathMatcherPart::HexDigit { case, .. } => {
                let case = *case;
                if let Ok(len) = len.try_into() {
                    results.push(PathMatcherPart::HexDigit {
                        case,
                        min_len: Some(len),
                        max_len: Some(len),
                    });
                }
                results.push(PathMatcherPart::HexDigit {
                    case,
                    min_len: Some(1),
                    max_len: None,
                });
            }
            PathMatcherPart::AlphaOrAlphanumeric {
                contains_digits,
                extra_allowed_chars,
                case,
                ..
            } => {
                let case = *case;
                if let Ok(len) = len.try_into() {
                    results.push(PathMatcherPart::AlphaOrAlphanumeric {
                        contains_digits: *contains_digits,
                        extra_allowed_chars: extra_allowed_chars.clone(),
                        case,
                        min_len: Some(len),
                        max_len: Some(len),
                    });
                }
                results.push(PathMatcherPart::AlphaOrAlphanumeric {
                    contains_digits: *contains_digits,
                    extra_allowed_chars: extra_allowed_chars.clone(),
                    case,
                    min_len: Some(1),
                    max_len: None,
                });
            }
            PathMatcherPart::Username => {
                // we cannot confirm that the name of the username is correct here, so the caller
                // is responsible for that
                results.push(self.clone());
            }
        }

        results
    }

    /// Returns a value that can be used to compare how flexible matcher parts are.
    ///
    /// Values comparing lower will match fewer paths while higher comparing values will match more paths.
    pub(super) fn flexibility(&self) -> u64 {
        // the resulting number is split as follows:
        //
        // MSB               LSB
        //  | `flags` | `len` |
        //
        // where
        // - `flags` encode general information about the part (ordered by importance)
        // - `len` encodes information about the possible lengths allowed by the matcher part

        const IMPORTANCE: &[&str] = &[
            "lit",
            "username",
            "locale",
            "uuid",
            "mixed_case",
            "assemblyver",
            "digit",
            "hexdigit",
            "alpha",
            "alphanum",
            "extra_chars",
            "dynamic_len",
        ];

        const FLAG_BITS: usize = IMPORTANCE.len();
        const FLAGS_START: usize = 64 - FLAG_BITS;
        const LEN_BITS: usize = 64 - FLAG_BITS;
        const LEN_START: usize = FLAGS_START - LEN_BITS;
        const MAX_LEN_VAL: u64 = (1 << (LEN_BITS + 1)) - 1;

        let mut score = 0u64;

        macro_rules! add_flag {
            ($val:literal) => {{
                score |=
                    1 << (IMPORTANCE.iter().position(|elem| elem == &$val).unwrap() + FLAGS_START);
            }};
        }

        {
            match self {
                PathMatcherPart::Literal(_) => add_flag!("lit"),
                PathMatcherPart::Uuid(_) => add_flag!("uuid"),
                PathMatcherPart::Locale(_) => add_flag!("locale"),
                PathMatcherPart::AssemblyVersion => add_flag!("assemblyver"),
                PathMatcherPart::Digit { .. } => add_flag!("digit"),
                PathMatcherPart::HexDigit { .. } => add_flag!("hexdigit"),
                PathMatcherPart::AlphaOrAlphanumeric {
                    contains_digits: false,
                    ..
                } => add_flag!("alpha"),
                PathMatcherPart::AlphaOrAlphanumeric {
                    contains_digits: true,
                    ..
                } => add_flag!("alphanum"),
                PathMatcherPart::Username => add_flag!("username"),
            }
        }

        let (hex_min, hex_max) = match self {
            PathMatcherPart::HexDigit {
                min_len, max_len, ..
            } => (*min_len, *max_len),
            _ => (None, None),
        };
        let (alpha_min, alpha_max) = match self {
            PathMatcherPart::AlphaOrAlphanumeric {
                min_len, max_len, ..
            } => (*min_len, *max_len),
            _ => (None, None),
        };
        let (digit_min, digit_max) = match self {
            PathMatcherPart::Digit { min_len, max_len } => (*min_len, *max_len),
            _ => (None, None),
        };
        let has_len_component = match self {
            PathMatcherPart::Literal(_)
            | PathMatcherPart::Uuid(_)
            | PathMatcherPart::Locale(_)
            | PathMatcherPart::AssemblyVersion
            | PathMatcherPart::Username => false,
            PathMatcherPart::Digit { .. }
            | PathMatcherPart::HexDigit { .. }
            | PathMatcherPart::AlphaOrAlphanumeric { .. } => true,
        };

        let min = hex_min.or(alpha_min).or(digit_min);
        let max = hex_max.or(alpha_max).or(digit_max);
        let dynamic_len = has_len_component && (min != max || min.is_none() || max.is_none());

        let mixed_case = match self {
            PathMatcherPart::Literal(_)
            // Note that the locale has a case component, but there "mixed" just specifies a
            // different fixed case configuration, so we return `false` here
            | PathMatcherPart::Locale(_)
            | PathMatcherPart::AssemblyVersion
            | PathMatcherPart::Digit { .. }
            | PathMatcherPart::Username => false,
            PathMatcherPart::Uuid(case)
            | PathMatcherPart::HexDigit { case, .. }
            | PathMatcherPart::AlphaOrAlphanumeric { case, .. } => *case == Case::Mixed,
        };

        let extra_chars = matches!(self, PathMatcherPart::AlphaOrAlphanumeric {
                extra_allowed_chars,
                ..
            } if !extra_allowed_chars.is_empty());

        if dynamic_len {
            add_flag!("dynamic_len");
        }

        if mixed_case {
            add_flag!("mixed_case");
        }

        if extra_chars {
            add_flag!("extra_chars");
        }

        let possible_lens = match (min, max) {
            (Some(min), Some(max)) => {
                if max > min {
                    (max - min) as u64
                } else {
                    0
                }
            }
            (Some(min), None) => MAX_LEN_VAL.saturating_sub(min as u64),
            (None, Some(max)) => (max as u64).min(MAX_LEN_VAL),
            (None, None) => MAX_LEN_VAL,
        };

        score |= possible_lens << LEN_START;

        score
    }
}

/// A helper function that consumes matching characters from the iterator.
///
/// If `min` is `Some(_)`, then at least that many characters are required to match.
/// If `max` is `Some(_)`, then at most that many characters are consumed.
fn match_part_min_max(
    iter: &mut std::iter::Peekable<impl Iterator<Item = char>>,
    mut matches: impl FnMut(char) -> bool,
    min: Option<u16>,
    max: Option<u16>,
) -> bool {
    if let Some(min) = min {
        let mut len = 0;
        for c in iter.take(min.into()) {
            len += 1;
            if !matches(c) {
                return false;
            }
        }
        if len != min {
            return false;
        }
    }
    let max = max.map(|max| max - min.unwrap_or(0));

    let mut len = 0;
    loop {
        if Some(len) == max {
            return true;
        }

        let Some(&c) = iter.peek() else { return true };
        if !matches(c) {
            return true;
        }

        iter.next();
        len += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::{super::case::Case, PathMatcherPart};

    // if `rest` is `Some(_)`, the matching is expected to be successful, otherwise it's expected
    // to fail
    fn test(matcher: &PathMatcherPart, input: &str, rest: Option<&str>) {
        for mut user in [None, Some("user".into())] {
            let user_before = user.clone();

            let mut iter = input.chars().peekable();
            if let Some(rest) = rest {
                assert!(matcher.matches(&mut iter, &mut user, None));
                assert_eq!(
                    iter.collect::<String>(),
                    rest,
                    "expected {rest} for {input}"
                );
            } else {
                assert!(!matcher.matches(&mut iter, &mut user, None));
            }

            assert_eq!(user, user_before);
        }
    }

    fn test_with_user(
        matcher: &PathMatcherPart,
        input: &str,
        rest: Option<&str>,
        user: Option<&str>,
        expected_user: Option<&str>,
        next_lit: Option<&str>,
    ) {
        let mut user = user.map(|user| user.into());

        let mut iter = input.chars().peekable();
        if let Some(rest) = rest {
            assert!(matcher.matches(&mut iter, &mut user, next_lit));
            assert_eq!(
                iter.collect::<String>(),
                rest,
                "expected {rest} for {input}"
            );
        } else {
            assert!(!matcher.matches(&mut iter, &mut user, next_lit));
        }

        assert_eq!(
            user,
            expected_user.map(|expected_user| expected_user.into())
        );
    }

    #[test]
    fn lit() {
        let matcher = PathMatcherPart::Literal("/some/literal".into());

        test(&matcher, "/some/literal", Some(""));
        test(&matcher, "/some/literal-path", Some("-path"));

        test(&matcher, "/some/litera", None);
        test(&matcher, "/some/literaa", None);
    }

    #[test]
    fn uuid() {
        let matcher = PathMatcherPart::Uuid(Case::Lower);

        test(&matcher, "123e4567-e89b-12d3-a456-426614174000", Some(""));
        test(&matcher, "123e4567-e89b-12d3-a456-4266141740005", Some("5"));

        test(&matcher, "123e4567-e89b-12d3-a456-42661417400", None);
        test(&matcher, "123e4567-e89b-12d3-a456-42661417400x", None);
        test(&matcher, "123E4567-E89B-12D3-A456-426614174000", None);
    }

    #[test]
    fn locale() {
        let matcher = PathMatcherPart::Locale(Case::Mixed);

        test(&matcher, "en-US", Some(""));
        test(&matcher, "en-us", None);
        test(&matcher, "EN-US", None);
        test(&matcher, "de-DE", Some(""));
        test(&matcher, "it", Some(""));
        test(&matcher, "en-US.", Some("."));
        test(&matcher, "enUS", Some("US"));
        test(&matcher, "it.", Some("."));
        test(&matcher, "i", None);
        test(&matcher, "en-U", None);
        test(&matcher, "en-", None);

        let matcher = PathMatcherPart::Locale(Case::Lower);

        test(&matcher, "en-US", None);
        test(&matcher, "en-us", Some(""));
        test(&matcher, "EN-US", None);
        test(&matcher, "de-DE", None);
        test(&matcher, "it", Some(""));
        test(&matcher, "en-US.", None);
        test(&matcher, "enUS", Some("US"));
        test(&matcher, "it.", Some("."));
        test(&matcher, "i", None);
        test(&matcher, "en-U", None);
        test(&matcher, "en-", None);

        let matcher = PathMatcherPart::Locale(Case::Upper);

        test(&matcher, "en-US", None);
        test(&matcher, "en-us", None);
        test(&matcher, "EN-US", Some(""));
        test(&matcher, "de-DE", None);
        test(&matcher, "it", None);
        test(&matcher, "en-US.", None);
        test(&matcher, "enUS", None);
        test(&matcher, "it.", None);
        test(&matcher, "i", None);
        test(&matcher, "en-U", None);
        test(&matcher, "en-", None);
    }

    #[test]
    fn assembly_version() {
        let matcher = PathMatcherPart::AssemblyVersion;

        test(&matcher, "0.0.0.0", Some(""));
        test(&matcher, "1.5.1254.0", Some(""));
        test(&matcher, "1.2.34.567abc", Some("abc"));

        test(&matcher, "0.0..0", None);
        test(&matcher, "ABC", None);
    }

    #[test]
    fn digit() {
        let matcher = PathMatcherPart::Digit {
            min_len: None,
            max_len: None,
        };

        test(&matcher, "", Some(""));
        test(&matcher, "1", Some(""));
        test(&matcher, "123456789", Some(""));
        test(&matcher, "007", Some(""));
        test(&matcher, "42abc", Some("abc"));
        test(&matcher, "abc", Some("abc"));

        let matcher = PathMatcherPart::Digit {
            min_len: Some(1),
            max_len: None,
        };

        test(&matcher, "1", Some(""));
        test(&matcher, "123456789", Some(""));
        test(&matcher, "007", Some(""));
        test(&matcher, "42abc", Some("abc"));

        test(&matcher, "", None);
        test(&matcher, "abc", None);

        let matcher = PathMatcherPart::Digit {
            min_len: Some(1),
            max_len: Some(2),
        };

        test(&matcher, "1", Some(""));
        test(&matcher, "123456789", Some("3456789"));
        test(&matcher, "007", Some("7"));
        test(&matcher, "42abc", Some("abc"));

        test(&matcher, "", None);
        test(&matcher, "abc", None);

        let matcher = PathMatcherPart::Digit {
            min_len: None,
            max_len: Some(2),
        };

        test(&matcher, "", Some(""));
        test(&matcher, "1", Some(""));
        test(&matcher, "123456789", Some("3456789"));
        test(&matcher, "007", Some("7"));
        test(&matcher, "42abc", Some("abc"));
        test(&matcher, "abc", Some("abc"));
    }

    #[test]
    fn hexdigit() {
        let matcher = PathMatcherPart::HexDigit {
            case: Case::Upper,
            min_len: Some(4),
            max_len: Some(4),
        };

        test(&matcher, "0000", Some(""));
        test(&matcher, "ABCD", Some(""));
        test(&matcher, "B00Fers", Some("ers"));
        test(&matcher, "ABCDE", Some("E"));

        test(&matcher, "abcd", None);
        test(&matcher, "ABC", None);
    }

    #[test]
    fn alpha() {
        let matcher = PathMatcherPart::AlphaOrAlphanumeric {
            contains_digits: false,
            case: Case::Lower,
            extra_allowed_chars: Vec::new(),
            min_len: Some(1),
            max_len: None,
        };

        test(&matcher, "abc", Some(""));
        test(&matcher, "aoseht", Some(""));
        test(&matcher, "blabber0yes", Some("0yes"));
        test(&matcher, "ABCDE", None);
        test(&matcher, "soBIG", Some("BIG"));
        test(&matcher, "0", None);

        test(&matcher, "", None);
        test(&matcher, "+", None);

        let matcher = PathMatcherPart::AlphaOrAlphanumeric {
            contains_digits: false,
            case: Case::Mixed,
            extra_allowed_chars: Vec::new(),
            min_len: Some(1),
            max_len: None,
        };

        test(&matcher, "abc", Some(""));
        test(&matcher, "aoseht", Some(""));
        test(&matcher, "blabber0yes", Some("0yes"));
        test(&matcher, "ABCDE", Some(""));
        test(&matcher, "soBIG", Some(""));
        test(&matcher, "0", None);

        test(&matcher, "", None);
        test(&matcher, "+", None);
    }

    #[test]
    fn alphanum() {
        let matcher = PathMatcherPart::AlphaOrAlphanumeric {
            contains_digits: true,
            case: Case::Mixed,
            extra_allowed_chars: Vec::new(),
            min_len: Some(1),
            max_len: None,
        };

        test(&matcher, "abc", Some(""));
        test(&matcher, "aoseht", Some(""));
        test(&matcher, "blabber0yes", Some(""));
        test(&matcher, "ABCDE", Some(""));
        test(&matcher, "soBIG", Some(""));
        test(&matcher, "0", Some(""));

        test(&matcher, "", None);
        test(&matcher, "+", None);
    }

    #[test]
    fn alphanum_extra_chars() {
        let matcher = PathMatcherPart::AlphaOrAlphanumeric {
            contains_digits: true,
            case: Case::Mixed,
            extra_allowed_chars: vec!['-'],
            min_len: Some(1),
            max_len: None,
        };

        test(&matcher, "abc", Some(""));
        test(&matcher, "aos-eht", Some(""));
        test(&matcher, "bl-abber0yes", Some(""));
        test(&matcher, "ABC-DE", Some(""));
        test(&matcher, "so-BIG", Some(""));
        test(&matcher, "----0----", Some(""));

        test(&matcher, "", None);
        test(&matcher, "+", None);
    }

    #[test]
    fn username() {
        let matcher = PathMatcherPart::Username;

        test_with_user(&matcher, "user", Some(""), None, Some("user"), None);
        test_with_user(&matcher, "user/", Some("/"), None, Some("user"), None);
        test_with_user(
            &matcher,
            "userbla",
            Some("bla"),
            Some("user"),
            Some("user"),
            None,
        );
        test_with_user(
            &matcher,
            "All Userss",
            Some(""),
            None,
            Some("All Userss"),
            None,
        );
        test_with_user(&matcher, "All User", Some(""), None, Some("All User"), None);

        test_with_user(
            &matcher,
            "userblablub",
            Some("blablub"),
            None,
            Some("user"),
            Some("bla"),
        );
        test_with_user(
            &matcher,
            "userblablub",
            Some(""),
            None,
            Some("userblablub"),
            Some("ble"),
        );

        test_with_user(&matcher, "user/", None, Some("user2"), Some("user2"), None);

        test_with_user(&matcher, "All Users", None, None, None, None);
        test_with_user(&matcher, "Default User", None, None, None, None);
        test_with_user(&matcher, "desktop.ini", None, None, None, None);
        test_with_user(&matcher, "Public", None, None, None, None);
        test_with_user(&matcher, "Default", None, None, None, None);
        test_with_user(&matcher, "defaultuser0", None, None, None, None);
    }

    #[test]
    fn flexibility_order() {
        let parts = [
            PathMatcherPart::Literal("".into()),
            PathMatcherPart::Username,
            PathMatcherPart::Locale(Case::Mixed),
            PathMatcherPart::Uuid(Case::Lower),
            PathMatcherPart::Uuid(Case::Upper),
            PathMatcherPart::Uuid(Case::Mixed),
            PathMatcherPart::AssemblyVersion,
            PathMatcherPart::Digit {
                min_len: Some(1),
                max_len: Some(1),
            },
            PathMatcherPart::HexDigit {
                case: Case::Lower,
                min_len: Some(1),
                max_len: Some(1),
            },
            PathMatcherPart::HexDigit {
                case: Case::Upper,
                min_len: Some(1),
                max_len: Some(1),
            },
            PathMatcherPart::HexDigit {
                case: Case::Mixed,
                min_len: Some(1),
                max_len: Some(1),
            },
            PathMatcherPart::AlphaOrAlphanumeric {
                contains_digits: false,
                case: Case::Lower,
                extra_allowed_chars: Vec::new(),
                min_len: Some(1),
                max_len: Some(1),
            },
            PathMatcherPart::AlphaOrAlphanumeric {
                contains_digits: false,
                case: Case::Mixed,
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
            PathMatcherPart::Digit {
                min_len: None,
                max_len: Some(1),
            },
            PathMatcherPart::Digit {
                min_len: None,
                max_len: Some(5),
            },
            PathMatcherPart::Digit {
                min_len: Some(2),
                max_len: None,
            },
            PathMatcherPart::Digit {
                min_len: Some(1),
                max_len: None,
            },
            PathMatcherPart::Digit {
                min_len: None,
                max_len: None,
            },
            PathMatcherPart::HexDigit {
                case: Case::Upper,
                min_len: Some(1),
                max_len: None,
            },
            PathMatcherPart::HexDigit {
                case: Case::Upper,
                min_len: None,
                max_len: None,
            },
            PathMatcherPart::AlphaOrAlphanumeric {
                contains_digits: true,
                case: Case::Mixed,
                extra_allowed_chars: Vec::new(),
                min_len: None,
                max_len: None,
            },
            PathMatcherPart::AlphaOrAlphanumeric {
                contains_digits: true,
                case: Case::Mixed,
                extra_allowed_chars: vec!['-'],
                min_len: None,
                max_len: None,
            },
        ];

        let mut sorted = parts.clone();
        sorted.sort_by_key(|part| part.flexibility());

        assert_eq!(parts, sorted);
    }
}
