//! Support for handling paths and globs.

mod piece;

use crate::{
    file::Files,
    rules::{Rule, RuleStorage},
};

use self::piece::{Case, PieceType};

pub(crate) use self::piece::GlobPiece;

/// Returns `true` if the given character is a special character in globs.
pub(crate) fn is_glob_special_char(c: char) -> bool {
    matches!(c, '?' | '*' | '[' | ']' | '{' | '}' | '!' | '^' | '\\')
}

/// Escapes the given path to be suitable in use with globs.
pub(crate) fn glob_escape(path: &str) -> String {
    path.chars().flat_map(glob_escape_char).collect()
}

/// Escapes the given path to be suitable in use with globs.
pub(crate) fn glob_escape_char(c: char) -> impl Iterator<Item = char> {
    is_glob_special_char(c)
        .then_some('\\')
        .into_iter()
        .chain(std::iter::once(c))
}

/// Returns true if the given glob matches the given path.
pub(crate) fn glob_matches(glob: &str, path: &str) -> bool {
    glob_match::glob_match(glob, path)
}

/// The possible types of replacements for a glob piece.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum Replacement {
    /// The glob piece is left in its original state.
    None,
    /// The glob piece is replaced with a pattern matching any decimal digits.
    Digit {
        /// How many digits are expected.
        len: usize,
    },
    /// The glob piece is replaced with a pattern matching any lower case hexadecimal digits.
    Hex {
        /// The case of the decimal hex digits.
        case: Case,
        /// How many lower hex digits are expected.
        len: usize,
    },
    /// The glob piece is being replaced with a pattern matching a UUID.
    Uuid {
        /// The case of the decimal hex digits.
        case: Case,
    },
    /// The glob piece is being replaced with a pattern matching any characters.
    Any {
        /// Optionally match only the given fixed length.
        len: Option<usize>,
    },
}

impl Replacement {
    /// Returns `true` if the replacement can be automatically applied.
    fn automatically_applicable(&self) -> bool {
        match self {
            Replacement::None
            | Replacement::Digit { .. }
            | Replacement::Hex { .. }
            | Replacement::Uuid { .. } => true,
            Replacement::Any { .. } => false,
        }
    }

    /// Displays the replacement.
    fn display_with_original(self, original: &str) -> String {
        match self {
            Replacement::None => original.to_string(),
            Replacement::Digit { len: 1 } => "<digit>".to_string(),
            Replacement::Digit { len } => format!("<digit*{len}>"),
            Replacement::Hex { case, len: 1 } => match case {
                Case::Upper => "<HEX>".to_string(),
                Case::Lower => "<hex>".to_string(),
                Case::Mixed => "<Hex>".to_string(),
            },
            Replacement::Hex { case, len } => match case {
                Case::Upper => format!("<HEX*{len}>"),
                Case::Lower => format!("<hex*{len}>"),
                Case::Mixed => format!("<Hex*{len}>"),
            },
            Replacement::Uuid { case } => match case {
                Case::Upper => "<UUID>".to_string(),
                Case::Lower => "<uuid>".to_string(),
                Case::Mixed => "<Uuid>".to_string(),
            },
            Replacement::Any { len: Some(len) } => "?".repeat(len),
            Replacement::Any { len: None } => "*".to_string(),
        }
    }
}

/// Returns an iterator over pieces of a glob that may likely be subject to changes.
pub(crate) fn glob_pieces(glob: &str) -> Vec<GlobPiece> {
    let mut pieces = Vec::new();
    let mut piece_start = 0;
    let mut piece_type = None;
    let mut escape_idx = None;
    let mut paren_stack = Vec::new();

    for (i, c) in glob.char_indices() {
        let was_escaped = escape_idx.is_some();
        let (new_piece_type, should_break) = if c == '\\' {
            escape_idx = Some(i);
            (piece_type, false)
        } else if piece_type == Some(PieceType::GlobPart) && !paren_stack.is_empty() {
            if c == ']' && paren_stack.last() == Some(&'[')
                || c == '}' && paren_stack.last() == Some(&'{')
            {
                paren_stack.pop();
            }
            (Some(PieceType::GlobPart), false)
        } else if is_glob_special_char(c) {
            if was_escaped {
                (Some(PieceType::Symbol), true)
            } else {
                if c == '[' || c == '{' {
                    paren_stack.push(c);
                }
                (
                    Some(PieceType::GlobPart),
                    !glob[..i].ends_with('?') || c != '?',
                )
            }
        } else if c.is_ascii_hexdigit() && !c.is_ascii_digit() {
            let new_case = c.try_into().unwrap_or(Case::Mixed);
            match piece_type {
                Some(PieceType::Digit) => (Some(PieceType::Hex { case: new_case }), false),
                Some(PieceType::Hex { case }) => (
                    Some(PieceType::Hex {
                        case: case + new_case,
                    }),
                    false,
                ),
                Some(PieceType::Alpha { case }) => (
                    Some(PieceType::Alpha {
                        case: case + new_case,
                    }),
                    false,
                ),
                Some(PieceType::AlphaHex { case }) => (
                    Some(PieceType::AlphaHex {
                        case: case + new_case,
                    }),
                    false,
                ),
                _ => (
                    Some(PieceType::AlphaHex {
                        case: c.try_into().unwrap_or(Case::Mixed),
                    }),
                    true,
                ),
            }
        } else if c.is_alphabetic() {
            let new_case = c.try_into().unwrap_or(Case::Mixed);
            match piece_type {
                Some(PieceType::Alpha { case } | PieceType::AlphaHex { case }) => (
                    Some(PieceType::Alpha {
                        case: case + new_case,
                    }),
                    false,
                ),
                _ => (Some(PieceType::Alpha { case: new_case }), true),
            }
        } else if c.is_ascii_digit() {
            match piece_type {
                Some(PieceType::AlphaHex { case } | PieceType::Hex { case }) => {
                    (Some(PieceType::Hex { case }), false)
                }
                Some(PieceType::Digit) => (Some(PieceType::Digit), false),
                _ => (Some(PieceType::Digit), true),
            }
        } else if c == '/' {
            (Some(PieceType::Slash), true)
        } else {
            (Some(PieceType::Symbol), true)
        };

        if should_break {
            let idx = escape_idx.unwrap_or(i);
            if idx != 0 {
                pieces.push((&glob[piece_start..idx], piece_type.unwrap()));
            }
            piece_start = idx;
        }

        piece_type = new_piece_type;
        if was_escaped {
            escape_idx = None;
        }
    }
    if let Some(piece_type) = piece_type {
        pieces.push((&glob[piece_start..], piece_type));
    }

    loop {
        let Some((piece_idx, case)) = ('uuid: {
            for (i, window) in pieces.windows(9).enumerate() {
                let [(p1, t1), ("-", _), (p2, t2), ("-", _), (p3, t3), ("-", _), (p4, t4), ("-", _), (p5, t5)] = window else { continue; };

                let types = [t1, t2, t3, t4, t5];

                if types.iter().any(|t| !t.is_hex_compatible()) {
                    continue;
                }
                if [p1.len(), p2.len(), p3.len(), p4.len(), p5.len()] != [8, 4, 4, 4, 12] {
                    continue;
                }

                let first_case = types.iter().find_map(|t| t.case());
                let case = if let Some(first_case) = first_case {
                    let mut case = first_case;
                    for t in &types {
                        case = case + t.case();
                    }
                    Some(case)
                } else {
                    None
                };

                break 'uuid Some((i, case));
            }

            None
        }) else { break };

        let start_char_idx = pieces.iter().take(piece_idx).map(|(p, _)| p.len()).sum();
        pieces.drain(piece_idx..piece_idx + 9);
        pieces.insert(
            piece_idx,
            (
                &glob[start_char_idx..start_char_idx + 36],
                PieceType::Uuid {
                    case: case.unwrap_or(Case::Mixed),
                },
            ),
        );
    }

    pieces
        .into_iter()
        .map(|(piece, piece_type)| GlobPiece::new(piece, piece_type))
        .collect()
}

/// Converts a list of glob pieces to a string.
pub(crate) fn pieces_to_str(pieces: &[GlobPiece]) -> String {
    let mut result = String::new();
    for piece in pieces {
        result.push_str(piece.current_str());
    }
    result
}

/// Suggests a glob for a new rule that isn't already matched by the rules.
pub(crate) fn suggest_glob(files: &Files, rules: &RuleStorage) -> Option<Vec<GlobPiece>> {
    for (path, file) in files.chronological_order() {
        if rules.is_matched(file) {
            continue;
        }

        let escaped = glob_escape(path);
        let pieces = glob_pieces(&escaped);

        let mut suggestion = Vec::new();
        let mut is_users = false;
        for (piece_num, mut piece) in pieces.iter().cloned().enumerate() {
            if piece.r#type == PieceType::Slash {
                suggestion.push(piece);
                continue;
            }
            if piece_num == 1 && piece.unescaped_original().as_deref() == Some("Users") {
                is_users = true;
            }

            if is_users && piece_num == 3 {
                piece.apply_replacement(Replacement::Any { len: None });
                suggestion.push(piece);
                continue;
            }

            let possible_replacements = piece.possible_replacements();
            let mut count_matches = |replacement: Replacement| {
                let mut total_glob = pieces_to_str(&suggestion);
                piece.apply_replacement(replacement);
                total_glob.push_str(piece.current_str());
                for piece in pieces.iter().skip(piece_num + 1) {
                    total_glob.push_str(piece.current_str());
                }

                if let Some(rule) = Rule::from_glob(&total_glob, files) {
                    let storage = RuleStorage::from_rule(rule);

                    files
                        .chronological_order()
                        .filter(|(_, file)| storage.is_matched(file))
                        .count()
                } else {
                    0
                }
            };

            let mut max_replacement = None;
            let mut max_count = 0;

            for replacement in possible_replacements {
                if !replacement.automatically_applicable() {
                    continue;
                }

                let count = count_matches(replacement);
                // the replacements are ordered from most specific to least specific
                // we want the most specific one with still the best count
                // that is why we choose `>` here instead of `>=`
                if count > max_count {
                    max_count = count;
                    max_replacement = Some(replacement);
                }
            }

            piece.apply_replacement(max_replacement.unwrap_or(Replacement::None));

            suggestion.push(piece);
        }

        return Some(suggestion);
    }

    None
}
