//! Implements an API for a partial blob.

use std::borrow::Cow;

use super::Replacement;

/// The case of text.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum Case {
    /// The text is fully upper case.
    Upper,
    /// The text is fully lower case.
    Lower,
    /// The text is mixed case.
    Mixed,
}

impl std::ops::Add<Case> for Case {
    type Output = Case;

    fn add(self, rhs: Case) -> Self::Output {
        match (self, rhs) {
            (Case::Lower, Case::Lower) => Case::Lower,
            (Case::Upper, Case::Upper) => Case::Upper,
            _ => Case::Mixed,
        }
    }
}

impl std::ops::Add<Option<Case>> for Case {
    type Output = Case;

    fn add(self, rhs: Option<Case>) -> Self::Output {
        match rhs {
            Some(rhs) => self + rhs,
            None => self,
        }
    }
}

impl TryFrom<char> for Case {
    type Error = &'static str;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        if value.is_lowercase() {
            Ok(Case::Lower)
        } else if value.is_uppercase() {
            Ok(Case::Upper)
        } else {
            Err("char doesn't have a case")
        }
    }
}

/// The type of a single piece of a glob.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum PieceType {
    /// The piece is a slash.
    Slash,
    /// The piece is a symbol.
    Symbol,
    /// The consists only of digits.
    Digit,
    /// The piece consists of alphabetic characters.
    Alpha {
        /// The case of the text.
        case: Case,
    },
    /// The piece consists only of hexadecimal letters.
    AlphaHex {
        /// The case of the possible hex letters.
        case: Case,
    },
    /// The piece consists of hexadecimal letters.
    Hex {
        /// The case of the hex letters.
        case: Case,
    },
    /// The piece is a UUID.
    Uuid {
        /// The case of the UUID hex letters.
        case: Case,
    },
    /// The piece is some glob syntax.
    GlobPart,
}

impl PieceType {
    /// Returns `true` if the piece type can be a valid hex digit.
    pub(crate) fn is_hex_compatible(&self) -> bool {
        matches!(
            self,
            PieceType::AlphaHex { .. } | PieceType::Hex { .. } | PieceType::Digit
        )
    }

    /// Returns the case of this piece type if any.
    pub(crate) fn case(&self) -> Option<Case> {
        match self {
            PieceType::Alpha { case } | PieceType::Hex { case } | PieceType::AlphaHex { case } => {
                Some(*case)
            }
            _ => None,
        }
    }
}

/// A single piece of a glob that should be treated as a unit.
#[derive(Debug, Clone)]
pub(crate) struct GlobPiece {
    /// The original glob piece, before any changes were applied to it.
    pub(crate) original: String,
    /// The unescaped length of the glob piece.
    pub(crate) unescaped_len: Option<usize>,
    /// The original glob piece type, before any changes were applied to it.
    pub(crate) r#type: PieceType,
    /// The replacement being used for the glob piece.
    replacement: Replacement,
    /// The current text of the given glob piece.
    current: String,
}

impl GlobPiece {
    /// Creates a new glob piece.
    pub(crate) fn new(piece: &str, r#type: PieceType) -> Self {
        Self {
            original: piece.to_string(),
            unescaped_len: if r#type == PieceType::GlobPart {
                None
            } else {
                let (escaped, len) = piece.chars().fold((false, 0), |(escaped, count), c| {
                    if escaped {
                        (false, count + 1)
                    } else if c == '\\' {
                        (true, count)
                    } else {
                        (false, count + 1)
                    }
                });

                (!escaped).then_some(len)
            },
            r#type,
            replacement: Replacement::None,
            current: piece.to_string(),
        }
    }

    /// The unescaped original glob piece.
    pub(crate) fn unescaped_original(&self) -> Option<Cow<str>> {
        if self.r#type != PieceType::GlobPart {
            if self.original.contains('\\') {
                let mut escaped = false;
                let mut res = String::new();

                for c in self.original.chars() {
                    if escaped {
                        res.push(c);
                        escaped = false;
                        continue;
                    }

                    if c == '\\' {
                        escaped = true;
                    } else {
                        res.push(c);
                    }
                }

                Some(Cow::Owned(res))
            } else {
                Some(Cow::Borrowed(&self.original))
            }
        } else {
            None
        }
    }

    /// Returns all the possible replacements for this glob piece.
    ///
    /// The replacements are ordered by specificity, such that the most specific one comes first and
    /// the most general one comes last.
    pub(crate) fn possible_replacements(&self) -> Vec<Replacement> {
        let mut result = vec![Replacement::None];

        match self.r#type {
            PieceType::Slash
            | PieceType::Symbol
            | PieceType::Alpha { .. }
            | PieceType::GlobPart => (),
            PieceType::Digit => {
                if let Some(len) = self.unescaped_len {
                    result.push(Replacement::Digit { len });
                    result.push(Replacement::Hex {
                        case: Case::Lower,
                        len,
                    });
                    result.push(Replacement::Hex {
                        case: Case::Upper,
                        len,
                    });
                    result.push(Replacement::Hex {
                        case: Case::Mixed,
                        len,
                    });
                }
            }
            PieceType::AlphaHex { case } | PieceType::Hex { case } => {
                if let Some(len) = self.unescaped_len {
                    if case == Case::Lower {
                        result.push(Replacement::Hex {
                            case: Case::Lower,
                            len,
                        });
                    }
                    if case == Case::Upper {
                        result.push(Replacement::Hex {
                            case: Case::Upper,
                            len,
                        });
                    }
                    result.push(Replacement::Hex {
                        case: Case::Mixed,
                        len,
                    });
                }
            }
            PieceType::Uuid { case } => result.push(Replacement::Uuid { case }),
        }

        if self.r#type != PieceType::Slash {
            if let Some(unescaped_len) = self.unescaped_len {
                result.push(Replacement::Any {
                    len: Some(unescaped_len),
                });
            }
            if self.original != "*" {
                result.push(Replacement::Any { len: None });
            }
        }

        result
    }

    /// Applies the given replacement to self.
    pub(crate) fn apply_replacement(&mut self, replacement: Replacement) {
        if self.replacement != replacement {
            let hex_base = |case| match case {
                Case::Upper => "[A-Z0-9]",
                Case::Lower => "[a-z0-9]",
                Case::Mixed => "[a-zA-Z0-9]",
            };

            self.current = match replacement {
                Replacement::None => self.original.clone(),
                Replacement::Digit { len } => "[0-9]".repeat(len),
                Replacement::Hex { case, len } => hex_base(case).repeat(len),
                Replacement::Uuid { case } => format!("{d}{d}{d}{d}{d}{d}{d}{d}-{d}{d}{d}{d}-{d}{d}{d}{d}-{d}{d}{d}{d}-{d}{d}{d}{d}{d}{d}{d}{d}{d}{d}{d}{d}", d = hex_base(case)),
                Replacement::Any { len: Some(len) } => "?".repeat(len),
                Replacement::Any { len: None } => "*".to_string(),
            };
            self.replacement = replacement;
        }
    }

    /// Returns a string representation for the given replacement in this glob piece.
    pub(crate) fn display_replacement(&self, replacement: Replacement) -> String {
        replacement.display_with_original(&self.original)
    }

    /// Returns the current representation of the glob piece as a string.
    pub(crate) fn current_str(&self) -> &str {
        &self.current
    }

    /// Returns the current replacement of the glob piece.
    pub(crate) fn current_replacement(&self) -> Replacement {
        self.replacement
    }
}
