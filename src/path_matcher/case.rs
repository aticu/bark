//! This module deals with the case of text.

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

impl Case {
    /// Whether the case matches the given character.
    pub(crate) fn matches_char(self, c: char) -> bool {
        match self {
            Case::Upper => !c.is_lowercase(),
            Case::Lower => !c.is_uppercase(),
            Case::Mixed => true,
        }
    }
}
