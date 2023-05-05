//! Support for handling paths.

/// Computes the longest common prefix between two paths.
pub(crate) fn longest_common_prefix<'p>(path1: &'p str, path2: &'p str) -> &'p str {
    for ((i, c1), c2) in path1.char_indices().zip(path2.chars()) {
        if c1 != c2 {
            return &path1[..i];
        }
    }

    if path1.len() > path2.len() {
        path2
    } else {
        path1
    }
}

/// Computes the longest common suffix between two paths.
pub(crate) fn longest_common_suffix<'p>(path1: &'p str, path2: &'p str) -> &'p str {
    let mut last_i = path1.len();
    for ((i, c1), c2) in path1.char_indices().rev().zip(path2.chars().rev()) {
        if c1 != c2 {
            return &path1[last_i..];
        }
        last_i = i;
    }

    if path1.len() > path2.len() {
        path2
    } else {
        path1
    }
}

/// Escapes the given path to be suitable in use with globs.
pub(crate) fn glob_escape(path: &str) -> String {
    path.chars().fold(String::new(), |mut s, c| {
        let (escape, c) = glob_escape_char(c);
        s.extend(escape);
        s.push(c);
        s
    })
}

/// Escapes the given path to be suitable in use with globs.
pub(crate) fn glob_escape_char(c: char) -> (Option<char>, char) {
    if matches!(c, '?' | '*' | '[' | ']' | '{' | '}' | '!' | '^' | '\\') {
        (Some('\\'), c)
    } else {
        (None, c)
    }
}
