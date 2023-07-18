//! Iterators over the `FsTree` type.

use inlinable_string::InlinableString;

use super::FsTree;

/// A semi-iterator over a `FsTree`.
///
/// This type does not implement `Iterator`, because that would make it impossible to change the
/// tree during iteration.
/// Also it would prohibit using `self` for something else in a type that contains a `FsTree`.
///
/// With this design, all the iteration state is stored independent of the tree, which allows for
/// more flexible borrowing.
/// However it also means that care must be taken not to modify the structure of the tree, as that
/// could break the iteration.
#[derive(Debug)]
pub(crate) struct FsTreeIter {
    /// The current directory through which the iterator is iterating.
    dir: String,
    /// The name of the current file within `dir`.
    file_name: Option<InlinableString>,
    /// The names of all current parent directories
    parents: Vec<(String, bool)>,
    /// Whether we need to ascend out of the current directory in the next iteration.
    needs_ascend: bool,
}

impl FsTreeIter {
    /// Creates a new iterator over an arbitrary `FsTree`.
    pub(crate) fn new() -> Self {
        FsTreeIter {
            dir: String::from("/"),
            file_name: None,
            parents: vec![(String::from("/"), false)],
            needs_ascend: false,
        }
    }

    /// Advances the iterator to the next element in the tree.
    pub(crate) fn next<'tree, T>(
        &'tree mut self,
        tree: &'tree mut FsTree<T>,
    ) -> Option<FsTreeIterResult<'tree, T>> {
        let mut skip_finished_dir = false;

        if self.needs_ascend {
            while matches!(self.parents.last(), Some((_, true))) {
                self.parents.pop();
                let last_segment_len = self.dir.split('/').next_back().unwrap().len();
                self.file_name = Some(self.dir[self.dir.len() - last_segment_len..].into());
                self.dir
                    .truncate((self.dir.len() - last_segment_len - 1).max(1));
            }
            self.needs_ascend = false;
            skip_finished_dir = true;
        }

        if self.parents.is_empty() {
            return None;
        }

        let (value, has_children, should_descend, next_file) =
            match tree.get_mut(&self.dir).unwrap() {
                FsTree::Leaf(val) => (val, false, false, None),
                FsTree::Inner { val, map } => match self.file_name.as_ref() {
                    None if self.dir == "/" => {
                        let has_children = !map.is_empty();
                        let mut iter = map.iter_mut();
                        (val, has_children, false, iter.next())
                    }
                    _ => {
                        let mut iter = map.range_mut(self.file_name.clone().unwrap_or("".into())..);
                        if skip_finished_dir {
                            iter.next();
                        }
                        // the current file should always be a valid file
                        let (name, current_file_tree) = iter.next().unwrap();
                        if self.file_name.is_none() || skip_finished_dir {
                            self.file_name = Some(name.clone());
                        }
                        let (val, should_descend) = match current_file_tree {
                            FsTree::Leaf(val) => (val, false),
                            FsTree::Inner { val, map } => (val, !map.is_empty()),
                        };
                        (val, should_descend, should_descend, iter.next())
                    }
                },
            };

        let next_file_name = next_file.map(|(name, _)| name.clone());
        if next_file_name.is_none() {
            self.parents.last_mut().unwrap().1 = true;
        }

        let name = if self.dir == "/" && self.file_name.is_none() {
            "/"
        } else if let Some(current_file) = &self.file_name {
            current_file
        } else {
            self.dir.split('/').next_back().unwrap()
        }
        .into();

        if should_descend {
            if !self.dir.ends_with('/') {
                self.dir.push('/');
            }
            self.dir.push_str(self.file_name.as_ref().unwrap());
            self.parents.push((self.dir.clone(), false));
            self.file_name = None;
        } else if next_file_name.is_some() {
            self.file_name = next_file_name;
        } else {
            self.needs_ascend = true;
        }

        let parents = if should_descend {
            &self.parents[..self.parents.len() - 1]
        } else {
            &self.parents
        };

        Some(FsTreeIterResult {
            value,
            name,
            parents,
            has_children,
        })
    }
}

/// The result of a single iteration of an `FsTree`.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct FsTreeIterResult<'tree, T> {
    /// The value of the current node in the tree.
    pub(crate) value: &'tree mut T,
    /// The name of the current node in the tree.
    pub(crate) name: InlinableString,
    /// Wether the given entry has children.
    pub(crate) has_children: bool,
    /// All parents of the current node in the tree.
    ///
    /// The `String` contains the full path of the parent, whereas the `bool` indicates whether the
    /// iteration is at the child node within that parent.
    pub(crate) parents: &'tree [(String, bool)],
}

impl<'tree, T> FsTreeIterResult<'tree, T> {
    /// Returns the path of this entry.
    pub(crate) fn path(&self) -> String {
        let parent = &self.parents.last().unwrap().0;
        let name = &self.name;

        if name == "/" {
            String::from("/")
        } else if parent == "/" {
            format!("/{name}")
        } else {
            format!("{parent}/{name}")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{super::FsTree, FsTreeIter, FsTreeIterResult};

    #[test]
    fn iter() {
        let mut tree = FsTree::<usize>::new();

        tree.insert("/root_val", 1);
        tree.insert("/root_val2", 2);
        tree.insert("/some", 3);
        tree.insert("/some/artifact", 4);
        tree.insert("/some/deep/stuff", 5);
        tree.insert("/some/deep/value", 6);
        tree.insert("/some/other/thing", 7);
        tree.insert("/some/other/value", 8);
        tree.insert("/val3", 9);

        let mut iter = FsTreeIter::new();

        macro_rules! iter_assert {
            (
                val: $val:expr,
                name: $path:expr,
                has_children: $has_children:expr,
                [$({dir: $dir:expr, last_in_dir: $is_last:expr}),*$(,)?]
            ) => {
                assert_eq!(
                    iter.next(&mut tree),
                    Some(FsTreeIterResult {
                        value: &mut $val,
                        name: $path.into(),
                        has_children: $has_children,
                        parents: &[$((String::from($dir), $is_last)),*][..]
                    })
                );
            };
        }

        iter_assert! {
            val: 0,
            name: "/",
            has_children: true,
            [{ dir: "/", last_in_dir: false }]
        };
        iter_assert! {
            val: 1,
            name: "root_val",
            has_children: false,
            [{ dir: "/", last_in_dir: false }]
        };
        iter_assert! {
            val: 2,
            name: "root_val2",
            has_children: false,
            [{ dir: "/", last_in_dir: false }]
        };
        iter_assert! {
            val: 3,
            name: "some",
            has_children: true,
            [{ dir: "/", last_in_dir: false }]
        };
        iter_assert! {
            val: 4,
            name: "artifact",
            has_children: false,
            [
                { dir: "/", last_in_dir: false },
                { dir: "/some", last_in_dir: false },
            ]
        };
        iter_assert! {
            val: 0,
            name: "deep",
            has_children: true,
            [
                { dir: "/", last_in_dir: false },
                { dir: "/some", last_in_dir: false },
            ]
        };
        iter_assert! {
            val: 5,
            name: "stuff",
            has_children: false,
            [
                { dir: "/", last_in_dir: false },
                { dir: "/some", last_in_dir: false },
                { dir: "/some/deep", last_in_dir: false },
            ]
        };
        iter_assert! {
            val: 6,
            name: "value",
            has_children: false,
            [
                { dir: "/", last_in_dir: false },
                { dir: "/some", last_in_dir: false },
                { dir: "/some/deep", last_in_dir: true },
            ]
        };
        iter_assert! {
            val: 0,
            name: "other",
            has_children: true,
            [
                { dir: "/", last_in_dir: false },
                { dir: "/some", last_in_dir: true },
            ]
        };
        iter_assert! {
            val: 7,
            name: "thing",
            has_children: false,
            [
                { dir: "/", last_in_dir: false },
                { dir: "/some", last_in_dir: true },
                { dir: "/some/other", last_in_dir: false },
            ]
        };
        iter_assert! {
            val: 8,
            name: "value",
            has_children: false,
            [
                { dir: "/", last_in_dir: false },
                { dir: "/some", last_in_dir: true },
                { dir: "/some/other", last_in_dir: true },
            ]
        };
        iter_assert! {
            val: 9,
            name: "val3",
            has_children: false,
            [
                { dir: "/", last_in_dir: true },
            ]
        };
    }
}
