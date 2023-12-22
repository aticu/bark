//! Implements a tree structure for modelling file systems.

mod iter;

use std::{collections::BTreeMap, mem};

use inlinable_string::InlinableString;

pub(crate) use iter::FsTreeIter;

/// Represents a file system tree structure.
///
/// Can store arbitrary data in each node.
#[derive(Debug, Hash, PartialEq, Eq)]
pub(crate) enum FsTree<T> {
    /// A leaf node in the file system tree.
    Leaf(T),
    /// An inner node could have more children.
    Inner {
        /// The data contained in the inner node.
        val: T,
        /// The map that contains the children of this tree.
        map: BTreeMap<InlinableString, FsTree<T>>,
    },
}

impl<T: Default> Default for FsTree<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Default> FsTree<T> {
    /// Creates a new empty tree.
    pub(crate) fn new() -> Self {
        Self::Leaf(Default::default())
    }

    /// Inserts the given value at the given path.
    pub(crate) fn insert(&mut self, path: &str, val: T) -> Option<T> {
        if path == "/" {
            let root_val = match self {
                FsTree::Leaf(root_val) => root_val,
                FsTree::Inner { val: root_val, .. } => root_val,
            };
            return Some(mem::replace(root_val, val));
        }

        let mut current = self;

        let mut iter = path.strip_prefix('/').unwrap_or(path).split('/').peekable();
        while let Some(segment) = iter.next() {
            let is_last = iter.peek().is_none();

            let map = current.force_inner();

            match (map.get(segment).is_some(), is_last) {
                (true, true) => {
                    return Some(mem::replace(map.get_mut(segment).unwrap().val_mut(), val))
                }
                (true, false) => {
                    current = map.get_mut(segment).unwrap();
                }
                (false, true) => {
                    map.insert(segment.into(), FsTree::Leaf(val));
                    return None;
                }
                (false, false) => {
                    current = map.entry(segment.into()).or_insert(FsTree::Inner {
                        val: Default::default(),
                        map: Default::default(),
                    });
                }
            }
        }

        None
    }

    /// Turns `self` into an inner node.
    ///
    /// If it already was an inner node, nothing is changed.
    /// If it was a leaf node, it's turned into an empty inner node.
    ///
    /// Returns the map of the then guaranteed inner node.
    fn force_inner(&mut self) -> &mut BTreeMap<InlinableString, FsTree<T>> {
        match self {
            FsTree::Leaf(val) => {
                let val = mem::take(val);
                let _ = mem::replace(
                    self,
                    FsTree::Inner {
                        val,
                        map: Default::default(),
                    },
                );

                match self {
                    FsTree::Leaf(_) => unreachable!("we just inserted a value here"),
                    FsTree::Inner { map, .. } => map,
                }
            }
            FsTree::Inner { map, .. } => map,
        }
    }

    /// Creates a clone of this tree, containing only entries where `filter` returns `true` and
    /// their parents.
    pub(crate) fn clone_filtered(
        &self,
        mut filter: impl FnMut(&T, &str) -> FilterResult,
    ) -> FsTree<T>
    where
        T: Clone,
    {
        // This function works by descending into the tree, keeping track of the
        // "speculation_depth"
        // This is the number of parent nodes between the current node and the last node that is
        // definitely included in the tree (0 if the current node is definitely included, 1 if the
        // parent is definitely included etc)
        //
        // When we find a node that is definitely included in the tree, we reset the
        // speculation_depth to 0 and know that we have to add all of the parents later, if the
        // speculation_depth is still greater than 1 when ascending again from a child, we know that
        // we do not need to include it in the resulting tree
        match self {
            Self::Leaf(root_val) => FsTree::Leaf(root_val.clone()),
            Self::Inner { val: root_val, map } => {
                let mut speculation_depth = 0u32;
                struct LevelInfo<
                    'a,
                    T: 'a,
                    I: Iterator<Item = (&'a InlinableString, &'a FsTree<T>)>,
                > {
                    iter: I,
                    val: T,
                    name: InlinableString,
                    level_map: BTreeMap<InlinableString, FsTree<T>>,
                    explicitly_included: bool,
                }
                let mut stack = vec![LevelInfo {
                    iter: map.iter(),
                    val: root_val.clone(),
                    name: InlinableString::from("/"),
                    level_map: BTreeMap::new(),
                    explicitly_included: true,
                }];
                let mut path = String::from("");
                let remove_last_path_component = |path: &mut String| {
                    if let Some(last_slash) = path.rfind('/') {
                        path.truncate(last_slash);
                    }
                };

                while let Some(LevelInfo {
                    iter, level_map, ..
                }) = stack.last_mut()
                {
                    match iter.next() {
                        Some((name, val)) => {
                            path.push('/');
                            path.push_str(name);
                            let mut further_descended = false;
                            match val {
                                FsTree::Leaf(val) => match filter(val, &path) {
                                    FilterResult::Keep | FilterResult::DiscardChildren => {
                                        speculation_depth = 0;
                                        level_map.insert(name.clone(), FsTree::Leaf(val.clone()));
                                    }
                                    FilterResult::Discard => (),
                                },
                                FsTree::Inner { val, map } => match filter(val, &path) {
                                    FilterResult::Keep => {
                                        speculation_depth = 1;
                                        further_descended = true;
                                        stack.push(LevelInfo {
                                            iter: map.iter(),
                                            val: val.clone(),
                                            name: name.clone(),
                                            level_map: BTreeMap::new(),
                                            explicitly_included: true,
                                        });
                                    }
                                    FilterResult::Discard => {
                                        speculation_depth += 1;
                                        further_descended = true;
                                        stack.push(LevelInfo {
                                            iter: map.iter(),
                                            val: val.clone(),
                                            name: name.clone(),
                                            level_map: BTreeMap::new(),
                                            explicitly_included: false,
                                        });
                                    }
                                    FilterResult::DiscardChildren => {
                                        speculation_depth = 0;
                                        level_map.insert(name.clone(), FsTree::Leaf(val.clone()));
                                    }
                                },
                            }
                            // we didn't descend further, so we need to remove the last part of the
                            // path
                            if !further_descended {
                                remove_last_path_component(&mut path);
                            }
                        }
                        None => {
                            let Some(LevelInfo {
                                val,
                                name,
                                explicitly_included,
                                level_map: map,
                                ..
                            }) = stack.pop()
                            else {
                                unreachable!("we're still in the loop, so there exists an element")
                            };
                            remove_last_path_component(&mut path);

                            if let Some(LevelInfo {
                                level_map: outer_map,
                                ..
                            }) = stack.last_mut()
                            {
                                if explicitly_included {
                                    speculation_depth = 0;
                                    if map.is_empty() {
                                        outer_map.insert(name, FsTree::Leaf(val));
                                    } else {
                                        outer_map.insert(name, FsTree::Inner { val, map });
                                    }
                                } else if speculation_depth == 0 {
                                    outer_map.insert(name, FsTree::Inner { val, map });
                                } else {
                                    speculation_depth -= 1;
                                }
                            } else {
                                return FsTree::Inner {
                                    val: root_val.clone(),
                                    map,
                                };
                            }
                        }
                    }
                }

                FsTree::Leaf(root_val.clone())
            }
        }
    }

    /// Visits all values in the tree to allow mutation of them.
    pub(crate) fn mutate_vals(&mut self, mut mutate: impl FnMut(&mut T)) {
        match self {
            FsTree::Leaf(val) => mutate(val),
            FsTree::Inner { val, map } => {
                mutate(val);
                let mut stack = vec![map.values_mut()];

                while let Some(current_dir) = stack.last_mut() {
                    match current_dir.next() {
                        Some(elem) => match elem {
                            FsTree::Leaf(val) => mutate(val),
                            FsTree::Inner { val, map } => {
                                mutate(val);
                                stack.push(map.values_mut());
                            }
                        },
                        None => {
                            stack.pop();
                        }
                    }
                }
            }
        }
    }

    /// Returns a reference to the inner value.
    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) fn val(&self) -> &T {
        match self {
            FsTree::Leaf(val) => val,
            FsTree::Inner { val, .. } => val,
        }
    }

    /// Returns a mutable reference to the inner value.
    pub(crate) fn val_mut(&mut self) -> &mut T {
        match self {
            FsTree::Leaf(val) => val,
            FsTree::Inner { val, .. } => val,
        }
    }
}

/// The result of a filter operation.
#[derive(Debug, PartialEq, Eq, Default, Clone, Copy)]
pub(crate) enum FilterResult {
    /// Keep the entry.
    Keep,
    /// Discard the entry (unless children of it are kept).
    #[default]
    Discard,
    /// Keeps the entry, but discards its children.
    DiscardChildren,
}

macro_rules! impl_get {
    (
        $(
            $(#[$($meta:meta)*])* $vis:vis fn $name:ident(& $($mut:ident)? ...) -> ...;
        )+
    ) => {
        impl<T> FsTree<T> {
            $(
                $(#[$($meta)*])*
                $vis fn $name(& $($mut)? self, path: &str) -> Option<& $($mut)? FsTree<T>> {
                    if path == "/" {
                        return Some(self);
                    }

                    let mut current = self;

                    let iter = path.strip_prefix('/').unwrap_or(path).split('/');
                    for segment in iter {
                        match current {
                            FsTree::Leaf(_) => return None,
                            FsTree::Inner { map, .. } => match map.$name(segment) {
                                Some(tree) => current = tree,
                                None => return None,
                            },
                        }
                    }

                    Some(current)
                }
            )+
        }
    };
}

impl_get! {
    /// Gets a reference to the subtree at the given path.
    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) fn get(& ...) -> ...;

    /// Gets a mutable reference to the subtree at the given path.
    pub(crate) fn get_mut(&mut ...) -> ...;
}

#[cfg(test)]
mod tests {
    use super::{FilterResult, FsTree};

    #[test]
    fn insert_and_get() {
        let mut tree = FsTree::<Option<u64>>::new();

        let get_val =
            |tree: &FsTree<Option<u64>>, path| tree.get(path).map(|tree| tree.val()).copied();

        assert_eq!(get_val(&tree, "/"), Some(None));
        assert_eq!(get_val(&tree, "/some"), None);
        assert_eq!(get_val(&tree, "/some/path"), None);

        assert!(tree.insert("/some/path", Some(0)).is_none());

        assert_eq!(get_val(&tree, "/"), Some(None));
        assert_eq!(get_val(&tree, "/some"), Some(None));
        assert_eq!(get_val(&tree, "/some/path"), Some(Some(0)));

        assert_eq!(tree.insert("/some/path", Some(1)), Some(Some(0)));

        assert_eq!(get_val(&tree, "/"), Some(None));
        assert_eq!(get_val(&tree, "/some"), Some(None));
        assert_eq!(get_val(&tree, "/some/path"), Some(Some(1)));

        assert_eq!(tree.insert("/", Some(2)), Some(None));

        assert_eq!(get_val(&tree, "/"), Some(Some(2)));
        assert_eq!(get_val(&tree, "/some"), Some(None));
        assert_eq!(get_val(&tree, "/some/path"), Some(Some(1)));

        assert_eq!(tree.insert("/some", Some(3)), Some(None));

        assert_eq!(get_val(&tree, "/"), Some(Some(2)));
        assert_eq!(get_val(&tree, "/some"), Some(Some(3)));
        assert_eq!(get_val(&tree, "/some/path"), Some(Some(1)));

        assert_eq!(get_val(&tree, "/some/other_path"), None);
    }

    #[test]
    fn filter_tree() {
        let mut original_tree = FsTree::<FilterResult>::new();

        original_tree.insert("/keep/very/deep/path", FilterResult::Discard);
        original_tree.insert("/parent", FilterResult::Keep);
        original_tree.insert("/parent/child", FilterResult::Discard);
        original_tree.insert("/root_val", FilterResult::Keep);
        original_tree.insert("/root_val2", FilterResult::Keep);
        original_tree.insert("/some", FilterResult::Keep);
        original_tree.insert("/some/artifact", FilterResult::Discard);
        original_tree.insert("/some/deep/stuff", FilterResult::Keep);
        original_tree.insert("/some/deep/value", FilterResult::Discard);
        original_tree.insert("/some/other/thing", FilterResult::Discard);
        original_tree.insert("/some/other/value", FilterResult::Keep);
        original_tree.insert("/totally/deep/stuff", FilterResult::Discard);
        original_tree.insert("/totally/deep/value", FilterResult::Discard);
        original_tree.insert("/val3", FilterResult::Discard);
        original_tree.insert("/x/y", FilterResult::DiscardChildren);
        original_tree.insert("/x/y/z", FilterResult::Keep);
        original_tree.insert("/z", FilterResult::DiscardChildren);

        let mut expected_tree = FsTree::<FilterResult>::new();

        expected_tree.insert("/keep/very/deep/path", FilterResult::Discard);
        expected_tree.insert("/parent", FilterResult::Keep);
        expected_tree.insert("/root_val", FilterResult::Keep);
        expected_tree.insert("/root_val2", FilterResult::Keep);
        expected_tree.insert("/some", FilterResult::Keep);
        expected_tree.insert("/some/deep/stuff", FilterResult::Keep);
        expected_tree.insert("/some/other/value", FilterResult::Keep);
        expected_tree.insert("/x/y", FilterResult::DiscardChildren);
        expected_tree.insert("/z", FilterResult::DiscardChildren);

        let expected_visited_paths = [
            "/keep",
            "/keep/very",
            "/keep/very/deep",
            "/keep/very/deep/path",
            "/parent",
            "/parent/child",
            "/root_val",
            "/root_val2",
            "/some",
            "/some/artifact",
            "/some/deep",
            "/some/deep/stuff",
            "/some/deep/value",
            "/some/other",
            "/some/other/thing",
            "/some/other/value",
            "/totally",
            "/totally/deep",
            "/totally/deep/stuff",
            "/totally/deep/value",
            "/val3",
            "/x",
            "/x/y",
            "/z",
        ];
        let mut visited_paths = Vec::new();

        assert_eq!(
            original_tree.clone_filtered(|val, path| {
                visited_paths.push(path.to_string());
                if path == "/keep/very/deep/path" {
                    FilterResult::Keep
                } else {
                    *val
                }
            }),
            expected_tree
        );

        assert_eq!(visited_paths, expected_visited_paths);
    }

    #[test]
    fn mutate_vals() {
        let mut original_tree = FsTree::<bool>::new();

        original_tree.insert("/data", false);
        original_tree.insert("/totally/deep/value", false);

        original_tree.mutate_vals(|val| *val = true);

        let mut expected_tree = FsTree::<bool>::new();

        expected_tree.insert("/", true);
        expected_tree.insert("/data", true);
        expected_tree.insert("/totally", true);
        expected_tree.insert("/totally/deep", true);
        expected_tree.insert("/totally/deep/value", true);

        assert_eq!(original_tree, expected_tree);
    }
}
