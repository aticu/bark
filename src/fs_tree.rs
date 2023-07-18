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

    /// Creates a copy of this tree, containing only entries where `filter` returns `true` and
    /// their parents.
    pub(crate) fn copy_filtered(&self, mut filter: impl FnMut(&T) -> FilterResult) -> FsTree<T>
    where
        T: Clone,
    {
        match self {
            Self::Leaf(root_val) => FsTree::Leaf(root_val.clone()),
            Self::Inner { val: root_val, map } => {
                let mut speculation_depth = 0u32;
                let mut stack = vec![(
                    map.iter(),
                    root_val.clone(),
                    InlinableString::from("/"),
                    BTreeMap::new(),
                )];

                while let Some((iter, _, _, outer_map)) = stack.last_mut() {
                    match iter.next() {
                        Some((name, val)) => match val {
                            FsTree::Leaf(val) => match filter(val) {
                                FilterResult::Keep | FilterResult::DiscardChildren => {
                                    speculation_depth = 0;
                                    outer_map.insert(name.clone(), FsTree::Leaf(val.clone()));
                                }
                                FilterResult::Discard => (),
                            },
                            FsTree::Inner { val, map } => match filter(val) {
                                FilterResult::Keep => {
                                    speculation_depth = 1;
                                    stack.push((
                                        map.iter(),
                                        val.clone(),
                                        name.clone(),
                                        BTreeMap::new(),
                                    ));
                                }
                                FilterResult::Discard => {
                                    speculation_depth += 1;
                                    stack.push((
                                        map.iter(),
                                        val.clone(),
                                        name.clone(),
                                        BTreeMap::new(),
                                    ));
                                }
                                FilterResult::DiscardChildren => {
                                    speculation_depth = 0;
                                    outer_map.insert(name.clone(), FsTree::Leaf(val.clone()));
                                }
                            },
                        },
                        None => {
                            let Some((_, val, name, map)) = stack.pop() else { unreachable!("we're still in the loop, so there exists an element") };

                            if let Some((_, _, _, outer_map)) = stack.last_mut() {
                                if speculation_depth == 0 {
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

        expected_tree.insert("/root_val", FilterResult::Keep);
        expected_tree.insert("/root_val2", FilterResult::Keep);
        expected_tree.insert("/some", FilterResult::Keep);
        expected_tree.insert("/some/deep/stuff", FilterResult::Keep);
        expected_tree.insert("/some/other/value", FilterResult::Keep);
        expected_tree.insert("/x/y", FilterResult::DiscardChildren);
        expected_tree.insert("/z", FilterResult::DiscardChildren);

        assert_eq!(original_tree.copy_filtered(|val| *val), expected_tree);
    }
}
