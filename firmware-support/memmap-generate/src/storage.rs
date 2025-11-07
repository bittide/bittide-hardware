// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::{
    marker::PhantomData,
    ops::{Index, IndexMut, RangeInclusive},
};

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq)]
pub struct Storage<T>(Vec<T>);

pub struct Handle<T>(usize, PhantomData<*mut T>);

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Handle<T> {}

impl<T> std::fmt::Debug for Handle<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Handle").field(&self.0).finish()
    }
}

impl<T> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Handle<T> {}

impl<T> PartialOrd for Handle<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Handle<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> Handle<T> {
    pub const fn const_handle(index: usize) -> Handle<T> {
        Handle(index, PhantomData)
    }

    pub const fn index(&self) -> usize {
        self.0
    }

    pub fn cast<U>(self) -> Handle<U> {
        Handle(self.0, PhantomData)
    }
}

impl<T> std::hash::Hash for Handle<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct HandleRange<T> {
    pub start: Handle<T>,
    pub len: usize,
}

impl<T> Clone for HandleRange<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for HandleRange<T> {}

impl<T> HandleRange<T> {
    pub fn empty() -> HandleRange<T> {
        Self {
            start: Handle(0, PhantomData),
            len: 0,
        }
    }

    pub fn handles(self) -> impl DoubleEndedIterator<Item = Handle<T>> + ExactSizeIterator {
        (self.start.0..(self.start.0 + self.len)).map(|idx| Handle(idx, PhantomData))
    }

    pub fn build() -> HandleRangeBuilder<T> {
        HandleRangeBuilder::Empty
    }
}

pub enum HandleRangeBuilder<T> {
    Empty,
    Start { start: Handle<T> },
    Continue { start: Handle<T>, len: usize },
}

impl<T> HandleRangeBuilder<T> {
    pub fn add(&mut self, handle: Handle<T>) {
        match self {
            HandleRangeBuilder::Empty => *self = HandleRangeBuilder::Start { start: handle },
            HandleRangeBuilder::Start { start } => {
                if handle.0 == start.0 + 1 {
                    *self = HandleRangeBuilder::Continue {
                        start: *start,
                        len: 2,
                    };
                } else {
                    panic!()
                }
            }
            HandleRangeBuilder::Continue { start, len } => {
                if handle.0 == (start.0 + *len) {
                    *len += 1;
                } else {
                    panic!()
                }
            }
        }
    }

    pub fn finish(self) -> HandleRange<T> {
        match self {
            HandleRangeBuilder::Empty => HandleRange::empty(),
            HandleRangeBuilder::Start { start } => HandleRange { start, len: 1 },
            HandleRangeBuilder::Continue { start, len } => HandleRange { start, len },
        }
    }
}

impl<T> Storage<T> {
    pub const fn new() -> Self {
        Self(vec![])
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(Vec::with_capacity(capacity))
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn range_counter(&self) -> StorageRangeCounter<T> {
        let start = self.len();
        StorageRangeCounter(start, PhantomData)
    }

    pub fn push(&mut self, data: T) -> Handle<T> {
        let index = self.0.len();
        self.0.push(data);
        Handle(index, PhantomData)
    }

    pub fn push_range(&mut self, data: impl IntoIterator<Item = T>) -> HandleRange<T> {
        let mut iter = data.into_iter();

        let Some(el) = iter.next() else {
            return HandleRange {
                start: Handle(0, PhantomData),
                len: 0,
            };
        };

        let start = self.push(el);

        let mut len = 1;

        for el in iter {
            self.push(el);
            len += 1;
        }

        HandleRange { start, len }
    }

    pub fn get(&self, idx: Handle<T>) -> Option<&T> {
        self.0.get(idx.0)
    }

    pub fn get_mut(&mut self, idx: Handle<T>) -> Option<&mut T> {
        self.0.get_mut(idx.0)
    }

    #[allow(clippy::should_implement_trait)]
    pub fn into_iter(self) -> impl DoubleEndedIterator<Item = (Handle<T>, T)> + ExactSizeIterator {
        self.0
            .into_iter()
            .enumerate()
            .map(|(idx, val)| (Handle(idx, PhantomData), val))
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = (Handle<T>, &T)> + ExactSizeIterator {
        self.0
            .iter()
            .enumerate()
            .map(|(idx, val)| (Handle(idx, PhantomData), val))
    }

    pub fn iter_mut(
        &mut self,
    ) -> impl DoubleEndedIterator<Item = (Handle<T>, &mut T)> + ExactSizeIterator {
        self.0
            .iter_mut()
            .enumerate()
            .map(|(idx, val)| (Handle(idx, PhantomData), val))
    }

    pub fn values(&self) -> impl DoubleEndedIterator<Item = &T> + ExactSizeIterator {
        self.0.iter()
    }

    pub fn values_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut T> + ExactSizeIterator {
        self.0.iter_mut()
    }

    pub fn whole_range(&self) -> HandleRange<T> {
        HandleRange {
            start: Handle::const_handle(0),
            len: self.len(),
        }
    }
}

impl<T> Default for Storage<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T> Index<Handle<T>> for Storage<T> {
    type Output = T;

    fn index(&self, index: Handle<T>) -> &Self::Output {
        &self.0[index.0]
    }
}

impl<T> IndexMut<Handle<T>> for Storage<T> {
    fn index_mut(&mut self, index: Handle<T>) -> &mut Self::Output {
        &mut self.0[index.0]
    }
}

impl<T> Index<RangeInclusive<Handle<T>>> for Storage<T> {
    type Output = [T];

    fn index(&self, index: RangeInclusive<Handle<T>>) -> &Self::Output {
        &self.0[index.start().0..=index.end().0]
    }
}

impl<T> IndexMut<RangeInclusive<Handle<T>>> for Storage<T> {
    fn index_mut(&mut self, index: RangeInclusive<Handle<T>>) -> &mut Self::Output {
        &mut self.0[index.start().0..=index.end().0]
    }
}

impl<T> Index<HandleRange<T>> for Storage<T> {
    type Output = [T];

    fn index(&self, index: HandleRange<T>) -> &Self::Output {
        &self.0[index.start.0..(index.start.0 + index.len)]
    }
}

impl<T> IndexMut<HandleRange<T>> for Storage<T> {
    fn index_mut(&mut self, index: HandleRange<T>) -> &mut Self::Output {
        &mut self.0[index.start.0..(index.start.0 + index.len)]
    }
}

pub struct StorageRangeCounter<T>(usize, PhantomData<*mut T>);

impl<T> StorageRangeCounter<T> {
    pub fn finish(self, storage: &Storage<T>) -> HandleRange<T> {
        let start = Handle(self.0, PhantomData);
        let len = storage.len() - self.0;
        HandleRange { start, len }
    }
}
