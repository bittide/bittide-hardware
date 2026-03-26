// SPDX-FileCopyrightText: 2026 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

/// A 4-byte aligned wrapper for any type.
///
/// This type ensures that the wrapped value is properly aligned for word-based
/// operations, enabling efficient bulk transfers using `copy_nonoverlapping`.
#[repr(align(4))]
#[derive(Copy, Clone)]
pub struct Aligned4<T>(T);

impl<T> Aligned4<T> {
    /// Creates a new aligned wrapper around the given value.
    pub const fn new(value: T) -> Self {
        Self(value)
    }

    /// Returns a reference to the underlying value.
    pub const fn get(&self) -> &T {
        &self.0
    }

    /// Returns a mutable reference to the underlying value.
    pub fn get_mut(&mut self) -> &mut T {
        &mut self.0
    }

    /// Consumes the wrapper and returns the inner value.
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T: Default> Default for Aligned4<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}
