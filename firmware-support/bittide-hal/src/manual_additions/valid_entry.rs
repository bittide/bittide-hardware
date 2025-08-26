// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

impl<T: Default> Default for crate::shared::types::ValidEntry<T> {
    fn default() -> Self {
        crate::freeze::ValidEntry { ve_entry: Default::default(), ve_repeat: 0 }
    }
}
