/* SPDX-FileCopyrightText: 2025 Google LLC
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#ifndef STRING_H
#define STRING_H

/* Minimal string.h for freestanding RISC-V environment */

#include <stddef.h>

void *memcpy(void *dest, const void *src, size_t count);

/// Perform a volatile copy of data from src to dest for count number of bytes.
///
/// This function tries to do as many word-sized reads/writes as it can and then
/// does 16bit and/or 8bit reads/writes for the remaining tail.
///
/// This optimisation is only applied when the alignment of dest and src is at
/// least word-sized (on RV32 = 4), otherwise copying will be done byte-by-byte.
///
/// As the reads/writes are volatile it means this word-optimisation might be
/// visible to memory mapped IO.
///
/// If this function used only byte read/writes then that would also be visible
/// to memory mapped IO, meaning that a single 4-byte value would be done using
/// 4 reads/writes. If a memory mapped register keeps track of the number of
/// reads/writes, this would be observable.
///
/// Here we just decided to do this optimisation as it most of the time does
/// what you would expect anyway.
///
/// If you need byte-copying behaviour, use memcpy_volatile_bytes
volatile void *memcpy_volatile(volatile void *dest, const volatile void *src,
                               size_t count);

/// Version of memcpy_volatile that only performs byte-sized reads and writes.
volatile void *memcpy_volatile_bytes(volatile void *dest,
                                     const volatile void *src, size_t count);

int memcmp(const void *lhs, const void *rhs, size_t count);

#endif // STRING_H
