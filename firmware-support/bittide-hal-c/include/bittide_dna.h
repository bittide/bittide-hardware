// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_DNA_H
#define BITTIDE_DNA_H

#include <stdint.h>

// ============================================================================
// DNA Function Declarations
// ============================================================================

/// Fetch the DNA value from the device, retrying until successful.
/// @param dna_register Pointer to the DNA memory-mapped register
/// @param out Pointer to output array that will receive the 96-bit DNA value
void dna_read(volatile uint8_t* dna_register, dna_t out);

#endif // BITTIDE_DNA_H
