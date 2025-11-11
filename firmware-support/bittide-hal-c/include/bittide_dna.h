// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_DNA_H
#define BITTIDE_DNA_H

#include "shared_devices/dna.h"
#include <stdint.h>

// ============================================================================
// DNA Function Declarations
// ============================================================================

/// Fetch the DNA value from the device, retrying until successful.
/// @param dna_device DNA memory-mapped device
/// @param out Pointer to output array that will receive the 96-bit DNA value
void dna_read(Dna dna_device, dna_t out);

#endif // BITTIDE_DNA_H
