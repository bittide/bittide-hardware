// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_DNA_H
#define BITTIDE_DNA_H

#include <stdint.h>
#include "shared_devices/dna.h"

// ============================================================================
// DNA Function Declarations
// ============================================================================

/// Fetch the DNA value from the device, retrying until successful.
/// @param dna_device DNA memory-mapped device
/// @param out Pointer to output array that will receive the 96-bit DNA value
void dna_read(Dna dna_device, dna_t out) {
    // Retry until we get a valid DNA
    while (1) {
        // Check if DNA is present (first byte of first word)
        uint8_t present = dna_device.base[0];
        if (present == 1) {
            // Read the 96-bit DNA value as three 32-bit words
            out[0] = dna_device.base[2];  // bits [31:0]   at bytes 8-11 (least significant word)
            out[1] = dna_device.base[3];  // bits [63:32]  at bytes 12-15
            out[2] = dna_device.base[4];  // bits [95:64]  at bytes 16-19 (most significant word)
            return;
        }
        // If not present, retry
    }
}

#endif // BITTIDE_DNA_H
