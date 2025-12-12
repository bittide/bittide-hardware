// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "bittide_dna.h"
#include "shared_devices/dna.h"

void dna_read(Dna dna_device, dna_t out) {
  // Retry until we get a valid DNA
  while (1) {
    Maybe_bv96 value = dna_get_maybe_dna(dna_device);
    if (value.tag == MAYBE_TAG_JUST) {
      // The DNA value is stored as a 12-byte array (96 bits)
      // Copy directly to the output array (3x 32-bit words = 12 bytes)
      memcpy(out, value.just._0, 12);
      return;
    }
    // If not present, retry
  }
}
