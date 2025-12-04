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
      // Read the 96-bit DNA value as three 32-bit words

      // conversion helper
      union {
        uint32_t vals[4];
        uint128_t dna;
      } conv;

      // Read the 96-bit DNA value as three 32-bit words

      conv.dna[0] = value.just._0[0];
      conv.dna[1] = value.just._0[1];

      out[0] = conv.vals[1];
      out[1] = conv.vals[2];
      out[2] = conv.vals[3];
      return;
    }
    // If not present, retry
  }
}
