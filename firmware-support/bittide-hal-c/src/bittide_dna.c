// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "bittide_dna.h"

void dna_read(volatile uint8_t* dna_register, dna_t out) {
    // The DNA is represented as a Maybe<BitVector(96)> occupying 24 bytes.
    // System uses: ?busByteOrder = BigEndian, ?regByteOrder = LittleEndian
    // Memory layout:
    //   Byte 0: Maybe tag (0=Nothing, 1=Just)
    //   Bytes 1-3: Padding (for 4-byte alignment)
    //   Bytes 4-7: Padding (for 8-byte alignment of BitVector 96)
    //   Bytes 8-19: The 96-bit DNA value as 3x 32-bit words
    //   Bytes 20-23: Padding
    const volatile uint32_t* maybe_dna_words = (const volatile uint32_t*)dna_register;

    // Retry until we get a valid DNA
    while (1) {
        // Check if DNA is present (first byte of first word)
        uint8_t present = ((const volatile uint8_t*)maybe_dna_words)[0];
        if (present == 1) {
            // Read the 96-bit DNA value as three 32-bit words
            out[0] = maybe_dna_words[2];  // bits [31:0]   at bytes 8-11 (least significant word)
            out[1] = maybe_dna_words[3];  // bits [63:32]  at bytes 12-15
            out[2] = maybe_dna_words[4];  // bits [95:64]  at bytes 16-19 (most significant word)
            return;
        }
        // If not present, retry
    }
}
