// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

/// This file contains helper functions for working with gather units.
///
/// Because there are multiple different implementations of gather units
/// (due to slight differences in register types) this code would need to be
/// generic.
/// As C doesn't have generics, this file instead depends on the specific
/// gather unit header file to be included to use the correct interface.

#ifndef BITTIDE_RING_TRANSMIT
#define BITTIDE_RING_TRANSMIT

#include "stdbool.h"
#include "stdint.h"
#include "string.h"

// All of those includes don't really do anything.. I guess it might help
// with auto-completion though?

#ifdef HAL_SOFT_UGN_DEMO_MU_DEVICE_TRANSMIT_RING_BUFFER_H
#include "hals/soft_ugn_demo_mu/devices/transmit_ring_buffer.h"
// Type, constant, and functions already defined in transmit_ring_buffer.h
#else
#error "No transmit ring_buffer header definition found!"
#endif

/**
 * Write a slice of data to transmit ring_buffer memory
 *
 * Writes data to the transmit memory buffer without performing bounds checking.
 *
 * @param unit Pointer to the TransmitRingBuffer
 * @param src Source buffer containing data to write (must contain at least len
 * words)
 * @param offset Offset in transmit memory (in uint64_t words) to start writing
 * to
 * @param len Number of uint64_t words to write
 */
static inline void transmit_ring_buffer_write_slice_unchecked(
    TransmitRingBuffer unit, uint64_t *src, uint32_t offset, uint32_t len) {
  for (uint32_t i = 0; i < len; i++) {
    transmit_ring_buffer_set_data_unchecked(unit, offset + i,
                                            (uint8_t const *)&src[i]);
  }
}

/**
 * Write a slice of data to transmit ring_buffer memory
 *
 * Writes data to the transmit memory buffer. This function performs bounds
 * checking to prevent writing beyond the allocated memory region.
 *
 * @param unit Pointer to the TransmitRingBuffer
 * @param src Source buffer containing data to write (must contain at least len
 * words)
 * @param offset Offset in transmit memory (in uint64_t words) to start writing
 * to
 * @param len Number of uint64_t words to write
 * @return true on success, false if bounds check fails or parameters are
 * invalid
 */
static inline bool transmit_ring_buffer_write_slice(TransmitRingBuffer unit,
                                                    uint64_t *src,
                                                    uint32_t offset,
                                                    uint32_t len) {
  // Validate parameters
  if (src == 0 || offset + len > TRANSMIT_RING_BUFFER_DATA_LEN) {
    return false;
  }

  transmit_ring_buffer_write_slice_unchecked(unit, src, offset, len);

  return true;
}

/**
 * Clear the transmit ring_buffer memory by setting all words to zero
 *
 * @param unit Pointer to the TransmitRingBuffer
 */
static inline void transmit_ring_buffer_clear(TransmitRingBuffer unit) {
  // For soft_ugn_demo_mu: clear byte-oriented ring_buffer
  uint8_t zeros[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  for (uint32_t i = 0; i < TRANSMIT_RING_BUFFER_DATA_LEN; i++) {
    transmit_ring_buffer_set_data_unchecked(unit, i, zeros);
  }
}
#endif
