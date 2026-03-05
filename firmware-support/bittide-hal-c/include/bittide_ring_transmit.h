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

#ifdef HAL_SOFT_UGN_DEMO_MU_DEVICE_TRANSMIT_RINGBUFFER_H
#include "hals/soft_ugn_demo_mu/devices/transmit_ringbuffer.h"
// Type, constant, and functions already defined in transmit_ringbuffer.h
#else
#error "No transmit ringbuffer header definition found!"
#endif

/**
 * Write a slice of data to transmit ringbuffer memory
 *
 * Writes data to the transmit memory buffer without performing bounds checking.
 *
 * @param unit Pointer to the TransmitRingbuffer
 * @param src Source buffer containing data to write (must contain at least len
 * words)
 * @param offset Offset in transmit memory (in uint64_t words) to start writing
 * to
 * @param len Number of uint64_t words to write
 */
static inline void transmit_ringbuffer_write_slice_unchecked(
    TransmitRingbuffer unit, uint64_t *src, uint32_t offset, uint32_t len) {
  for (uint32_t i = 0; i < len; i++) {
    transmit_ringbuffer_set_data_unchecked(unit, offset + i,
                                           (uint8_t const *)&src[i]);
  }
}

/**
 * Write a slice of data to transmit ringbuffer memory
 *
 * Writes data to the transmit memory buffer. This function performs bounds
 * checking to prevent writing beyond the allocated memory region.
 *
 * @param unit Pointer to the TransmitRingbuffer
 * @param src Source buffer containing data to write (must contain at least len
 * words)
 * @param offset Offset in transmit memory (in uint64_t words) to start writing
 * to
 * @param len Number of uint64_t words to write
 * @return true on success, false if bounds check fails or parameters are
 * invalid
 */
static inline bool transmit_ringbuffer_write_slice(TransmitRingbuffer unit,
                                                   uint64_t *src,
                                                   uint32_t offset,
                                                   uint32_t len) {
  // Validate parameters
  if (src == 0 || offset + len > TRANSMIT_RINGBUFFER_DATA_LEN) {
    return false;
  }

  transmit_ringbuffer_write_slice_unchecked(unit, src, offset, len);

  return true;
}
/**
 * Write a slice of data to transmit ringbuffer memory with wrapping
 *
 * This function writes data to the transmit memory, wrapping around to the
 * beginning of the memory if the write operation exceeds the memory boundary.
 *
 * @param unit Pointer to the TransmitRingbuffer
 * @param src Source buffer containing data to write
 * @param offset Offset in transmit memory (in uint64_t words) to start writing
 * to
 * @param len Number of uint64_t words to write
 */
static inline void
transmit_ringbuffer_write_slice_wrapping(TransmitRingbuffer unit, uint64_t *src,
                                         uint32_t offset, uint32_t len) {
  if (offset + len <= TRANSMIT_RINGBUFFER_DATA_LEN) {
    // No wrapping needed
    transmit_ringbuffer_write_slice_unchecked(unit, src, offset, len);
  } else {
    // Wrapping needed
    uint32_t first_part_len = TRANSMIT_RINGBUFFER_DATA_LEN - offset;
    uint32_t second_part_len = len - first_part_len;

    transmit_ringbuffer_write_slice_unchecked(unit, src, offset,
                                              first_part_len);
    transmit_ringbuffer_write_slice_unchecked(unit, src + first_part_len, 0,
                                              second_part_len);
  }
}

/**
 * Clear the transmit ringbuffer memory by setting all words to zero
 *
 * @param unit Pointer to the TransmitRingbuffer
 */
static inline void transmit_ringbuffer_clear(TransmitRingbuffer unit) {
  // For soft_ugn_demo_mu: clear byte-oriented ringbuffer
  uint8_t zeros[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  for (uint32_t i = 0; i < TRANSMIT_RINGBUFFER_DATA_LEN; i++) {
    transmit_ringbuffer_set_data_unchecked(unit, i, zeros);
  }
}
#endif
