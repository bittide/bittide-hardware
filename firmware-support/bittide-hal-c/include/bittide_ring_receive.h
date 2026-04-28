// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

/// This file contains helper functions for working with scatter units / receive
/// ring_buffers.
///
/// Because there are multiple different implementations of scatter units
/// (due to slight differences in register types) this code would need to be
/// generic.
/// As C doesn't have generics, this file instead depends on the specific
/// scatter unit header file to be included to use the correct interface.

#ifndef BITTIDE_RING_RECEIVE
#define BITTIDE_RING_RECEIVE

#include "stdbool.h"
#include "stdint.h"
#include "string.h"

// All of those includes don't really do anything.. I guess it might help
// with auto-completion though?

#ifdef HAL_SOFT_UGN_DEMO_MU_DEVICE_RECEIVE_RING_BUFFER_H
#include "hals/soft_ugn_demo_mu/devices/receive_ring_buffer.h"
// Type, constant, and functions already defined in receive_ring_buffer.h
#else
#error "No receive ring_buffer header definition found!"
#endif

/**
 * Read a slice of data from receive ring_buffer memory
 *
 * Reads data from the receive memory buffer without performing bounds checking.
 *
 * @param unit Pointer to the ReceiveRingBuffer
 * @param dst Destination buffer to store read data (must have space for at
 * least len words)
 * @param offset Offset in receive memory (in uint64_t words) to start reading
 * from
 * @param len Number of uint64_t words to read
 */
static inline void
receive_ring_buffer_read_slice_unchecked(ReceiveRingBuffer unit, uint64_t *dst,
                                         uint32_t offset, uint32_t len) {
#ifdef HAL_SOFT_UGN_DEMO_MU_DEVICE_RECEIVE_RING_BUFFER_H
  // For soft_ugn_demo_mu: use byte-oriented API with 8-byte chunks
  for (uint32_t i = 0; i < len; i++) {
    receive_ring_buffer_get_data_unchecked(unit, offset + i,
                                           (uint8_t *)&dst[i]);
  }
#else
  // For scatter/gather units: use direct memory access
  memcpy_volatile(dst, (unit.base + 0) + (offset * sizeof(uint64_t)),
                  len * sizeof(uint64_t));
#endif
}

/**
 * Read a slice of data from receive ring_buffer memory
 *
 * Reads data from the receive memory buffer. This function performs bounds
 * checking to prevent reading beyond the allocated memory region.
 *
 * @param unit Pointer to the ReceiveRingBuffer
 * @param dst Destination buffer to store read data (must have space for at
 * least len words)
 * @param offset Offset in receive memory (in uint64_t words) to start reading
 * from
 * @param len Number of uint64_t words to read
 * @return true on success, false if bounds check fails or parameters are
 * invalid
 */
static inline bool receive_ring_buffer_read_slice(ReceiveRingBuffer unit,
                                                  uint64_t *dst,
                                                  uint32_t offset,
                                                  uint32_t len) {
  // Validate parameters
  if (dst == 0 || offset + len > RECEIVE_RING_BUFFER_DATA_LEN) {
    return false;
  }

  receive_ring_buffer_read_slice_unchecked(unit, dst, offset, len);

  return true;
}

/**
 * Clear the receive ring_buffer memory by setting all words to zero
 * Note: This function is generally not needed for receive buffers as they are
 * read-only.
 *
 * @param unit Pointer to the ReceiveRingBuffer
 */
static inline void receive_ring_buffer_clear(ReceiveRingBuffer unit) {
  (void)unit;
  // Receive ring_buffers are typically read-only, so clearing is not applicable
}

#endif // BITTIDE_RING_RECEIVE
