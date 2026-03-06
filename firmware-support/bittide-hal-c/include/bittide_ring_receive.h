// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

/// This file contains helper functions for working with scatter units / receive
/// ringbuffers.
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

#ifdef HAL_SCATTER_GATHER_PE_DEVICE_SCATTER_UNIT_H
#include "hals/scatter_gather_pe/devices/scatter_unit.h"
#define ReceiveRingbuffer ScatterUnit
#define RECEIVE_RINGBUFFER_DATA_LEN SCATTER_UNIT_SCATTER_MEMORY_LEN
#define receive_ringbuffer_get_data_unchecked(unit, idx, val)                  \
  scatter_unit_get_scatter_memory_unchecked(unit, idx, val)
#elif defined HAL_SWITCH_DEMO_GPPE_PE_DEVICE_SCATTER_UNIT_H
#include "hals/switch_demo_gppe_pe/devices/scatter_unit.h"
#define ReceiveRingbuffer ScatterUnit
#define RECEIVE_RINGBUFFER_DATA_LEN SCATTER_UNIT_SCATTER_MEMORY_LEN
#define receive_ringbuffer_get_data_unchecked(unit, idx, val)                  \
  scatter_unit_get_scatter_memory_unchecked(unit, idx, val)
#elif defined HAL_SOFT_UGN_DEMO_GPPE_DEVICE_RECEIVE_RINGBUFFER_H
#include "hals/soft_ugn_demo_gppe/devices/receive_ringbuffer.h"
// Type, constant, and functions already defined in receive_ringbuffer.h
#else
#error "No receive ringbuffer header definition found!"
#endif

/**
 * Read a slice of data from receive ringbuffer memory
 *
 * Reads data from the receive memory buffer without performing bounds checking.
 *
 * @param unit Pointer to the ReceiveRingbuffer
 * @param dst Destination buffer to store read data (must have space for at
 * least len words)
 * @param offset Offset in receive memory (in uint64_t words) to start reading
 * from
 * @param len Number of uint64_t words to read
 */
static inline void
receive_ringbuffer_read_slice_unchecked(ReceiveRingbuffer unit, uint64_t *dst,
                                        uint32_t offset, uint32_t len) {
#ifdef HAL_SOFT_UGN_DEMO_GPPE_DEVICE_RECEIVE_RINGBUFFER_H
  // For soft_ugn_demo_gppe: use byte-oriented API with 8-byte chunks
  for (uint32_t i = 0; i < len; i++) {
    receive_ringbuffer_get_data_unchecked(unit, offset + i, (uint8_t *)&dst[i]);
  }
#else
  // For scatter/gather units: use direct memory access
  memcpy_volatile(dst, (unit.base + 0) + (offset * sizeof(uint64_t)),
                  len * sizeof(uint64_t));
#endif
}

/**
 * Read a slice of data from receive ringbuffer memory
 *
 * Reads data from the receive memory buffer. This function performs bounds
 * checking to prevent reading beyond the allocated memory region.
 *
 * @param unit Pointer to the ReceiveRingbuffer
 * @param dst Destination buffer to store read data (must have space for at
 * least len words)
 * @param offset Offset in receive memory (in uint64_t words) to start reading
 * from
 * @param len Number of uint64_t words to read
 * @return true on success, false if bounds check fails or parameters are
 * invalid
 */
static inline bool receive_ringbuffer_read_slice(ReceiveRingbuffer unit,
                                                 uint64_t *dst, uint32_t offset,
                                                 uint32_t len) {
  // Validate parameters
  if (dst == 0 || offset + len > RECEIVE_RINGBUFFER_DATA_LEN) {
    return false;
  }

  receive_ringbuffer_read_slice_unchecked(unit, dst, offset, len);

  return true;
}

/**
 * Read a slice of data from receive ringbuffer memory with wrapping
 *
 * This function reads data from the receive memory, wrapping around to the
 * beginning of the memory if the read operation exceeds the memory boundary.
 *
 * @param unit Pointer to the ReceiveRingbuffer
 * @param dst Destination buffer to store read data
 * @param offset Offset in receive memory (in uint64_t words) to start reading
 * from
 * @param len Number of uint64_t words to read
 */
static inline void
receive_ringbuffer_read_slice_wrapping(ReceiveRingbuffer unit, uint64_t *dst,
                                       uint32_t offset, uint32_t len) {
  if (offset + len <= RECEIVE_RINGBUFFER_DATA_LEN) {
    // No wrapping needed
    receive_ringbuffer_read_slice_unchecked(unit, dst, offset, len);
  } else {
    // Wrapping needed
    uint32_t first_part_len = RECEIVE_RINGBUFFER_DATA_LEN - offset;
    uint32_t second_part_len = len - first_part_len;

    receive_ringbuffer_read_slice_unchecked(unit, dst, offset, first_part_len);
    receive_ringbuffer_read_slice_unchecked(unit, dst + first_part_len, 0,
                                            second_part_len);
  }
}

/**
 * Clear the receive ringbuffer memory by setting all words to zero
 * Note: This function is generally not needed for receive buffers as they are
 * read-only.
 *
 * @param unit Pointer to the ReceiveRingbuffer
 */
static inline void receive_ringbuffer_clear(ReceiveRingbuffer unit) {
  (void)unit;
  // Receive ringbuffers are typically read-only, so clearing is not applicable
}

#endif // BITTIDE_RING_RECEIVE
