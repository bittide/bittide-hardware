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

#ifndef BITTIDE_GATHER_H
#define BITTIDE_GATHER_H

#include "stdbool.h"
#include "stdint.h"

// All of those includes don't really do anything.. I guess it might help
// with auto-completion though?

#ifdef HAL_SCATTER_GATHER_PE_DEVICE_GATHER_UNIT_H
#include "hals/scatter_gather_pe/devices/gather_unit.h"
#elif defined HAL_SOFT_UGN_DEMO_GPPE_DEVICE_GATHER_UNIT_H
#include "hals/soft_ugn_demo_gppe/devices/gather_unit.h"
#elif defined HAL_SWITCH_DEMO_GPPE_PE_DEVICE_GATHER_UNIT_H
#include "hals/switch_demo_gppe_pe/devices/gather_unit.h"
#else
#error "No gather unit header definition found!"
#endif

/**
 * Write a slice of data to gather memory
 *
 * Writes data to the gather memory buffer without performing bounds checking.
 *
 * @param unit Pointer to the GatherUnit
 * @param src Source buffer containing data to write (must contain at least len
 * words)
 * @param offset Offset in gather memory (in uint64_t words) to start writing to
 * @param len Number of uint64_t words to write
 */
static inline void gather_unit_write_slice_unchecked(GatherUnit unit,
                                                     const uint8_t (*src)[8],
                                                     uint32_t offset,
                                                     uint32_t len) {
  for (uint32_t i = 0; i < len; i++) {
    gather_unit_set_gather_memory_unchecked(unit, offset + i, src[i]);
  }
}

/**
 * Write a slice of data to gather memory
 *
 * Writes data to the gather memory buffer. This function performs bounds
 * checking to prevent writing beyond the allocated memory region.
 *
 * @param unit Pointer to the GatherUnit
 * @param src Source buffer containing data to write (must contain at least len
 * words)
 * @param offset Offset in gather memory (in uint64_t words) to start writing to
 * @param len Number of uint64_t words to write
 * @return true on success, false if bounds check fails or parameters are
 * invalid
 */
static inline bool gather_unit_write_slice(GatherUnit unit,
                                           const uint8_t (*src)[8],
                                           uint32_t offset, uint32_t len) {
  // Validate parameters
  if (src == 0 || offset + len > GATHER_UNIT_GATHER_MEMORY_LEN) {
    return false;
  }

  gather_unit_write_slice_unchecked(unit, src, offset, len);

  return true;
}
/**
 * Write a slice of data to gather memory with wrapping
 *
 * This function writes data to the gather memory, wrapping around to the
 * beginning of the memory if the write operation exceeds the memory boundary.
 *
 * @param unit Pointer to the GatherUnit
 * @param src Source buffer containing data to write
 * @param offset Offset in gather memory (in uint64_t words) to start writing to
 * @param len Number of uint64_t words to write
 */
static inline void gather_unit_write_slice_wrapping(GatherUnit unit,
                                                    const uint8_t (*src)[8],
                                                    uint32_t offset,
                                                    uint32_t len) {
  if (offset + len <= GATHER_UNIT_GATHER_MEMORY_LEN) {
    // No wrapping needed
    gather_unit_write_slice_unchecked(unit, src, offset, len);
  } else {
    // Wrapping needed
    uint32_t first_part_len = GATHER_UNIT_GATHER_MEMORY_LEN - offset;
    uint32_t second_part_len = len - first_part_len;

    gather_unit_write_slice_unchecked(unit, src, offset, first_part_len);
    gather_unit_write_slice_unchecked(unit, src + first_part_len, 0,
                                      second_part_len);
  }
}
/**
 * Wait for the next metacycle boundary
 *
 * This function blocks until the end of the current metacycle. It is typically
 * used to synchronize operations with metacycle boundaries.
 *
 * @param unit Pointer to the GatherUnit
 */
static inline void gather_unit_wait_for_new_metacycle(GatherUnit unit) {
  uint8_t val[4];
  gather_unit_get_metacycle_register(unit, val);
  (void)val;
}

#endif // BITTIDE_GATHER_H
