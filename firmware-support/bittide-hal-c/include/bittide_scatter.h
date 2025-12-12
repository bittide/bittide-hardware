// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

/// This file contains helper functions for working with scatter units.
///
/// Because there are multiple different implementations of scatter units
/// (due to slight differences in register types) this code would need to be
/// generic.
/// As C doesn't have generics, this file instead depends on the specific
/// scatter unit header file to be included to use the correct interface.

#ifndef BITTIDE_SCATTER_H
#define BITTIDE_SCATTER_H

#include "stdbool.h"
#include "stdint.h"

// All of those includes don't really do anything.. I guess it might help
// with auto-completion though?

#ifdef HAL_SCATTER_GATHER_PE_DEVICE_SCATTER_UNIT_H
#include "hals/scatter_gather_pe/devices/scatter_unit.h"
#elif defined HAL_SOFT_UGN_DEMO_GPPE_DEVICE_SCATTER_UNIT_H
#include "hals/soft_ugn_demo_gppe/devices/scatter_unit.h"
#elif defined HAL_SWITCH_DEMO_GPPE_PE_DEVICE_SCATTER_UNIT_H
#include "hals/switch_demo_gppe_pe/devices/scatter_unit.h"
#else
#error "No scatter unit header definition found!"
#endif

/**
 * Read a slice of data from scatter memory
 *
 * Reads data from the scatter memory buffer. This function does not perform
 * bounds checking, so the caller must ensure that the read operation is within
 * valid memory bounds.
 *
 * @param unit Pointer to the ScatterUnit
 * @param dst Destination buffer for the read data (must be at least len words)
 * @param offset Offset in scatter memory (in uint64_t words) to start reading
 * from
 * @param len Number of uint64_t words to read
 */
static inline void scatter_unit_read_slice_unchecked(ScatterUnit unit,
                                                     uint8_t (*dst)[8],
                                                     uint32_t offset,
                                                     uint32_t len) {
  for (uint32_t i = 0; i < len; i++) {
    scatter_unit_get_scatter_memory_unchecked(unit, offset + i, dst[i]);
  }
}

/**
 * Read a slice of data from scatter memory
 *
 * Reads data from the scatter memory buffer. This function performs bounds
 * checking to prevent reading beyond the allocated memory region.
 *
 * @param unit Pointer to the ScatterUnit
 * @param dst Destination buffer for the read data (must be at least len words)
 * @param offset Offset in scatter memory (in uint64_t words) to start reading
 * from
 * @param len Number of uint64_t words to read
 * @return true on success, false if bounds check fails or parameters are
 * invalid
 */
static inline bool scatter_unit_read_slice(ScatterUnit unit, uint8_t (*dst)[8],
                                           uint32_t offset, uint32_t len) {
  if (dst == 0 || offset + len > SCATTER_UNIT_SCATTER_MEMORY_LEN) {
    return false;
  }

  scatter_unit_read_slice_unchecked(unit, dst, offset, len);

  return true;
}
/**
 * Read a slice of data from scatter memory with wrapping
 *
 * This function reads data from the scatter memory, wrapping around to the
 * beginning of the memory if the read operation exceeds the memory boundary.
 *
 * @param unit Pointer to the ScatterUnit
 * @param dst Destination buffer for the read data
 * @param offset Offset in scatter memory (in uint64_t words) to start reading
 * from
 * @param len Number of uint64_t words to read
 */
static inline void scatter_unit_read_slice_wrapping(ScatterUnit unit,
                                                    uint8_t (*dst)[8],
                                                    uint32_t offset,
                                                    uint32_t len) {
  if (offset + len <= SCATTER_UNIT_SCATTER_MEMORY_LEN) {
    // No wrapping needed
    scatter_unit_read_slice_unchecked(unit, dst, offset, len);
  } else {
    // Wrapping needed
    uint32_t first_part_len = SCATTER_UNIT_SCATTER_MEMORY_LEN - offset;
    uint32_t second_part_len = len - first_part_len;

    scatter_unit_read_slice_unchecked(unit, dst, offset, first_part_len);
    scatter_unit_read_slice_unchecked(unit, dst + first_part_len, 0,
                                      second_part_len);
  }
}

/**
 * Wait for the next metacycle boundary
 *
 * This function blocks until the end of the current metacycle. It is typically
 * used to synchronize operations with metacycle boundaries.
 *
 * @param unit ScatterUnit device
 */
static inline void scatter_unit_wait_for_new_metacycle(ScatterUnit unit) {
  uint8_t val[4];
  scatter_unit_get_metacycle_register(unit, val);
  (void)val;
}

#endif // BITTIDE_SCATTER_H
