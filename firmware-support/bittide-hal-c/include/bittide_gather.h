// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_GATHER_H
#define BITTIDE_GATHER_H

#include <stdint.h>

/**
 * GatherUnit - Handle for interfacing with a gather unit
 *
 * The gather unit collects data from local memory and sends it to other nodes
 * over the Bittide network. The gather memory is double-buffered and swaps
 * buffers at metacycle boundaries according to the calendar configuration.
 */
typedef struct {
  volatile uint64_t *gather_memory;   ///< Pointer to gather memory base address
  volatile uint32_t *metacycle_count; ///< Pointer to metacycle count register
  volatile uint32_t
      *metacycle_register; ///< Pointer to metacycle synchronization register
  uint32_t memory_len;     ///< Length of gather memory in uint64_t words
} GatherUnit;

/**
 * Initialize a GatherUnit
 *
 * @param gather_memory Pointer to the gather memory base address
 * @param metacycle_count Pointer to the metacycle count register
 * @param metacycle_register Pointer to the metacycle synchronization register
 * @param memory_len Length of the gather memory in uint64_t words
 * @return Initialized GatherUnit struct
 */
GatherUnit gather_unit_init(volatile uint64_t *gather_memory,
                            volatile uint32_t *metacycle_count,
                            volatile uint32_t *metacycle_register,
                            uint32_t memory_len);

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
 * @return 0 on success, -1 if bounds check fails or parameters are invalid
 */
int gather_unit_write_slice(const GatherUnit *unit, const uint64_t *src,
                            uint32_t offset, uint32_t len);

/**
 * Read a slice of data from gather memory
 *
 * Reads data from the gather memory buffer. This function performs bounds
 * checking to prevent reading beyond the allocated memory region.
 *
 * @param unit Pointer to the GatherUnit
 * @param dst Destination buffer for the read data (must be at least len words)
 * @param offset Offset in gather memory (in uint64_t words) to start reading
 * from
 * @param len Number of uint64_t words to read
 * @return 0 on success, -1 if bounds check fails or parameters are invalid
 */
int gather_unit_read_slice(const GatherUnit *unit, uint64_t *dst,
                           uint32_t offset, uint32_t len);

/**
 * Wait for the next metacycle boundary
 *
 * This function blocks until the end of the current metacycle. It is typically
 * used to synchronize operations with the gather unit's buffer swapping.
 *
 * @param unit Pointer to the GatherUnit
 */
void gather_unit_wait_for_new_metacycle(const GatherUnit *unit);

/**
 * Get the current metacycle count
 *
 * @param unit Pointer to the GatherUnit
 * @return Current metacycle count
 */
uint32_t gather_unit_metacycle_count(const GatherUnit *unit);

#endif // BITTIDE_GATHER_H
