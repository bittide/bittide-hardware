// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_SCATTER_H
#define BITTIDE_SCATTER_H

#include <stdint.h>

/**
 * ScatterUnit - Handle for interfacing with a scatter unit
 *
 * The scatter unit receives data from other nodes over the Bittide network
 * and stores it in local memory.
 */
typedef struct {
  volatile uint64_t *scatter_memory; ///< Pointer to scatter memory base address
  volatile uint32_t *metacycle_count; ///< Pointer to metacycle count register
  volatile uint32_t
      *metacycle_register; ///< Pointer to metacycle synchronization register
  uint32_t memory_len;     ///< Length of scatter memory in uint64_t words
} ScatterUnit;

/**
 * Initialize a ScatterUnit
 *
 * @param scatter_memory Pointer to the scatter memory base address
 * @param metacycle_count Pointer to the metacycle count register
 * @param metacycle_register Pointer to the metacycle synchronization register
 * @param memory_len Length of the scatter memory in uint64_t words
 * @return Initialized ScatterUnit struct
 */
ScatterUnit scatter_unit_init(volatile uint64_t *scatter_memory,
                              volatile uint32_t *metacycle_count,
                              volatile uint32_t *metacycle_register,
                              uint32_t memory_len);

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
 * @return 0 on success, -1 if bounds check fails or parameters are invalid
 */
int scatter_unit_read_slice(const ScatterUnit *unit, uint64_t *dst,
                            uint32_t offset, uint32_t len);

/**
 * Write a slice of data to scatter memory
 *
 * Writes data to the scatter memory buffer. This function performs bounds
 * checking to prevent writing beyond the allocated memory region.
 *
 * @param unit Pointer to the ScatterUnit
 * @param src Source buffer containing data to write (must contain at least len
 * words)
 * @param offset Offset in scatter memory (in uint64_t words) to start writing
 * to
 * @param len Number of uint64_t words to write
 * @return 0 on success, -1 if bounds check fails or parameters are invalid
 */
int scatter_unit_write_slice(const ScatterUnit *unit, const uint64_t *src,
                             uint32_t offset, uint32_t len);

/**
 * Wait for the next metacycle boundary
 *
 * This function blocks until the end of the current metacycle. It is typically
 * used to synchronize operations with metacycle boundaries.
 *
 * @param unit Pointer to the ScatterUnit
 */
void scatter_unit_wait_for_new_metacycle(const ScatterUnit *unit);

/**
 * Get the current metacycle count
 *
 * @param unit Pointer to the ScatterUnit
 * @return Current metacycle count
 */
uint32_t scatter_unit_metacycle_count(const ScatterUnit *unit);

#endif // BITTIDE_SCATTER_H
