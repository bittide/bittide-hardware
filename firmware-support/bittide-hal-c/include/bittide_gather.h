// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_GATHER_H
#define BITTIDE_GATHER_H

#include <stdint.h>

#ifdef HAL_SCATTER_GATHER_PE_DEVICE_GATHER_UNIT_H
#include "hals/scatter_gather_pe/devices/gather_unit.h"
#elif HAL_SOFT_UGN_DEMO_GPPE_DEVICE_GATHER_UNIT_H
#include "hals/soft_ugn_demo_gppe/devices/gather_unit.h"
#elif HAL_SWITCH_DEMO_GPPE_PE_DEVICE_GATHER_UNIT_H
#include "hals/switch_demo_gppe_pe/devices/gather_unit.h"
#else
#error "No scatter unit header definition found!"
#endif

/**
 * Write a slice of data to gather memory
 *
 * Writes data to the gather memory buffer. This function performs bounds
 * checking to prevent writing beyond the allocated memory region.
 *
 * @param unit Pointer to the GatherUnitHandle
 * @param src Source buffer containing data to write (must contain at least len words)
 * @param offset Offset in gather memory (in uint64_t words) to start writing to
 * @param len Number of uint64_t words to write
 * @return true on success, false if bounds check fails or parameters are invalid
 */
bool gather_unit_write_slice(
    GatherUnit unit,
    const uint64_t* src,
    uint32_t offset,
    uint32_t len
) {
    // Validate parameters
    if (src == 0 || offset + len > GATHER_UNIT_GATHER_MEMORY_LEN) {
        return false;
    }

    // Perform write
    for (uint32_t i = 0; i < len; i++) {
        gather_unit_set_gather_memory_unchecked(unit, offset + i, src[i]);
    }

    // return 0;
    return true;
}

/**
 * Wait for the next metacycle boundary
 *
 * This function blocks until the end of the current metacycle. It is typically
 * used to synchronize operations with the gather unit's buffer swapping.
 *
 * @param unit Pointer to the GatherUnit
 */
void gather_unit_wait_for_new_metacycle(GatherUnit unit){
    (void)gather_unit_get_metacycle_register(unit);
}

#endif // BITTIDE_GATHER_H
