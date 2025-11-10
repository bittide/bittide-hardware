// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "bittide_gather.h"

GatherUnit gather_unit_init(
    volatile uint64_t* gather_memory,
    volatile uint32_t* metacycle_count,
    volatile uint32_t* metacycle_register,
    uint32_t memory_len
) {
    GatherUnit unit = {
        .gather_memory = gather_memory,
        .metacycle_count = metacycle_count,
        .metacycle_register = metacycle_register,
        .memory_len = memory_len
    };
    return unit;
}

int gather_unit_write_slice(
    const GatherUnit* unit,
    const uint64_t* src,
    uint32_t offset,
    uint32_t len
) {
    // Validate parameters
    if (unit == 0 || src == 0) {
        return -1;
    }

    // Check bounds to prevent buffer overrun
    if (offset + len > unit->memory_len) {
        return -1;
    }

    // Perform the write
    for (uint32_t i = 0; i < len; i++) {
        unit->gather_memory[offset + i] = src[i];
    }

    return 0;
}

int gather_unit_read_slice(
    const GatherUnit* unit,
    uint64_t* dst,
    uint32_t offset,
    uint32_t len
) {
    // Validate parameters
    if (unit == 0 || dst == 0) {
        return -1;
    }

    // Check bounds to prevent buffer overrun
    if (offset + len > unit->memory_len) {
        return -1;
    }

    // Perform the read
    for (uint32_t i = 0; i < len; i++) {
        dst[i] = unit->gather_memory[offset + i];
    }

    return 0;
}

void gather_unit_wait_for_new_metacycle(const GatherUnit* unit) {
    // Reading this register causes a stall until end of metacycle
    (void)*unit->metacycle_register;
}

uint32_t gather_unit_metacycle_count(const GatherUnit* unit) {
    return *unit->metacycle_count;
}
