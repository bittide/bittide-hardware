// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "bittide_scatter.h"

ScatterUnit scatter_unit_init(
    volatile uint64_t* scatter_memory,
    volatile uint32_t* metacycle_count,
    volatile uint32_t* metacycle_register,
    uint32_t memory_len
) {
    ScatterUnit unit = {
        .scatter_memory = scatter_memory,
        .metacycle_count = metacycle_count,
        .metacycle_register = metacycle_register,
        .memory_len = memory_len
    };
    return unit;
}

int scatter_unit_read_slice(
    const ScatterUnit* unit,
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
        dst[i] = unit->scatter_memory[offset + i];
    }

    return 0;
}

void scatter_unit_read_slice_wrapping(const ScatterUnit *unit, uint64_t *dst,
                                      uint32_t offset, uint32_t len) {
  if (offset + len <= unit->memory_len) {
    // No wrapping needed
    scatter_unit_read_slice(unit, dst, offset, len);
  } else {
    // Wrapping needed
    uint32_t first_part_len = unit->memory_len - offset;
    uint32_t second_part_len = len - first_part_len;

    scatter_unit_read_slice(unit, dst, offset, first_part_len);
    scatter_unit_read_slice(unit, dst + first_part_len, 0, second_part_len);
  }
}

void scatter_unit_wait_for_new_metacycle(const ScatterUnit* unit) {
    // Reading this register causes a stall until end of metacycle
    (void)*unit->metacycle_register;
}

uint32_t scatter_unit_metacycle_count(const ScatterUnit* unit) {
    return *unit->metacycle_count;
}
