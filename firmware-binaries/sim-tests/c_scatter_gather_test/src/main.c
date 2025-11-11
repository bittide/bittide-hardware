// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "scattergatherpe_memmap.h"
#include "bittide_uart.h"
#include "bittide_scatter.h"
#include "bittide_gather.h"
#include "stdint.h"

// Memory size is 16 words of uint64_t (from the memory map)
#define MEM_SIZE 16

// Global UART for test output
static Uart uart;

// Entry point called from Rust
void c_main(void) {
    // Initialize UART
    uart = uart_init(UART_DATA, UART_STATUS);

    // Track error counts for each test
    uint32_t test_errors[3] = {0, 0, 0}; // 0: data transfer, 1: metacycle identical, 2: metacycle increment

    // Initialize ScatterUnit
    ScatterUnit scatter = scatter_unit_init(
        (volatile uint64_t*)SCATTER_UNIT_SCATTER_MEMORY,
        SCATTER_UNIT_METACYCLE_COUNT,
        SCATTER_UNIT_METACYCLE_REGISTER,
        MEM_SIZE
    );

    // Initialize GatherUnit
    GatherUnit gather = gather_unit_init(
        (volatile uint64_t*)GATHER_UNIT_GATHER_MEMORY,
        GATHER_UNIT_METACYCLE_COUNT,
        GATHER_UNIT_METACYCLE_REGISTER,
        MEM_SIZE
    );

    // Create source array with incrementing values [0, 1, 2, ..., 31]
    uint64_t source[MEM_SIZE * 2];
    uint64_t destination[MEM_SIZE * 2];

    for (uint32_t i = 0; i < MEM_SIZE * 2; i++) {
        source[i] = i;
        destination[i] = 0xDEADBEEF;
    }

    // ===== TIME-CRITICAL SECTION: No UART =====
    // Metacycle 1: Write first half to gather memory
    gather_unit_wait_for_new_metacycle(&gather);
    if (gather_unit_write_slice(&gather, source, 0, MEM_SIZE) != 0) {
        uart_puts(&uart, "ERROR: gather_unit_write_slice failed (first half)\r\n");
        while (1) {}
    }

    // Metacycle 2: Write second half to gather memory
    gather_unit_wait_for_new_metacycle(&gather);
    if (gather_unit_write_slice(&gather, &source[MEM_SIZE], 0, MEM_SIZE) != 0) {
        uart_puts(&uart, "ERROR: gather_unit_write_slice failed (second half)\r\n");
        while (1) {}
    }

    // Metacycle 3: Read first half from scatter memory
    scatter_unit_wait_for_new_metacycle(&scatter);
    if (scatter_unit_read_slice(&scatter, destination, 0, MEM_SIZE) != 0) {
        uart_puts(&uart, "ERROR: scatter_unit_read_slice failed (first half)\r\n");
        while (1) {}
    }

    // Metacycle 4: Read second half from scatter memory
    scatter_unit_wait_for_new_metacycle(&scatter);
    if (scatter_unit_read_slice(&scatter, &destination[MEM_SIZE], 0, MEM_SIZE) != 0) {
        uart_puts(&uart, "ERROR: scatter_unit_read_slice failed (second half)\r\n");
        while (1) {}
    }
    // ===== END TIME-CRITICAL SECTION =====

    // Now verify and report results
    for (uint32_t i = 0; i < MEM_SIZE * 2; i++) {
        if (source[i] != destination[i]) {
            test_errors[0]++;
        }
    }

    if (test_errors[0] != 0) {
        uart_puts(&uart, "Data test FAILED: ");
        uart_putdec(&uart, test_errors[0]);
        uart_puts(&uart, "/");
        uart_putdec(&uart, MEM_SIZE * 2);
        uart_puts(&uart, " mismatches\r\n");
        uart_puts(&uart, "First mismatch at index ");
        for (uint32_t i = 0; i < MEM_SIZE * 2; i++) {
            if (source[i] != destination[i]) {
                uart_putdec(&uart, i);
                uart_puts(&uart, ": expected ");
                uart_puthex64(&uart, source[i]);
                uart_puts(&uart, ", got ");
                uart_puthex64(&uart, destination[i]);
                uart_puts(&uart, "\r\n");
                break;
            }
        }
    }

    // Now test metacycle count functionality
    // Track 5 readings: initial + 4 waits
    uint32_t scatter_mc_readings[5];
    uint32_t gather_mc_readings[5];

    // Read initial counts
    scatter_mc_readings[0] = scatter_unit_metacycle_count(&scatter);
    gather_mc_readings[0] = gather_unit_metacycle_count(&gather);

    // Wait and read after each wait (alternating between gather and scatter)
    for (uint32_t i = 1; i < 5; i++) {
        // Alternate between gather and scatter waits
        if (i % 2 == 0) {
            gather_unit_wait_for_new_metacycle(&gather);
        } else {
            scatter_unit_wait_for_new_metacycle(&scatter);
        }
        scatter_mc_readings[i] = scatter_unit_metacycle_count(&scatter);
        gather_mc_readings[i] = gather_unit_metacycle_count(&gather);
    }

    // Check that scatter and gather readings are identical
    for (uint32_t i = 0; i < 5; i++) {
        if (scatter_mc_readings[i] != gather_mc_readings[i]) {
            test_errors[1]++;
        }
    }

    if (test_errors[1] != 0) {
        uart_puts(&uart, "Metacycle identical test FAILED: ");
        uart_putdec(&uart, test_errors[1]);
        uart_puts(&uart, " mismatches\r\n");
        uart_puts(&uart, "Scatter readings: ");
        for (uint32_t i = 0; i < 5; i++) {
            uart_putdec(&uart, scatter_mc_readings[i]);
            if (i < 4) uart_puts(&uart, " ");
        }
        uart_puts(&uart, "\r\nGather readings:  ");
        for (uint32_t i = 0; i < 5; i++) {
            uart_putdec(&uart, gather_mc_readings[i]);
            if (i < 4) uart_puts(&uart, " ");
        }
        uart_puts(&uart, "\r\n");
    }

    // Check that readings increment by 1 each time
    for (uint32_t i = 1; i < 5; i++) {
        if (scatter_mc_readings[i] != scatter_mc_readings[i-1] + 1) {
            test_errors[2]++;
        }
    }

    if (test_errors[2] != 0) {
        uart_puts(&uart, "Metacycle increment test FAILED: ");
        uart_putdec(&uart, test_errors[2]);
        uart_puts(&uart, " errors\r\n");
        uart_puts(&uart, "Readings: ");
        for (uint32_t i = 0; i < 5; i++) {
            uart_putdec(&uart, scatter_mc_readings[i]);
            if (i < 4) uart_puts(&uart, " -> ");
        }
        uart_puts(&uart, "\r\n");
    }

    // Final result
    if (test_errors[0] == 0 && test_errors[1] == 0 && test_errors[2] == 0) {
        uart_puts(&uart, "Scatter/Gather HAL tests PASSED\r\n");
    } else {
        uart_puts(&uart, "Scatter/Gather HAL tests FAILED\r\n");
    }

    while (1) {}
}
