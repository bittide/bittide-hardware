// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "softugndemogppe_memmap.h"
#include "bittide_uart.h"

int c_main(void) {
    Uart uart = uart_init(UART_DATA, UART_STATUS);

    uart_puts(&uart, "Hello from C!\n");
    uart_puts(&uart, "Running on RISC-V\n");

    while (1) {
    }
}
