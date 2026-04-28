<!--
SPDX-FileCopyrightText: 2026 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

## Transmit Ring Buffer

The Transmit Ring Buffer is a hardware component designed to transmit data frames over a Bittide link. It uses a single memory buffer and a free-running hardware read counter that wraps around the buffer, forming a ring buffer. The CPU can write to the buffer via the Wishbone interface.

### Architecture

{{#drawio path="diagrams/components.drawio" page=4}}

The Transmit Ring Buffer consists of:
-   **Memory Buffer**: A single memory buffer written by the CPU and read by hardware.
-   **Free-Running Read Counter**: A hardware counter that increments each cycle, wrapping around the buffer. When the ring buffer is enabled, data at the address indicated by this counter is transmitted over the Bittide link.
-   **Enable Register**: When enabled, the buffer contents are transmitted to the network. When disabled, zeroes are transmitted instead, but the counter continues to increment to maintain alignment.
-   **Wishbone Interface**: Allows the CPU to write data to be transmitted.

### Operation

1.  **Data Transmission**:
    -   Each cycle, if the ring buffer is enabled, the data at the address indicated by the free-running read counter is read from the buffer and sent over the Bittide link.
    -   The read counter increments and wraps around the buffer.

2.  **CPU Access**:
    -   The CPU writes data to the buffer using the Wishbone interface.
    -   Because the hardware continuously reads from the buffer, the CPU must ensure it writes data before the hardware's read counter reaches that address. See [Ring Buffer Alignment](../ringbuffer-alignment.md) and [Asynchronous Communication](../asynchronous-communication.md) for protocols that handle this.
