<!--
SPDX-FileCopyrightText: 2026 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

## Receive Ring Buffer

The Receive Ring Buffer is a hardware component designed to receive data frames from a Bittide link and store them into a local memory. It uses a single memory buffer and a free-running hardware write counter that wraps around the buffer, forming a ring buffer. The CPU can read from the buffer via the Wishbone interface.

### Architecture

The Receive Ring Buffer consists of:
-   **Memory Buffer**: A single memory buffer written by hardware and read by the CPU.
-   **Free-Running Write Counter**: A hardware counter that increments each cycle, wrapping around the buffer. When the ring buffer is enabled, incoming frames are written at the address indicated by this counter.
-   **Enable Register**: When enabled, incoming frames from the network are written to the buffer. When disabled, incoming frames are ignored, but the counter continues to increment to maintain alignment.
-   **Clear-at-Count Register**: Resets the write counter to zero when it reaches the configured value. This is used during the [Ring Buffer Alignment](../ringbuffer-alignment.md) procedure to synchronize the TX and RX counters.
-   **Wishbone Interface**: Allows the CPU to read received data.

### Operation

1.  **Data Reception**:
    -   Each cycle, if the ring buffer is enabled and a valid frame arrives from the Bittide link, the frame is written to the memory at the address indicated by the free-running write counter.
    -   The write counter increments and wraps around the buffer.

2.  **CPU Access**:
    -   The CPU reads data from the buffer using the Wishbone interface.
    -   Because the hardware continuously overwrites the buffer, the CPU must read data before it is overwritten. See [Ring Buffer Alignment](../ringbuffer-alignment.md) and [Asynchronous Communication](../asynchronous-communication.md) for protocols that handle this.
