<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

## Scatter Unit

The Scatter Unit is a hardware component designed to receive data frames from a Bittide link and store them into a local memory. It uses a double-buffered memory architecture and a configurable **Calendar** to determine the write address for each incoming frame. This allows for cycle-accurate, deterministic data reception.

### Architecture

{{#drawio path="diagrams/components.drawio" page=0}}

The Scatter Unit consists of:
-   **Double-Buffered Memory**: Two memory buffers (Active and Shadow).
    -   **Active Buffer**: Receives data from the Bittide link.
    -   **Shadow Buffer**: Can be read by the CPU via the Wishbone interface.
-   **Calendar**: Determines the write address in the Active Buffer for each incoming frame.
-   **Wishbone Interface**: Allows the CPU to read received data and monitor metacycle progress.

### Operation

1.  **Data Reception**:
    -   In each cycle, if a valid frame arrives from the Bittide link, the **Calendar** provides the write address for the Active Buffer.
    -   The frame is written to the Active Buffer at that address.

2.  **Buffer Swapping**:
    -   The Active and Shadow buffers are swapped at end of each **metacycle**.
    -   A metacycle is defined by the Calendar's schedule.
    -   After the swap, the data that was just received becomes available in the Shadow Buffer for the CPU to read.

3.  **CPU Access**:
    -   The CPU reads the data from the Shadow Buffer using the Wishbone interface.
    -   To ensure data consistency, the CPU can synchronize with the metacycle using the stalling mechanism.
