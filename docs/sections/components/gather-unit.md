<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

## Gather Unit

The Gather Unit is a hardware component designed to transmit data frames over a Bittide link. It uses a double-buffered memory architecture and a configurable **Calendar** to determine which data to send in each cycle. This ensures deterministic data transmission.

### Architecture

{{#drawio path="diagrams/components.drawio" page=1}}

The Gather Unit consists of:
-   **Double-Buffered Memory**: Two memory buffers (Active and Shadow).
    -   **Active Buffer**: Provides data to the Bittide link.
    -   **Shadow Buffer**: Can be written to by the CPU via the Wishbone interface.
-   **Calendar**: Determines the read address in the Active Buffer for each outgoing frame.
-   **Wishbone Interface**: Allows the CPU to write data to be transmitted and monitor metacycle progress.

### Operation

1.  **Data Transmission**:
    -   Each clock tick, the **Calendar** provides the read address for the Active Buffer.
    -   The data at that address is read from the Active Buffer and sent over the Bittide link.

2.  **Buffer Swapping**:
    -   The Active and Shadow buffers are swapped at end of each **metacycle**.
    -   A metacycle is defined by the Calendar's schedule.
    -   After the swap, the data that was written by the CPU becomes the active data being transmitted.

3.  **CPU Access**:
    -   The CPU writes the data to the Shadow Buffer using the Wishbone interface.
    -   **Byte Enables**: The Wishbone interface is 32-bit, but the Gather Unit memory is 64-bit. The hardware uses the byte enables to allow writing to the upper or lower 32 bits of the 64-bit memory words.
    -   To ensure data consistency, the CPU can synchronize with the metacycle using the stalling mechanism.
