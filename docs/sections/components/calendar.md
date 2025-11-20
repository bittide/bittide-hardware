<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

## Calendar

The Calendar component is a programmable state machine that drives the configuration of other components (like the Scatter Unit, Gather Unit, or Switch) on a cycle-by-cycle basis. It allows for time-division multiplexing of resources by cycling through a sequence of pre-programmed configurations.

### Architecture

{{#drawio path="diagrams/components.drawio" page=2}}

The Calendar consists of two memories (buffers) to allow for double-buffering: an **active** calendar and a **shadow** calendar.
- The **active** calendar drives the output signals to the target component.
- The **shadow** calendar can be reconfigured via the Wishbone interface without interrupting the operation of the active calendar.

The active and shadow calendars can be swapped at the end of a **metacycle**. A metacycle is one full iteration through the active calendar's entries, including all repetitions.

### Operation

Each entry in the calendar consists of:
1.  **Configuration Data**: The actual control signals for the target component.
2.  **Repetition Count**: The number of additional cycles this configuration should remain active.
    - `0`: The entry is valid for 1 cycle.
    - `N`: The entry is valid for `N + 1` cycles.

The Calendar iterates through the entries in the active buffer. When it reaches the end of the active buffer (determined by the configured depth), it loops back to the beginning, completing a metacycle.

### Double Buffering and Swapping

To update the schedule:
1.  Software writes new entries into the **shadow** calendar using the Wishbone interface.
2.  Software configures the depth of the shadow calendar.
3.  Software arms the swap mechanism by writing to the `swapActive` register.

The swap does not happen immediately. It occurs only when the active calendar completes its current metacycle. This ensures that the schedule is always switched at a deterministic point, preventing glitches or partial schedules.

### Registers

The Calendar exposes the following registers via the Wishbone interface:

| Name | Access | Description |
|---|---|---|
| `shadowEntry` | Write-Only | Stores the entry data before writing it to the shadow calendar. Writing to `writeAddr` commits this data. |
| `writeAddr` | Write-Only | Writing an address to this register triggers a write of the data currently in `shadowEntry` to the shadow calendar at the specified address. |
| `readAddr` | Write-Only | Sets the address for reading from the shadow calendar. The data will be available in `shadowEntry` (read-only view). |
| `shadowEntry` | Read-Only | Reads the data from the shadow calendar at the address specified by `readAddr`. |
| `shadowDepthIndex` | Read-Write | The index of the last valid entry in the shadow calendar. The size of the calendar is `shadowDepthIndex + 1`. |
| `swapActive` | Write-Only | Writing any value to this register arms the swap mechanism. The active and shadow buffers will swap at the end of the current metacycle. |
| `metacycleCount` | Read-Only | A 32-bit counter that increments every time a metacycle completes. |
| `endOfMetacycle` | Write-Only | Writes to this register are only acknowledged by the hardware when the metacycle ends. This can be used for synchronization. |
