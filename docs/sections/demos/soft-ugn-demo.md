<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Software UGN Demo

This chapter describes the specific hardware setup used to perform a test where each node
is able to determine the UGNs to its neighbors without communicating with a host PC that
is able to read all of the information from each of the nodes in the system. This is
accomplished through the firmware on the GPPE.

## Architecture
{{#drawio path="diagrams/softUgnDemo.drawio" page=0}}

### Initialization sequence
1. "Boot" CPU
    1. Gets programmed by the host
    2. Programs the clock boards
    3. Gets transceiver block out of reset (enabling the bittide domain / other CPUs)
    4. Activates each transceiver channel and waits until they've negotiated a link with their neighbors.
    5. Prints "all done" message to UART.
2. Clock control CPU
    1. Gets programmed by the host
    2. Calibrates clocks
    3. Prints "all done" message to UART.
    4. (Keeps calibrating clocks.)
3. Management unit CPU
    1. Gets programmed by the host
    2. Centers elastic buffers
    3. Initializes the [scatter](../components/scatter-unit.md)/[gather](../components/gather-unit.md) [calendars](../components/calendar.md).
    4. Sets the channels to "user" mode. This makes the channels accept data from the outside world instead of their negotiation state machinery.
    5. Prints UGNs captured by hardware component to UART.
4. General purpose procesisng element (PE)
    1. Gets programmed by the host
    2. Waits for the management unit to initialize the calendars
    3. Calls the "c_main" and runs the software UGN discovery protocol
    4. Prints the discovered UGNs over UART

### Domain related
Components:
- Boot CPU (BOOT)
- Transceivers
- Domain difference counters
- Clock control (CC)
- Elastic buffers (one per incoming transceiver link)
- Hardware UGN capture (for comparison)

### Node related
Components:
- Management unit (MU)
- 1x general purpose processing element (PE)
- 7 [scatter](../components/scatter-unit.md) and [gather](../components/gather-unit.md) units, one of each per elastic buffer

### Management unit
Connected components:
- Timer
- UART (for debugging)
- FPGA DNA register

The management unit has access to and is responsible for all [scatter](../components/scatter-unit.md)/[gather](../components/gather-unit.md) [calendars](../components/calendar.md) in
the node. In this demo, it programs the calendars with increasing values (0, 1, 2, ...),
effectively creating a transparent link for the GPPE to access the scatter/gather units directly.
It also centers the elastic buffers to ensure stable communication.

To change the binary run on this CPU, one may either:
- Edit `bittide-instances/src/bittide/Instances/Hitl/SoftUgnDemo/Driver.hs`, line 215 (at
  time of writing) to use another binary instead of `soft-ugn-mu`
- Edit the source files in `firmware-binaries/soft-ugn-mu/` to change the binary
  pre-selected by the driver function

### General purpose processing element
This component is labeled as "PE" in the diagram above. Connected components:
- 7 [scatter](../components/scatter-unit.md) and [gather](../components/gather-unit.md) units, one of each per elastic buffer
- UART (for debugging)
- Timer
- FPGA DNA register

The general purpose processing element runs the `soft-ugn-gppe` firmware, which implements a distributed protocol to discover the Uninterpretable Garbage Numbers (UGNs) of the network links. For a detailed description of the procedure, see [Software UGN Discovery Procedure](soft-ugn-procedure.md). It uses the scatter/gather units (enabled by the MU) to exchange timestamped messages with neighbors,
calculating the propagation delays in software.

### Debugging related
- UART arbiter
- JTAG interconnect
- Integrated logic analyzers
- `SYNC_IN`/`SYNC_OUT`

## Running tests
One may specifically run the software UGN demo test by making a
`.github/synthesis/debug.json` with the following contents:

```json
[
  {"top": "softUgnDemoTest",       "stage": "test", "cc_report": true}
]
```

At the time of writing, the clock control CPU stabilizes system. The driver running
on the host (`bittide-instances/src/bittide/Instances/Hitl/SoftUgnDemo/Driver.hs`)
then releases the reset of the management unit CPU. In turn, this CPU will center
the elastic buffers, initialize the scatter/gather calendars, and print out the UGNs captured using the hardware UGN capture component over UART. Finally, the general purpose processing element is started. It executes the software UGN discovery protocol and prints the results over UART. The host driver then compares the hardware-captured UGNs with the software-discovered UGNs to verify correctness.

Tests are configured to run the following binaries on the system's CPUs:
- Boot CPU: `switch-demo1-boot` (`firmware-binaries/demos/switch-demo1-boot`)
- Clock control CPU: `clock-control` (`firmware-binaries/demos/clock-control`)
- Management unit: `soft-ugn-mu` (`firmware-binaries/demos/soft-ugn-mu`)
- General purpose processing element: `soft-ugn-gppe` (`firmware-binaries/demos/soft-ugn-gppe`)

One may change this by either:
1. Changing the driver function so that it loads different binaries onto the CPUs. This
   may be accomplished by changing which binary name is used with each of the `initGdb`
   function calls.
2. Changing the source code for the binaries. The locations for them are listed above.
