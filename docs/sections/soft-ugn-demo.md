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
    3. Sets the channels to "user" mode. This makes the channels accept data from the outside world instead of their negotiation state machinery.
    4. Prints UGNs captured by hardware component to UART.
4. General purpose procesisng element (PE)
    1. Gets programmed by the host
    2. Prints "Hello!" message over UART
    3. Calls the "c_main"
    4. Prints "Hello from C!" over UART

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
- 7 scatter and gather units, one of each per elastic buffer

### Management unit
Connected components:
- Timer
- UART (for debugging)
- FPGA DNA register

The management unit has access to and is responsible for all scatter/gather calendars in
the node.

To change the binary run on this CPU, one may either:
- Edit `bittide-instances/src/Bittide/Instances/Hitl/SoftUgnDemo/Driver.hs`, line 215 (at
  time of writing) to use another binary instead of `soft-ugn-mu`
- Edit the source files in `firmware-binaries/soft-ugn-mu/` to change the binary
  pre-selected by the driver function

### General purpose processing element
This component is labeled as "PE" in the diagram above. Connected components:
- 7 scatter and gather units, one of each per elastic buffer
- UART (for debugging)
- Timer
- FPGA DNA register

The general purpose processing element has no functionality other than printing "Hello
from C!" over UART.

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
on the host (`bittide-instances/src/Bittide/Instances/Hitl/SoftUgnDemo/Driver.hs`)
then releases the reset of the management unit CPU. In turn, this CPU will center
the elastic buffers and print out the UGNs captured using the hardware UGN capture
component over UART. Finally, the general purpose processing element has its
reset deasserted. It simply prints "Hello from C!".

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
