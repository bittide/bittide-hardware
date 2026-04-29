<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Software UGN Demo

This chapter describes the specific hardware setup used to perform a test where each node
is able to determine the UGNs to its neighbors without communicating with a host PC that
is able to read all of the information from each of the nodes in the system. This is
accomplished through the firmware on the management unit (MU).

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
    2. Sets the channels to "user" mode. This makes the channels accept data from the outside world instead of their negotiation state machinery.
    3. Waits for hardware UGN to be captured
    4. Start auto-centering elastic buffers
    5. Waits for clocks to be considered stable
    6. Stops auto-centering the elastic buffers
    7. Prints UGNs captured by hardware component to UART.
    8. Aligns the [transmit](../components/transmit-ring-buffer.md)/[receive](../components/receive-ring-buffer.md) ring buffers.
    9. Calls the "c_main" and runs the software UGN discovery protocol

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
- 7 [transmit](../components/transmit-ring-buffer.md) and [receive](../components/receive-ring-buffer.md) ring buffers, one of each per link

### Management unit
Connected components:
- Timer
- UART (for debugging)
- FPGA DNA register

The management unit is connected to the bittide interconnect network via [transmit](../components/transmit-ring-buffer.md)/[receive](../components/receive-ring-buffer.md) ring buffers. In this demo the management unit runs a distributed protocol to discover the Uninterpretable Garbage Numbers (UGNs) of the network links. The protocol uses the aligned ring buffers to exchange timestamped messages with neighbors, calculating the propagation delays in software. For a detailed description of the procedure, see [Software UGN Discovery Procedure](soft-ugn-procedure.md).

To change the binary run on this CPU, one may either:
- Edit `bittide-instances/src/bittide/Instances/Hitl/SoftUgnDemo/Driver.hs`, line 215 (at
  time of writing) to use another binary instead of `soft-ugn-mu`
- Edit the source files in `firmware-binaries/soft-ugn-mu/` to change the binary
  pre-selected by the driver function

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

Tests are configured to run the following binaries on the system's CPUs:
- Boot CPU: `switch-demo1-boot` (`firmware-binaries/demos/switch-demo1-boot`)
- Clock control CPU: `clock-control` (`firmware-binaries/demos/clock-control`)
- Management unit: `soft-ugn-mu` (`firmware-binaries/demos/soft-ugn-mu`)

One may change this by either:
1. Changing the driver function so that it loads different binaries onto the CPUs. This
   may be accomplished by changing which binary name is used with each of the `initGdb`
   function calls.
2. Changing the source code for the binaries. The locations for them are listed above.
