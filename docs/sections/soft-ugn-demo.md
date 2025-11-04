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
1. FPGA board reset
2. SPI programming of clock generators
3. Transceiver initialization
4. Clock control to stabilize Bittide domain
5. Elastic buffer centering
6. UGN measurement

### Domain related
Components:
- Transceivers
- Domain difference counters
- Clock control (CC)
- SPI programmer for clock generator
- Elastic buffers (one per incoming transceiver link)
- UGN capture

### Node related
Components:
- Management unit (MU)
- 1x general purpose processing element (GPPE)
- N scatter and gather units, one of each per elastic buffer

### Management unit
Connected components:
- RISC-V core
- Time counter + stalling
- UART (for debugging)
- FPGA DNA register
- CPU identifier register

The management unit has access to and is responsible for all scatter/gather calendars in
the node.

To change the binary run on this CPU, one may either:
- Edit `bittide-instances/src/Bittide/Instances/Hitl/Driver/SoftUgnDemo.hs`, line 215 (at
  time of writing) to use another binary instead of `soft-ugn-mu`
- Edit the source files in `firmware-binaries/soft-ugn-mu/` to change the binary
  pre-selected by the driver function

### General purpose processing element
This component is labeled as "PE" in the diagram above. Connected components:
- N scatter and gather units, one of each per elastic buffer
- UART (for debugging)
- CPU identifier register

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
At time of writing, the actual "test" done by the system is simply a sanity check of the
CPUs by waiting for them to output something over UART. To change this, adjust the code
in the driver function (located in
`bittide-instances/src/Bittide/Instances/Hitl/Driver/SoftUgnDemo.hs`) _after_ the
`"Waiting for stable links"` action (ends on line 247 presently) and _before_ the line
which does `liftIO goDumpCcSamples` (line 274 presently).

Additionally, tests are configured to run the following binaries on the system's CPUs:
- Clock control CPU: `clock-control` (`firmware-binaries/clock-control`)
- Management unit: `soft-ugn-mu` (`firmware-binaries/soft-ugn-mu`)
- General purpose processing element: `soft-ugn-gppe` (`firmware-binaries/soft-ugn-gppe`)

One may change this by either:
1. Changing the driver function so that it loads different binaries onto the CPUs. This
   may be accomplished by changing which binary name is used with each of the `initGdb`
   function calls, which are on lines 203, 215, and 228 at time of writing.
2. Changing the source code for the binaries. The locations for them are listed above.
