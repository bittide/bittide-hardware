<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Switch Demo with ASIC processing element

This chapter describes the specific hardware setup used to perform a demonstration of the
Bittide switch.

## Architecture
{{#drawio path="diagrams/switchDemoAsic.drawio" page=2}}

### Initialization sequence
1. FPGA board reset
2. SPI programming of clock generators
3. Transceiver initialization
4. Clock control to stabilize Bittide domain
5. Elastic buffer centering
6. Start running binary on management unit
7. Start running binary on processing element(s)

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
- Crossbar
- Crossbar calendar
- Null link
- General purpose processing element (GPPE)
- 1x scatter/gather units

### Management unit
- Timer
- UART (for debugging)
- FPGA DNA register

The management unit has access to and is responsible for all scatter/gather calendars in
the node, as well as the crossbar calendar.

To change the binary run on this CPU, one may either:
- Edit `bittide-instances/src/Bittide/Instances/Hitl/SwitchDemo/Driver.hs`, line 500
  (at time of writing) to use another binary instead of `switch-demo1-mu`
- Edit the source files in `firmware-binaries/switch-demo1-mu/` to change the binary
  pre-selected by the driver function

### Application specific processing element
This component is labeled as "PE" in the diagram above. It is directly connected to the
output of the crossbar without any buffering, and as such can only work on data as it is
streamed to it, and the manner in which this happens is determined by the crossbar
calendar.

The processing element itself can be instructed to read from its incoming link for a
configurable number of clock cycles starting at a very specific one. Similarly, it can
write to its outgoing link for a configurable number of clock cycles starting at a very
specific one. The demo is such that UGNs are received from all nodes in the system and
with that a "schedule" is created for when to read and write on the link on each PE. If
the schedule is correct and the bittide property holds, a "thread" through all nodes is
created. For more information see [this presentation](https://docs.google.com/presentation/d/1AGbAJQ1zhTPtrekKnQcthd0TUPyQs-zowQpV1ux4k-Y/).



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
  {"top": "switchDemoGppeTest",       "stage": "test", "cc_report": true}
]
```

At the time of writing, the clock control CPU stabilizes system. The driver running
on the host (`bittide-instances/src/Bittide/Instances/Hitl/SwitchDemo/Driver.hs`)
then releases the reset of the management unit CPU. In turn, this CPU will center
the elastic buffers and print out the UGNs captured using the hardware UGN capture
component over UART. Finally, the general purpose processing element has its
reset deasserted. It simply prints "Hello from C!".

Tests are configured to run the following binaries on the system's CPUs:
- Clock control CPU: `clock-control` (`firmware-binaries/demos/clock-control`)
- Management unit: `switch-demo1-mu` (`firmware-binaries/demos/switch-demo1-mu`)

One may change this by either:
1. Changing the driver function so that it loads different binaries onto the CPUs. This
   may be accomplished by changing which binary name is used with each of the `initGdb`
   function calls.
2. Changing the source code for the binaries. The locations for them are listed above.
