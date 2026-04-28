<!--
SPDX-FileCopyrightText: 2026 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Wire Demo
The [switch demo](switch-demo-asic.md) shows that we could use the bittide mechanism to thread a path through an 8-node network, all statically scheduled. To simplify things, we programmed a static calendar and used a calculator to produce the exact clock cycles at which the PE should send and receive data. For more details, see [this presentation](https://docs.google.com/presentation/d/1AGbAJQ1zhTPtrekKnQcthd0TUPyQs-zowQpV1ux4k-Y/edit?usp=sharing).

While talking to potential users of bittide we've found that they're not interested in this switching behavior (yet). Instead, they're more interested in the "wire" behavior of bittide and would simply like to be kept away from anything else.

The idea is still the same: thread a path through a bunch of nodes/FPGAs (in our case 8). The implementation is much simpler than the previous iteration though: instead of using calendars to do time multiplexing on every link, the links are connected to the PE through a single mux. This mux can either be set to serve the MU or the PE.


## Architecture
{{#drawio path="diagrams/wireDemo.drawio" page=0}}

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
- 7 [transmit](../components/transmit-ring-buffer.md) and [receive](../components/receive-ring-buffer.md) ring buffers, one of each per elastic buffer
- Programmable mux
- WireDemoPe, a processing elemenent specific for this demo
- WireDemoPeConfig, a bus accessible device with a writable configuration for the processing element

### Management unit
Connected components:
- Timer
- UART (for debugging)
- FPGA DNA register

The management unit has access to and is responsible for all [transmit](../components/transmit-ring-buffer.md)/[receive](../components/receive-ring-buffer.md) ring buffers in the node. In this demo these are not used.

To change the binary run on this CPU, one may either:
- Edit `bittide-instances/src/bittide/Instances/Hitl/WireDemo/Driver.hs`, line 215 (at
  time of writing) to use another binary instead of `wire-demo-mu`
- Edit the source files in `firmware-binaries/wire-demo-mu/` to change the binary
  pre-selected by the driver function

### Wire Demo processing element
#### PE (`wireDemoPe`)
Takes two configuration values (from "PE config"):
- What link to read from
- What link to write to

The PE is active for two cycles after reset:
- First cycle: read from link indicated by config (or source static "0").
- Second cycle: write value_read_in_first_cycle XOR fpga_dna_lsbs to link indicated by config (or do nothing)

#### PE Config (`wireDemoPeConfig`)
Has 3 bus accessible registers:
- `read_link :: Maybe (Index 7)`
- `write_link :: Maybe (Index 7)`
- `written_data :: Maybe (BitVector 64)`

The first 2 registers are used to configure the PE. The third register stores the data written by the PE over the configured link. This is used to verify the demo works. The data in the last PE Config should be the XOR of all DNAs.

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
  {"top": "wireDemoTest",       "stage": "test", "cc_report": true}
]
```

At the time of writing, the clock control CPU stabilizes system. The driver running
on the host (`bittide-instances/src/bittide/Instances/Hitl/WireDemo/Driver.hs`)
then releases the reset of the management unit CPU. In turn, this CPU will center
the elastic buffers and print out the UGNs captured using the hardware UGN capture component over UART.

The host driver then calculates the a schedule to thread a path through all nodes and programs all PE Configs and programmable muxes. After waiting for a static time the host driver reads the `written_data` register in each PE Config device and verifies it is equal to the XOR of all device DNAs up to that node. The last node should therefore have stored the XOR of all device DNAs.

Tests are configured to run the following binaries on the system's CPUs:
- Boot CPU: `switch-demo1-boot` (`firmware-binaries/demos/switch-demo1-boot`)
- Clock control CPU: `clock-control` (`firmware-binaries/demos/clock-control`)
- Management unit: `wire-demo-mu` (`firmware-binaries/demos/wrie-demo-mu`)

One may change this by either:
1. Changing the driver function so that it loads different binaries onto the CPUs. This
   may be accomplished by changing which binary name is used with each of the `initGdb`
   function calls.
2. Changing the source code for the binaries. The locations for them are listed above.
