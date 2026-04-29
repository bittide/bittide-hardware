<!--
SPDX-FileCopyrightText: 2026 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Smoltcp Demo

This chapter describes the specific hardware setup used to demonstrate reliable asynchronous
communication between bittide nodes using the [smoltcp](https://github.com/smoltcp-rs/smoltcp)
lightweight TCP/IP stack. The demo builds on top of the [Ring Buffer Alignment](../ringbuffer-alignment.md)
and [Asynchronous Communication](../asynchronous-communication.md) protocols to establish TCP
connections over every inter-node link and uses them to collect UGN information across the network.

## Motivation

Raw aligned ring buffers are unreliable: data corruption and loss can occur when hardware and
software access pointers overlap (see [Asynchronous Communication](../asynchronous-communication.md)
for details). This demo proves that a standard TCP/IP stack can provide reliable communication
over these links, handling error detection, retransmission, and flow control transparently.

## Architecture

This demo reuses the same hardware design as the [Software UGN Demo](soft-ugn-demo.md)
(`softUgnDemoTest` top entity). The only difference is the firmware loaded onto the management
unit.

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
    2. Brings up all 7 inter-node links (transceiver negotiation, elastic buffer centering, UGN capture)
    3. Aligns all [transmit](../components/transmit-ring-buffer.md)/[receive](../components/receive-ring-buffer.md) ring buffers using the [alignment protocol](../ringbuffer-alignment.md)
    4. Creates a `LinkInterface` (TCP connection) per link
    5. Exchanges DNA identifiers and port numbers with all neighbors
    6. Executes the UGN collection protocol (manager/subordinate)
    7. Prints "Demo complete." to UART

### Domain related
Components:
- Boot CPU (BOOT)
- Transceivers
- Domain difference counters
- Clock control (CC)
- Elastic buffers (one per incoming transceiver link)
- Hardware UGN capture

### Node related
Components:
- Management unit (MU)
- 7 [transmit](../components/transmit-ring-buffer.md) and [receive](../components/receive-ring-buffer.md) ring buffers, one of each per link

### Management unit
Connected components:
- Timer
- UART (for debugging)
- FPGA DNA register

## Networking stack

Each link runs a `smoltcp` TCP/IP stack with the aligned ring buffer as the physical device
(`smoltcp::phy::Device`). Key parameters:

- **Medium**: `Medium::Ip` (no Ethernet headers; point-to-point)
- **MTU**: 1500 bytes
- **Addressing**: Static IP (all nodes use the same placeholder address)
- **Connection**: TCP simultaneous open (both sides connect symmetrically, no client/server distinction)
- **Buffer sizes**: 256 bytes TX/RX per TCP socket

## Protocol

The demo executes a distributed UGN collection protocol in three steps:

### Step 1 — TCP connection establishment
All 7 `LinkInterface` instances initiate TCP simultaneous open. The demo waits until all
connections are established.

### Step 2 — Identity exchange
Each node sends its 12-byte FPGA DNA and local port index over each link. Both sides learn
the identity and port of their neighbor with a 1-second timeout.

### Step 3 — UGN report collection
One node is designated as **manager** (determined by matching a known DNA). All other nodes are
**subordinates**.

- **Subordinates** wait for a `RequestUgnReport` command from the manager, then send their local
  UGN edges (source/destination node DNA, port, and UGN value).
- **Manager** sends `RequestUgnReport` to each neighbor in turn, collects all edges, and
  assembles a complete UGN graph for the network.

## Differences from the Software UGN Demo

| Aspect | [Software UGN Demo](soft-ugn-demo.md) | Smoltcp Demo |
|--------|---------------------------------------|--------------|
| **Communication** | Direct ring buffer reads/writes | TCP/IP over ring buffers |
| **Reliability** | Application-level protocol | TCP retransmission and checksums |
| **UGN measurement** | Software based | Hardware based |
| **UGN collection** | Serial prints to host PC | Using TCP based `LinkInterface` |
| **Firmware** | `soft-ugn-mu` (C + Rust) | `smoltcp-demo` (Rust) |

## Running tests
One may specifically run the smoltcp demo test by making a
`.github/synthesis/debug.json` with the following contents:

```json
[
  {"top": "softUgnDemoTest",       "stage": "test", "cc_report": true}
]
```

The host driver (`driverTcp` in `bittide-instances/src/Bittide/Instances/Hitl/SoftUgnDemo/Driver.hs`)
programs the boot CPU, clock control CPU, and management unit, then waits for the MU to print
`"Demo complete."` over UART.

Tests are configured to run the following binaries on the system's CPUs:
- Boot CPU: `switch-demo1-boot` (`firmware-binaries/demos/switch-demo1-boot`)
- Clock control CPU: `clock-control` (`firmware-binaries/demos/clock-control`)
- Management unit: `smoltcp-demo` (`firmware-binaries/demos/smoltcp-demo`)

One may change this by either:
1. Changing the driver function so that it loads different binaries onto the CPUs. This
   may be accomplished by changing which binary name is used with each of the `initGdb`
   function calls.
2. Changing the source code for the binaries. The locations for them are listed above.
