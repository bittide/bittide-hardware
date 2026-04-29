<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Asynchronous Communication Protocol

## Context
In a bittide system, we need asynchronous communication between nodes, particularly during the boot phase. The [Ring Buffer Alignment Protocol](ringbuffer-alignment.md) provides an `AlignedRing Buffer` abstraction that allows for packet exchange.

However, as described in that protocol's documentation, the raw `AlignedRing Buffer` link is unreliable, subject to packet corruption and loss due to hardware/software speed mismatches.

## Objective
Establish a reliable, asynchronous, point-to-point communication channel between nodes over the potentially unreliable `AlignedRing Buffer` links.

## Solution
Leverage the **TCP/IP** protocol suite to handle error detection, retransmission, and flow control. We use the `smoltcp` library, a lightweight TCP/IP stack designed for embedded systems, to implement this layer.

For a working demonstration of this approach, see the [Smoltcp Demo](demos/smoltcp-demo.md).

## Implementation

### 1. Network Interface (`smoltcp::phy::Device`)
The `smoltcp::phy::Device` trait is implemented for the Aligned Ring Buffer (`RingBufferDevice` in `firmware-support/bittide-sys/src/smoltcp/ring_buffer.rs`).
*   **Medium:** Use `Medium::Ip` to minimize overhead (no Ethernet headers required for point-to-point).
*   **MTU:** Set to **1500 bytes** (standard Ethernet size) to accommodate typical payloads.

### 2. Framing & Alignment
*   The underlying Aligned Ring Buffer abstraction ensures packets are read from the correct aligned memory location.
*   **Packet Boundaries:** The length of each packet will be derived directly from the **IP Header** length field.

### 3. Addressing
*   **Topology:** Initially restricted to **Peer-to-Peer** links.
*   **IP Assignment:** Placeholder IPs, if necessary we use static addressing derived from unique hardware identifiers (e.g., FPGA DNA or Port ID) to avoid the complexity of DHCP.

### 4. Demo Application
The [Smoltcp Demo](demos/smoltcp-demo.md) is a proof-of-concept application that:
1.  Initializes and aligns the ring buffers.
2.  Sets up a `smoltcp` interface per link.
3.  Establishes TCP connections between all neighboring nodes using simultaneous open.
4.  Exchanges identity information and collects UGN reports over TCP to verify reliability.

## Assumptions
*   An **Aligned Ring Buffer** abstraction exists that provides a read/write interface for single aligned packets.
*   The ringbuffer size is sufficient to hold at least one MTU-sized packet plus overhead.
