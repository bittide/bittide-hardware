<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Asynchronous Communication Protocol

## Context
In a bittide system, we need asynchronous communication between nodes, particularly during the boot phase. The [Ringbuffer Alignment Protocol](ringbuffer-alignment.md) provides an `AlignedRingbuffer` abstraction that allows for packet exchange.

However, as described in that protocol's documentation, the raw `AlignedRingbuffer` link is unreliable, subject to packet corruption and loss due to hardware/software speed mismatches.

## Objective
Establish a reliable, asynchronous, point-to-point communication channel between nodes over the potentially unreliable `AlignedRingbuffer` links.

## Proposed Solution
Leverage the **TCP/IP** protocol suite to handle error detection, retransmission, and flow control. We will use the `smoltcp` library, a lightweight TCP/IP stack designed for embedded systems, to implement this layer. Note that future versions of bittide will probably use a bespoke network stack for asynchronous communication.

## Implementation Strategy

### 1. Network Interface (`smoltcp::phy::Device`)
We will implement the `smoltcp::phy::Device` trait for the Aligned Ringbuffer.
*   **Medium:** Use `Medium::Ip` to minimize overhead (no Ethernet headers required for point-to-point).
*   **MTU:** Set to **1500 bytes** (standard Ethernet size) to accommodate typical payloads.

### 2. Framing & Alignment
*   The underlying Aligned Ringbuffer abstraction ensures packets are read from the correct aligned memory location.
*   **Packet Boundaries:** The length of each packet will be derived directly from the **IP Header** length field.

### 3. Addressing
*   **Topology:** Initially restricted to **Peer-to-Peer** links.
*   **IP Assignment:** Placeholder IPs, if necessary we use static addressing derived from unique hardware identifiers (e.g., FPGA DNA or Port ID) to avoid the complexity of DHCP.

### 4. Demo Application
Develop a proof-of-concept application that:
1.  Initializes the Aligned Ringbuffer.
2.  Sets up a `smoltcp` interface.
3.  Establishes a TCP connection between two nodes.
4.  Transfers data to verify reliability against induced packet loss/corruption.

## Assumptions
*   An **Aligned Ringbuffer** abstraction exists that provides a read/write interface for single aligned packets.
*   The ringbuffer size is sufficient to hold at least one MTU-sized packet plus overhead.
