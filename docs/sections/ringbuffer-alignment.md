<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Ringbuffer Alignment Protocol

## Definitions
*   **Transmitting Ringbuffer (TX):** Local memory written by CPU, read by hardware using a wrapping counter.
*   **Receiving Ringbuffer (RX):** Local memory written by hardware using a wrapping counter, read by CPU.

## Context
In a Bittide system, nodes operate in a globally synchronous manner despite being asynchronous devices with unknown start times. Communication occurs via ringbuffers. When TX and RX ringbuffers are the same size, the address mapping between them is constant, determined by (logical) network latency and the start time difference between nodes.

## Objective
Determine the constant offset between the transmit address (TX) and receive address (RX) for each link to enable reliable asynchronous communication.

## Alignment Algorithm
1.  **Initialization:** Set all ringbuffers to a uniform default size.
2.  **Announce:** Each CPU writes a recognizable message containing the non-zero state `ALIGNMENT_ANNOUNCE` to index `0` of its TX ringbuffer and clears all other positions.
3.  **Search:** Each CPU scans its RX ringbuffer for the `ALIGNMENT_ANNOUNCE` message.
4.  **Determine Offset:** Upon finding the message at `RX_Index`, the CPU stores the `RX_Index`, to be used as future read offset.
5.  **Acknowledge:** The CPU updates index `0` of its TX ringbuffer with the state `ALIGNMENT_RECEIVED`.
6.  **Confirm:** The CPU monitors the RX ringbuffer until it receives `ALIGNMENT_RECEIVED` from its neighbor.
7.  **Finalize:** Once the CPU has received `ALIGNMENT_RECEIVED`, the link is aligned. The offset is stored for future communication and we proceed to normal operation.

## Resulting Interface: AlignedRingbuffer
Upon successful alignment, the system can instantiate an `AlignedRingbuffer` abstraction. This interface handles the offset calculations transparently, allowing the CPU to read and write to the ringbuffers without concern for alignment.

## Communication Challenges
While the `AlignedRingbuffer` provides logical connectivity, the physical link remains unreliable due to the interaction between the read/write counters of the ringbuffers and asynchronous CPU access:

1.  **Continuous Hardware Operation:** The hardware continuously cycles through the ringbuffers at the network link speed.
2.  **Asynchronous CPU Access:** The CPU operates asynchronously and often slower than the network link.

This leads to specific failure modes:
*   **Data Corruption (Pointer Overtaking):**
    *   *TX Side:* If the hardware's read pointer overtakes the CPU's write pointer during a write, a torn frame is sent.
    *   *RX Side:* If the hardware's write pointer overtakes the CPU's read pointer during a read, the message is corrupted.
*   **Data Loss:** If the CPU does not read from the RX ringbuffer every iteration, the hardware will overwrite unread data.
*   **Data duplication:** If the CPU does not write to the TX ringbuffer every iteration, the hardware will resend old data.

Reliable communication requires a higher-level protocol to handle these errors.
