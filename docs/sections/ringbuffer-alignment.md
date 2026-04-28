<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Ring Buffer Alignment Protocol

## Definitions
*   **Transmitting Ring Buffer (TX):** Local memory written by CPU, read by hardware using a free-running wrapping counter. Has an **enable** register: when enabled, the hardware transmits buffer contents; when disabled, zeroes are transmitted, but the counter continues to increment.
*   **Receiving Ring Buffer (RX):** Local memory written by hardware using a free-running wrapping counter, read by CPU. Has an **enable** register (incoming frames are ignored when disabled, but the counter keeps incrementing) and a **clear\_at\_count** register that resets the write counter to zero when it reaches the configured value.

## Context
In a bittide system, nodes operate in a globally synchronous manner despite being asynchronous devices with unknown start times. Communication occurs via ring buffers. When TX and RX ring buffers are the same size, the address mapping between them is constant, determined by (logical) network latency and the start time difference between nodes.

Because nodes start at different times, data written to TX index `0` by one node may arrive at an arbitrary RX index on the neighbor. The alignment protocol measures this offset and uses the hardware `clear_at_count` register to physically realign the RX write counter so that TX index `0` maps to RX index `0`.

## Objective
Physically synchronize the TX and RX hardware counters on each link so that corresponding indices map to each other, enabling reliable asynchronous communication without software-level offset tracking.

## Alignment Algorithm

The alignment is implemented as a per-link state machine (`AlignedReceiveBuffer::align_step`) with four phases after the initial setup. Both sides of a link run the same algorithm simultaneously.

### Phase 0 — Unaligned (setup)
1. Clear the TX buffer (write zeroes to all positions).
2. Write the marker `ALIGNMENT_ANNOUNCE` (`0xBADC0FFEE`) to TX index `0`.
3. Enable both the TX and RX ring buffers.
4. Transition to **FindingAlignment**.

### Phase 1 — FindingAlignment
Scan the entire RX buffer for a non-zero marker (`ALIGNMENT_ANNOUNCE` or `ALIGNMENT_ACKNOWLEDGE`).

*   **Marker found at RX index `0`:** The counters are already aligned. Write `ALIGNMENT_ACKNOWLEDGE` (`0xDEADABBA`) to TX index `0` and transition to **AcknowledgingAlignment**.
*   **Marker found at RX index `n` (n ≠ 0):** Write `n` to the `clear_at_count` register. The next time the hardware write counter reaches `n`, it resets to `0`, shifting the marker to RX index `0` on a subsequent scan. Stay in **FindingAlignment**.
*   **No marker found:** Stay in **FindingAlignment** (keep scanning).

### Phase 2 — AcknowledgingAlignment
Poll RX index `0`:

*   When the value is `ALIGNMENT_ACKNOWLEDGE` (partner sent its acknowledgement) **or** `0` (partner already disabled TX): disable our TX (`set_enable(false)`) and transition to **WaitingForZeroes**.

The invariant is that we only disable our TX *after* confirming the partner has seen our marker and acknowledged.

### Phase 3 — WaitingForZeroes
Poll RX index `0`:

*   When the value is `0` (partner has also disabled TX): disable our RX (`set_enable(false)`) and transition to **Aligned**. Alignment is complete.

This two-step shutdown (AcknowledgingAlignment → WaitingForZeroes) prevents a race where one side disables TX before the partner has seen the acknowledgement.

### Summary

| Phase | TX output | Watching RX\[0\] for | Transition when |
|---|---|---|---|
| **FindingAlignment** | `ANNOUNCE` | `ANNOUNCE` or `ACK` at index 0 | marker at index 0 |
| **AcknowledgingAlignment** | `ACK` | `ACK` or `0` | partner acknowledged |
| **WaitingForZeroes** | zeroes (TX disabled) | `0` | partner disabled TX |
| **Aligned** | — (TX disabled) | — (RX disabled) | done |

## Resulting Interface: Aligned Receive Buffer

Upon successful alignment, the hardware counters are synchronized: TX index `i` on one node maps to RX index `i` on the neighbor. No software-level offset tracking is needed. The `AlignedReceiveBuffer` type tracks the alignment state and exposes the underlying RX buffer for direct indexed access once aligned.

After alignment completes, both TX and RX are disabled. Higher-level code re-enables them as needed for normal operation.

## Communication Challenges
While the aligned ring buffer provides logical connectivity, the physical link remains unreliable due to the interaction between the read/write counters and asynchronous CPU access:

1.  **Continuous Hardware Operation:** The hardware continuously cycles through the ring buffers at the network link speed.
2.  **Asynchronous CPU Access:** The CPU operates asynchronously and often slower than the network link.

This leads to specific failure modes:
*   **Data Corruption (Pointer Overtaking):**
    *   *TX Side:* If the hardware's read pointer overtakes the CPU's write pointer during a write, a torn frame is sent.
    *   *RX Side:* If the hardware's write pointer overtakes the CPU's read pointer during a read, the message is corrupted.
*   **Data Loss:** If the CPU does not read from the RX ring buffer every iteration, the hardware will overwrite unread data.
*   **Data Duplication:** If the CPU does not write to the TX ring buffer every iteration, the hardware will resend old data.

Reliable communication requires a higher-level protocol to handle these errors. See the [Asynchronous Communication Protocol](asynchronous-communication.md) for a solution using the `smoltcp` library to implement a reliable TCP/IP layer over the aligned ring buffers.
