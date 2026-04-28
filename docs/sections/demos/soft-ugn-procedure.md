<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Software UGN Discovery Procedure

The Software UGN (Uninterpretable Garbage Number) Discovery Procedure is a distributed mechanism used by Bittide nodes to determine network propagation delays to their neighbors without centralized coordination. This procedure runs on the Management Unit (MU) of each node.

## Overview

The procedure operates by exchanging timestamped messages between neighbors. By measuring the time difference between when a message is sent and when it is received (and acknowledged), nodes can calculate the round-trip time.

The procedure relies on the underlying ring buffers. Specifically:
*   The transmit and receive ring buffers are of equal size and operate as ring buffers with free-running hardware counters.
*   The alignment of the ring buffers (where data starting at address `0` in a transmit ring buffer arrives at address `n` in a receive ring buffer) is unknown at the start and is measured by the [Ring Buffer Alignment Procedure](../ringbuffer-alignment.md).

## Message Types

The procedure uses two main message types:

1.  **ANNOUNCE (`0xF`)**: A message sent to communicate an incoming UGN.
2.  **ACKNOWLEDGE (`0xF0`)**: A message sent to communicate an incoming and outgoing UGN.

## Event System

The firmware implements an event-driven architecture using a priority queue with 3 types of events:

### Event Types

*   **SEND**: Sends a UGN to a specific port at a specific time (that aligns with the start of the ringbuffer).
*   **RECEIVE**: Checks for an available UGN at a specific port (at the start of the ringbuffer).
*   **INVALIDATE**: Clears the data written by a SEND event to prevent it from being sent twice.

### Scheduling

Events are scheduled according to the send and receive periods:
*   Initially, one **SEND** event is scheduled far enough into the future for each port, evenly spaced over the send period.
*   Initially, one **RECEIVE** event is scheduled far enough into the future for each port, evenly spaced over the receive period.
*   Every **SEND** event schedules an **INVALIDATE** event and reschedules a new **SEND** event for this port.
*   Every **RECEIVE** event reschedules a new **RECEIVE** event.

### Preconditions

*   `memory_size == rx_memory_size == tx_memory_size` to preserve the ringbuffer alignment.
*   Since we are limited by compute resources, we can only process one port per event. For this reason `receive_period / memory_size` and `send_period / memory_size` need to be coprime to make sure we cover all possible overlaps between ports. We divide by `memory_size` because we know exactly where to write and read messages in our memories based on the ringbuffer alignment.

## Procedure Operation

1.  **Initialization**:
    *   The MU aligns the ring buffers  leveraging the [Ring Buffer Alignment Procedure](../ringbuffer-alignment.md).
    *   Initial `SEND` and `RECEIVE` events are scheduled for all ports.

2.  **Discovery Loop**:
    *   **Sending**: When a `SEND` event triggers, the node writes a message to the transmit ring buffer.
    *   **Receiving**: When a `RECEIVE` event triggers, the node inspects the receive ring buffer.
        *   If an `ANNOUNCE` is received: The node records the arrival time and schedules an `ACKNOWLEDGE` message to be sent back.
        *   If an `ACKNOWLEDGE` is received: The node uses the comprehensive timing data (send time, receive time, ack transmit time, ack arrival time) to calculate the UGN.

3.  **UGN Calculation**:
    *   By exchanging messages that contain transmission timestamps, nodes can collect and communicate the necessary timing information to compute the UGN.
    *   The UGN represents the propagation delay between nodes.
