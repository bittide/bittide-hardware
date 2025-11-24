<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# UGN Discovery Protocol

The UGN (Uninterpretable Garbage Number) discovery protocol enables nodes in a Bittide network to autonomously discover their network topology and measure link delays without centralized coordination.

## The Initialization Paradox

At startup, we face a paradox:
1.  To read a message from a neighbor, we need to know which slot in the scatter buffer to check (offset depends on link latency).
2.  To know the link latency, we need to successfully read a message.
3.  We cannot scan the entire buffer every cycle due to compute constraints; we only check **Offset 0**.

## Probabilistic Discovery

To solve this, we use a **staggered send** strategy.

*   **Receiver:** Checks Offset 0 every Metacycle.
*   **Sender:** Increments the position of the next message by 1 every metacycle.

Because `n` and `n + 1` are coprime, eventually a message is guaranteed to land at Offset 0, where the receiver is waiting.

## Protocol Phases

### Phase 1: Incoming UGN Discovery
We do not know the incoming or outgoing UGN. We perform the **staggered send** strategy and listen at **Address 0**.

At some point, we read a message at Address 0. This message allows us to calculate the **Incoming UGN**. Once we have the Incoming UGN, we transition to Phase 2.

{{#drawio path="diagrams/ugnDiscoveryTimeline.drawio" page=0}}

### Phase 2: Outgoing UGN Discovery
We now know our Incoming UGN. We include this Incoming UGN in our outgoing messages and continue to follow the **staggered send** strategy.

Our neighbor is doing the same. At some point, we read a message at Address 0 that contains the **Outgoing UGN** (which is the neighbor's Incoming UGN). Once we receive this, we know the link latency in both directions and transition to Phase 3.

{{#drawio path="diagrams/ugnDiscoveryTimeline.drawio" page=1}}

### Phase 3: Targeted Send (Connection Established)
We have received the Outgoing UGN. We can now calculate exactly when to send a message so that it arrives at **Address 0** of our neighbor.

Because our neighbor always reads at Address 0, we know this targeted message will be received. We stop scheduling send events for this port. Once we have passed Phase 3 for all ports, we can also stop scheduling receive events.

{{#drawio path="diagrams/ugnDiscoveryTimeline.drawio" page=2}}
