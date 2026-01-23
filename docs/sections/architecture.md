<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Architecture Overview
Bittide as a communication link is inherently scalable. Bittide nodes can be connected point-to-point between nodes, boxes, and racks without loss of per-cycle accuracy. The only parameters to a bittide network are link topology and link latency. Notably, link latency DOES NOT affect per-cycle accuracy, but it does affect inter-node latency.

Here's an example bittide network

{{#drawio path="diagrams/bittide-network-simple.drawio" page=0}}

All computational workloads are done by the Processing Elements. The bittide network is transparent to the Processing Elements. The Processing Elements operate on the logic layer of the bittide network, as seen below.

{{#drawio path="diagrams/bittide-logical-network-simple-2.drawio" page=0}}

Any Processing Element can be added to a bittide network, with the requirements that
1) the Processing Element is able to run on the bittide clock domain, or some PLL multiple
2) ...that's about it, actually

The rest of this chapter is devoted to how bittide achieves logical synchrony on a per-node basis. For scheduling a computation over the network and failure recovery, see a further chapter.

## Bittide Node bringup sequence

For actually using a bittide network, these details are largely irrelevant. Still, it's useful to have a general understanding of how bittide works in practice.

### Step 0: Booting the bittide clock
{{#drawio path="diagrams/bittide-boot-diagram.drawio" page=0}}

Contrary to what one might expect, there are actually TWO clocks in a bittide node: an adjustable clock (called the bittide clock) and a static clock (called the boot clock). For most of the bittide boot and all of the Processing Element functioning, the bittide clock is used. However, since the adjustable clock is actually a somewhat complex piece of silicon, it itself needs to be set up. To do this, we have a Clock Setup CPU running on the boot clock, which has two jobs:
 1) set up bittide clock by configuring clock registers and setting initial target frequency
 2) initiate handshake of SerDes between bittide nodes, such that every node establishes a link with other nodes (by sending comma values)

Once the bringup sequence has moved to the next step, the Boot CPU and boot clock are no longer used.

### Step 1: Achieving clock syntony(ish)
{{#drawio path="diagrams/bittide-cc-diagram.drawio" page=0}}

At this point in the bringup sequence, all bittide clocks have been coordinated with reference to local, inperfect, static clocks. The goal of this step is to align every bittide clock on the network, creating syntony.

Each bittide node starts sending a PBRS (Pusedo-random byte stream) over every link. The content of the data is not important, merely that data is being sent. The SerDes of every link locks onto the remote clock frequency embedded in the link in order to deserialize the data.

In typical networks, once the data is deserialized, this clock information is thrown away. In bittide, the remote clock frequency is stored locally in a register that counts every tick of the remote clock. Bittide also stores a counter for the local bittide clock. Collectively, we call these registers the Domain Difference Counter (or DDC). The Clock Control CPU reads in these counters and adjusts (FINC or FDEC) the local bittide clock depending on whether it detects the local clock is too fast or too slow.

Over time, we can show bittide clocks settle to a common network frequency (up to a small delta). Once this state is achieved, these clock control circuits stay active to maintain a common frequency despite changes in heat, etc.

### Step 2: Actually achieving clock syntony and determining logical latencies
{{#drawio path="diagrams/bittide-mu-diagram.drawio" page=0}}
So far, we have achieved a common bittide network frequency. The dutiful engineer will have noticed we only guaranteed that frequency with a small delta. This seems counter to bittide's promise of cycle-level accuracy. And if we left it there, you would be right. Cycle-ish-level accuracy doesn't get us much.

To absorb small wobbles between the remote bittide clocks and local bittide clock, a small Elastic Buffer (EB) is inserted. Notably, this buffer can be made much smaller than the corresponding input buffer used by most networking interfaces, because bittide guarantees a small delta of clock drift. The bittide system tries to keep the elastic buffer filled halfway at all times, so that it can absorb a wobble of `(buffer size)/2` clock cycles.

We can now say the bittide system has achieved cycle-level latency. But we have a new problem: no bittide node knows what that latency actually is between itself and its neighbors (nor even who its neighbors are).

To discover the latencies between nodes, the Management Unit (MU) sends the local clock cycle to each neighbor node. The neighbor node records the clock cycle it received the message at and the clock cycle it sent the response at. The local node then records the local clock cycle it received the response. Given these four numbers, the local node can determine the latency between each of its neighboring nodes.

### Step 3: Handover
{{#drawio path="diagrams/bittide-pe-diagram.drawio" page=0}}
Once logical latency has been established, the bittide network guarantees these latencies, and control is handed over to the processing element.

## Glossary for bittide-specific terms
**(Aligned) Ring buffer**

**Clock Control CPU (CC)** - The CPU that reads in the domain differences between each foreign node and the local node and sends a signal to the clock to speed up or slow down based on some function.

**Clock Setup CPU (CS)** - The CPU that boots the adjustable (bittide) clock and configures its registers via SPI. It also brings SerDes and Handshake out of reset, which negotiates the 8b10b link with every other node.

**Elastic buffer (EB)** -

**Handshake** (needs better name) - A step function that sends out PBRS until a link is negotiated. Then simply passes data through.

**Logic layer** -

**Management Unit (MU)** - The CPU that performs elastic buffer centering and UGN capturing.

**PBRS** -

**Physical layer** -

**Static clock (SCLK)** - A reference clock that is not adjustable. Its only purpose is to provide a clock for the Clock Setup CPU. Once the Clock Setup CPU is finished, the static clock is not needed.

**Unreadable garbage number (UGN)** - A 64 bit number, where the upper 32 bits are the remote clock cycle and the lower 32 bits are the local clock cycle. Used to determine the logical latency between two nodes.

### Glossary for non-bittide technical terms
**Comma symbol** - An alignment symbol in 8b10b link negotiation

**PCS** - Physical coding sublayer
