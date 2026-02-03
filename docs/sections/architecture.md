<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Architecture Overview
Bittide as a communication link is inherently scalable. Bittide nodes can be connected point-to-point between nodes, boxes, and racks without loss of per-cycle accuracy. The only parameters to a bittide network are link topology and link latency. Notably, link latency DOES NOT affect per-cycle accuracy, but it does affect inter-node latency.

Here's an example bittide network

{{#drawio path="diagrams/bittide-network-simple.drawio" page=0}}

Any Processing Element can be added to a bittide network, with the requirements that
1) the Processing Element is able to run on the bittide clock domain, or some PLL multiple
2) the Processing Element dedicates a small amount of die or FPGA space for the bittide interface
2) ...that's about it, actually

The rest of this chapter is devoted to how bittide achieves logical synchrony on a per-node basis. For scheduling a computation over the network and failure recovery, see a further chapter.

## Bittide Node bringup sequence

For actually using a bittide network, these details are largely irrelevant. Still, it's useful to have a general understanding of how bittide works in practice.

### Step 1: Booting the bittide clock
{{#drawio path="diagrams/bittide-boot-diagram.drawio" page=0}}

Contrary to what one might expect, there are actually TWO clocks in a bittide node: an adjustable clock (called the bittide clock) and a static clock (called the boot clock). For most of the bittide boot and all of the Processing Element functioning, the bittide clock is used. However, since the adjustable clock is actually a somewhat complex piece of silicon, it itself needs to be set up. To do this, we have a Clock Setup CPU running on the boot clock, which has two jobs:
 1) set up bittide clock by configuring clock registers and setting initial target frequency
 2) initiate handshake of SerDes between bittide nodes, such that every node establishes a link with other nodes (by sending comma values)

Once the bringup sequence has moved to the next step, the Boot CPU and boot clock are no longer used.

### Step 2: Achieving clock syntony(ish)
{{#drawio path="diagrams/bittide-cc-diagram.drawio" page=0}}

At this point in the bringup sequence, all bittide clocks have been coordinated with reference to local, inperfect, static clocks. The goal of this step is to align every bittide clock on the network, creating syntony.

Each bittide node starts sending a pseudorandom binary sequence over every link. The content of the data is not important, merely that data is being sent. The SerDes of every link locks onto the remote clock frequency embedded in the link in order to deserialize the data.

In typical networks, once the data is deserialized and converted to the local frequency, this remote clock information is discarded. In bittide, the remote clock frequency is stored locally in a register that counts every tick of the remote clock. Bittide also stores a counter for the local bittide clock. Collectively, we call these registers the Domain Difference Counter (or DDC). The Clock Control CPU reads in these counters and adjusts (FINC or FDEC) the local bittide clock depending on whether it detects the local clock is too fast or too slow.

Over time, we can show bittide clocks settle to a common network frequency (up to a small delta). Once this state is achieved, these clock control circuits stay active to maintain a common frequency despite changes in heat, etc.

### Step 3: Actually achieving clock syntony
{{#drawio path="diagrams/bittide-mu-diagram.drawio" page=0}}

So far, we have achieved a common bittide network frequency. The dutiful engineer will have noticed we only guaranteed that frequency with a small delta. This seems counter to bittide's promise of cycle-level accuracy. And if we left it there, you would be right. Cycle-ish-level accuracy doesn't get us much.

To absorb small wobbles between the remote bittide clocks and local bittide clock, a small Elastic Buffer (EB) is inserted. Notably, this buffer can be made much smaller than the corresponding input buffer used by most networking interfaces, because bittide guarantees a small delta of clock drift. The bittide system tries to keep the elastic buffer filled halfway at all times, so that it can absorb a wobble of `(buffer size)/2` clock cycles.

We can now say the bittide system has achieved cycle-level latency. But we have a new problem: no bittide node knows what that latency actually is between itself and its neighbors (nor even who its neighbors are).

### Step 4: Determining logical latencies
Each node will send out a message of the current clock cycle. Notably, this message sending does not need to be coordinated in any way. Each neighbor will receive that clock cycle and record the local clock cycle it was received on. This number defines the one way domain mapping, also called logical latency.

How this "recording" happens in practice is an open technical discussion on bittide. We present the two main solutions below, both of which have been tested on the bittide hardware.

#### Option 1: Hard UGN capture
Hard UGN capture has two parts:
1) For each node, the first Bittide Word it writes to the network is the local clock counter value
2) Each node also has a hardware component, called the UGN capture, that sits between the EB and the RB on the receiving end. Its sole job is to wait for the first valid piece of bittide data and save it with the local clock cycle at that time. It lets all data thereafter through to the RB

These two pieces, together, ensure the UGNs are captured between each node. The Management Unit (MU) can then read the UGN values and use them. Below we have an example of a UGN capture in practice.

**Example Hard UGN capture**
<div class="svg-slideshow">
  <img id="frame" src="../diagrams/ugnCapture/frame00.svg" width="600">

  <div>
    <button onclick="prev()">◀</button>
    <button onclick="next()">▶</button>
  </div>
</div>

<script>
  const frames = [
    "../diagrams/ugnCapture/frame00.svg",
    "../diagrams/ugnCapture/frame01.svg",
    "../diagrams/ugnCapture/frame02.svg",
    "../diagrams/ugnCapture/frame03.svg",
    "../diagrams/ugnCapture/frame04.svg",
    "../diagrams/ugnCapture/frame05.svg",
    "../diagrams/ugnCapture/frame06.svg",
    "../diagrams/ugnCapture/frame07.svg",
    "../diagrams/ugnCapture/frame08.svg",
    "../diagrams/ugnCapture/frame09.svg",
    "../diagrams/ugnCapture/frame10.svg",
  ];
  let i = 0;

  function show(k) {
    i = (k + frames.length) % frames.length;
    document.getElementById("frame").src = frames[i];
  }

  function next() { show(i + 1); }
  function prev() { show(i - 1); }
</script>

#### Option 2: Soft UGN capture
Soft UGN capture is done by using the Management Unit (MU) CPU to read the RingBuffer (RB). The benefit of this approach is we can re-use an existing component instead of creating a new one, saving space on the hardware.

However, the CPU approach comes with a major limitation - unlike the UGN Capture component, the MU cannot inspect every bittide word the same cycle it comes in.[^1] Therefore, we need to do two things:
1) let the MU know which element in the Rx RB corresponds to the start of the Tx RB.
2) have the neighbor node send the clock cycle sometime at the start of the Tx RB.

[^1]: This difference occurs because the RingBuffer (RB) only supports accessing one address per cycle. The UGN Capture sits before the RingBuffer (RB) in the data pipeline, while the MU CPU sits behind the RB. So UGN capture can inspect every new word, while the MU needs to know which RB element to inspect. If the MU were to scan the entire RB, it would find the right element, but it would then not know on which clock cycle the element was put into the RB.

This way, the MU does not need to inspect every element in the RB for the clock cycle, it just needs to inspect the one entry it knows the clock cycle will eventually be in. For more detail, see the [Ringbuffer alignment](ringbuffer-alignment.html) section.

Once the relationship has been mapped, the sending node can send a "UGN event" (5 bittide words), which will be read by the receiving MU.

### Step 5: Handover to the processing element
{{#drawio path="diagrams/bittide-pe-diagram.drawio" page=0}}

Once logical latency has been established, the bittide network guarantees these latencies until reboot. Control is handed over to the Processing Element. The Processing Element can operate as normal, without any knowledge of the bittide network. However, it now has guaranteed latency with all other nodes, allowing it to schedule and execute computations.

## Glossary for bittide-specific terms

**Bittide word** - The smallest unit of the bittide network in a clock cycle. The word size of bittide is 64 bits.

**Clock Control CPU (CC)** - The CPU that reads in the domain differences between each neighbor node and the local node and sends a signal to the clock to speed up or slow down based on some function.

**Clock Setup CPU (CS)** - The CPU that boots the adjustable (bittide) clock and configures its registers via SPI. It also brings SerDes and Handshake out of reset, which negotiates the 8b10b link with every other node.

**Domain difference counter (DDC)** -

**Domain mapping** - Hidde previously described this as the UGN, The domain mapping is a the actually observation we make to obtain the logical latency. It consists of transmit timestamp expressed in clock cycles of your link partner. When this timestamp arrives in your own domain, it is stored together with the receive timestamp. This pair of counters is the domain mapping because it maps the transmit cycle of your link partner to your receive cycle. Both of these are natural numbers (represented as u64)

**Elastic buffer (EB)** -

**Processing element (PE)** - The computation element that is connected to the bittide network via bittide. The computational elements can be anything (hence the general term), but is often considered to be an ASIC or similar.

**Handshake** (needs better name) - A step function that sends out PRBS until a link is negotiated. Then simply passes data through.

**Logical latency** - Integer that is derived from the domain mapping, we can use this to predict the exact clock cycle when a message will arrive at our link partner.

**Logic layer** -

**Management Unit (MU)** - The CPU that performs elastic buffer centering and UGN capturing.

**Pseudorandom binary sequence (PRBS)** -

**Physical layer** -

**(Aligned) Ring buffer**

**Roundtrip time** - Natural number that represents the number of clock cycles it takes for a message to make a roundtrip from node A to node B and back. It is the sum of the logical latencies l a -> b and l b -> a.

**Static clock (SCLK)** - A reference clock that is not adjustable. Its only purpose is to provide a clock for the Clock Setup CPU. Once the Clock Setup CPU is finished, the static clock is no longer needed.

**Uninterpretable garbage number (UGN)** - A 64 bit number, where the upper 32 bits are the remote clock cycle and the lower 32 bits are the local clock cycle. Used to determine the logical latency between two nodes.

### Glossary for non-bittide technical terms
**Comma symbol** - An alignment symbol in 8b10b link negotiation
