<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Introduction

Welcome to the bittide documentation book! Bittide is a inter-chip communication link that allows cycle-level deterministic communication between chips with no in-band overhead.

The benefits include:
 - eliminating tail latency
 - allowing compilers to statically schedule workloads across physical chips
 - scaling to an arbitrary number of nodes

This book serves as an introduction to bittide and its concepts. Our goal is to provide the necessary information to develop and deploy experiments on bittide-based systems.

## Key terms
**Clock drift** - The gradual deviation of two physical clocks from each other

**Logical synchrony** - Distributed computing to be coordinated as tightly as in synchronous systems without the distribution of a global clock or any reference to universal time

**Latency-deterministic hardware** - Computer hardware whose latencies and computation graph can be known ahead of time, independent of the data. Examples of non latency-deterministic hardware are branch predictors and cache prefetching, since the computation time cannot be known at compile time. Non latency-deterministic hardware can still use bittide, but loses most of the benefit of bittide's cycle-accurate communication.

## The problem with inter-chip communication
Modern computation workloads are growing larger. To accomodate this growth, workloads are being split between multiple physical processors. The real-world scaling of these workloads does not match the theoretically achievable scaling.

This disconnect is caused by non-determinism in the underlying physical clocks of each chip causing **clock drift**. Due to physical phenomena including manufacturing defects, heat, or vibrations, these clocks do not run perfectly in sync. Most chips have an input buffer to compensate, but this buffer can be overrun. To handle this, chips must be able to apply backpressure, or a signal to wait before sending more data. The problem also scales exponentially with the number of chips in a system, effectively limiting the scaling of systems.

As you scale up the number of connected processors, the problem compounds. More chips mean more independent clocks, more buffering, and more backpressure, which together place practical limits on system scaling.

## Bittide's Core Idea
Bittide uses a decentralized hardware algorithm to prevent clock drift. The algorithm is based on the following observations:
 1) The clock drift between any two nodes can be known as the difference between the number of data frames sent out by a node and the number of data frames received by a node
 2) If you adjust the clock frequency based on this signal, you can keep two nodes in **logical synchrony**
 3) If this algorithm is carefully scaled up to an arbitrary number of nodes[^1], an entire system can be held in logical synchrony

[^1]: See [this paper](https://arxiv.org/abs/2504.07044)

If we remove all non-determinism from inter-chip communication, then we get the benefit of scaling logic as if we were using a bigger chip, but the physical scaling of adding more chips to the cluster.

For an in-depth description of the bittide system, please see [this paper](https://arxiv.org/pdf/2503.05033).

## Benefits of bittide
By removing non-determinism from inter-chip communication, bittide allows distributed systems to scale as if they were a single, larger chip. From the perspective of software and compilers, communication becomes a deterministic operation with known cost, enabling more aggressive scheduling, static placement of data, and compile-time reasoning about performance.

In effect, bittide shifts complexity away from runtime mechanisms and into design-time guarantees. This tradeoff enables large-scale systems that preserve the simplicity and predictability traditionally associated with small, tightly integrated hardware designs.

## Who might be interested in bittide
The bittide system represents a novel approach to inter-chip communication that guarantees determinism. Certain workloads and compute architectures are better suited to take advantage of this property than others.

We believe those engineers are:
 - Hardware engineers who work on **latency-deterministic hardware**
 - Compiler engineers who are interested in optimizing the mapping of computation onto distributed hardware
 - Software engineers who need a fixed latency output for their workload

## The requirements of a bittide system
Any engineering implementation requires tradeoffs. For bittide,
 - a small amount of die space to handle input buffers and clock control
 - an adjustable clock source
 - some startup time on power up to synchronize clocks before starting workload
 - a compiler that is able to take advantage of a bittide-based system

## Timeline of project
The bittide project is Apache 2.0 licensed and is being developed by [QBayLogic](https://qbaylogic.com/) and [Google DeepMind](https://deepmind.google/).

### Timeline

**2025** - Physical proof-of-concept demo completed

**2016** - Project start
