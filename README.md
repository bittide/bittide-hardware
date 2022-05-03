<!--
SPDX-FileCopyrightText: 2022 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# bittide

bittide is a novel distributed system architecture based on the idea
that synchronous, ahead-of-time scheduling using a logical global
clock provides a set of properties that simplify the design and
utilization of large systems.

With bittide we aim to build a system with guaranteed performance and
robustness and address today's distributed systems inefficiencies and
unpredictable long tail latencies. This ranges from simple things like
video conferencing without glitches to virtual reality sports
competitions with precisely equivalent system response times for all
competitors. The properties underlying bittide support the
orchestration and composition of microservices into a system without
queueing delays.

## Components

This repository consists of a collection of independent components
that model various aspects of the bittide system. These include:

- control system simulator(s): models of the bittide synchronization
  control system
- hardware architecture models: RTL level modeling of bittide components
- system architecture models: functional simulator of the bittide
  system
- bittide sample applications

## Documentation

To be published.

## Contributing

Contributions to the project are welcome. We ask that you please read
our [code of conduct](docs/code-of-conduct.md) and
[guidelines](docs/contributing.md) before deciding how to contribute.

## Licence

This code is shared under by Google LLC under the Apache 2.0
license. See the [LICENSE](LICENSES/Apache-2.0.txt) file for terms.

This project uses the [REUSE](https://reuse.software/) tool to check for
license compliance.

**Note**: bittide is not an official Google product.
