<!--
SPDX-FileCopyrightText: 2022 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# bittide-hardware
`bittide/bittide-hardware` contains a (very much work-in-progress) hardware
implementation of a Bittide system (also see [About Bittide](#about-bittide)). We
currently target Vivado FPGAs paired with SkyWorks clock adjustment boards.

# Getting started
This project uses a bunch of different languages and tool(chain)s. Nix is used
to manage this. To get a development shell [install nix](https://nixos.org/download.html)
and run the following command in the root of this repository:

```
nix-shell
```

The started shell contains everything needed to develop Bittide components.

TODO: Add overview of components

# About Bittide
Bittide is a novel distributed system architecture based on the idea
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

_Bittide is part of Google Resarch, and is not an official Google product._

# Licence

This code is shared under by Google LLC under the Apache 2.0
license. See the [LICENSE](LICENSES/Apache-2.0.txt) file for terms.

This project uses the [REUSE](https://reuse.software/) tool to check for
license compliance.
