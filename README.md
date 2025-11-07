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
to manage this. To get a development shell [install nix](https://nixos.org/download.html).
If this is your first time setting up Nix or when you haven't enabled Nix Flakes
yet, you need to add the following to `.config/nix/nix.conf`:

```
experimental-features = nix-command flakes
```

If this is your first time using Nix, you can do it programmatically by executing:

```bash
mkdir -p ~/.config/nix
echo "" >> ~/.config/nix/nix.conf
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

Optionally, you can install `cachix` to use caches built by our CI infrastructure:

```
nix profile install nixpkgs#cachix
cachix use bittide-hardware
```

Run the following command in the root of this repository to get a development shell.
You will need about 20 GB of free disk space.

```
nix develop
```

The started shell contains everything needed to develop Bittide components.

TODO: Add overview of components

# Development
We follow a standard GitHub development flow. Our development branch is called `main`. Free free to open a PR. If you're not sure what to do, open a [discussion](https://github.com/bittide/bittide-hardware/discussions) thread.

## Tips & Tricks

  * The full (expensive) test suite only runs nightly on `main`. If you want to run the full test suite on a PR, add `[force_expensive_checks]` to your commit message.
  * While debugging, we often only want one bittide instance to be tested with our hardware-in-the-loop infrastructure. With the increasing number of bittide instances with are synthesized and tested, these CI runs take a long time. You can add a file `.github/synthesis/debug.json`, with only the instances you want CI to synthesize/test. The CI run will always fail on the 'all' job when this file exists to prevent a premature merge.
  * You can run `format` in the Nix shell to format all Cabal, Haskell, and Rust files.

## Pre-commit hooks
The nix development shell is configured with pre-commit hooks to ensure code quality and consistency. These hooks run automatically before each commit when you are in the shell. Nix generates a `.pre-commit-config.yaml` file in the root of the repository to configure these hooks. Do not manually edit this file.

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

_Bittide is part of Google Research, and is not an official Google product._

# License

This code is shared under by Google LLC under the Apache 2.0
license. See the [LICENSE](LICENSES/Apache-2.0.txt) file for terms.

This project uses the [REUSE](https://reuse.software/) tool to check for
license compliance.
