<!--
SPDX-FileCopyrightText: 2023 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# VexRiscv core for Clash

This repository contains a [VexRiscv](https://github.com/SpinalHDL/VexRiscv) based CPU core and
bindings for use in [Clash](https://clash-lang.org/).

For now this repository only contains one
[example CPU configuration](clash-vexriscv/example-cpu/src/main/scala/example/ExampleCpu.scala),
which implements a 32 bit IMC RISC-V core.

This package includes simulation via [`verilator`](https://github.com/verilator/verilator) as
well as a black-box for Verilog synthesis.

The core interfaces with other components via [Wishbone](https://cdn.opencores.org/downloads/wbspec_b4.pdf)
interfaces, using [`clash-protocols`](https://github.com/clash-lang/clash-protocols) types.

## Building the project

For building the CPU, the following software needs to be installed and available in the `PATH`:

- a recent JDK installation
- SBT, the scala build tool
- a recent C compiler
- `verilator`, at least version 5.001 (development version at time of writing)
- `make` for building the verilated library and FFI code

### Notes for using the core

- VexRiscv has a "reset vector" for the instruction bus. This is the initial PC that gets fetched.
  This address only gets presented to the IBUS after at least one cycle of RST being asserted.

- The contents of memories need to be stored in little endian. This means that for example the
  contents of an ELF file need to be endian-swapped before being used as the contents of the
  instruction storage. This applies to all storages.
___

# Continuous Integration Setup

The Continuous Integration (CI) flow for this project is defined in the `ci.yml` file. The CI has the following goals:
* Verify that the project builds successfully.
* Prevent warnings and errors in the codebase.
* Ensure functional correctness by running tests.
* Ensure code quality by running linters and formatters.

CI uses a separate cabal project file that turns warnings into errors.

### Building and Pushing the Docker Image

To build the Docker image for this project, follow these steps:

1. Navigate to the docker directory:
  ```sh
  cd .github/docker
  ```

2. Run the `build-and-publish-docker-image.sh` script:
  ```sh
  ./build-and-publish-docker-image.sh
  ```

This script builds the Docker image using the specified Ubuntu and GHC versions. It also installs the necessary dependencies and tools required for the project.

### Authentication for Pushing to GitHub

If you have an authentication token, you can use `docker login <repo_url>` to log in using your docker username and token as password.

If you don't have an authentication token, you can request the maintainers to build and push the image. Please email [devops@qbaylogic.com](mailto:devops@qbaylogic.com) with the following information:
- Repository
- Commit hash
- Branch name

## Additional Support

For other problems or questions, please refer to one of the communication channels listed on [clash-lang.org](https://clash-lang.org).
