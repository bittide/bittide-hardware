<!--
SPDX-FileCopyrightText: 2022 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Support code for `contranomy` simulation

This package contains supporting code for writing and debugging `contranomy`
simulations.

At the moment, this code consists of the following components:

- An [ELF loader](src/ContranomySim/ReadElf.hs) to load a program into
  intruction/data memory
- A [print-debugging module](src/ContranomySim/Print.hs) implementing a
  character device at a given address. Writes to the special character-device
  address will be outputted to `stdout`.

Together those components are intended to be used for integration tests.
