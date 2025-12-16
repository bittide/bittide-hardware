<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Example bittide System

This document describes a concrete example of a bittide design that can be programmed on the FPGAs of the HITL platform. It showcases the internal architecture and configuration of the system.

## Architecture
**Requires diagram**

### Domain related
Components:
- Transceivers
- Domain difference counters
- Clock control (CC)
- SPI programmer for clock generator
- Elastic buffers
- UGN capture

### Node related
Components:
- Switch (Crossbar + calendar)
- Management unit (MNU)
- 1x General purpose processing element (GPPE)

### Management unit
Components:
- RISCV core
- Scatter unit
- Gather unit
- UART (For debugging)

The management unit has access to, and is responsible for all calendars in the node.

Calendars:
- Switch calendar
- Management unit scatter calendar
- Management unit gather calendar
- GPPE scatter unit
- GPPE gather unit

## Debugging related
- UART arbiter
- JTAG interconnect
- Integrated logic analyzers
- SYNC_IN / SYNC_OUT

# Example System

Below is a diagram generated from the first page of `bittide_concepts.drawio`:

TODO: Insert diagram
<!-- ![bittide System](diagrams/bittide_concepts_0.svg) -->
