<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Hardware-in-the-Loop (HITL) Platform

This chapter describes the specific hardware setup used to realize a bittide system in our lab environment. The HITL platform is designed to implement the principles of bittide using real hardware components for experimentation, development, and validation for topologies up to 8 nodes.

## Platform Overview
**Requires diagram**

The HITL platform consists of the following main components:

- **Host Computer**: Executes experiments on the bittide system.
- **FPGA Boards**: Implement clock control, bittide routing, and compute fabric.
- **Clock Adjustment Boards**: Provide single controllable clock source for an FPGA.
- **High-Speed Interconnects**: Serial links (such as SFP+ or QSFP) connect the FPGAs for low-latency, high-bandwidth communication.
- **JTAG/UART dongles**: Provide JTAG and UART interface for each FPGA to the host computer.
- **Ethernet connections**: Each FPGA has an ethernet connection from a dedicated RJ45 port to the host computer.
- **SYNC_IN / SYNC_OUT**: Ring connection through all FPGAs to get a sense of global time. This is used for starting / stopping experiments and mapping measurement data from all FPGAs to a single time line. This is **not** part of the bittide architecture itself.
