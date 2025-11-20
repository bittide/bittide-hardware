<!--
SPDX-FileCopyrightText: 2024 Google LLC

SPDX-License-Identifier: Apache-2.0
-->
**Warning**: We swapped 2 PCIe extension cards offering 4x1G Ethernet connections for
a single extension card offering 2x10G. One of those connections is connected to a
switch offering access to the larger network (build/cache) servers. The other one is
connected to a switch that is in turn connected to all FPGAs. Since this change, the
VexRiscv TCP test fails for unknown reasons. We've done a few hours of debugging, but
that didn't yield the root cause. We decided to shelve it for now, because we don't
critically rely on TCP/Ethernet working at the moment. That also means that the
instructions in this README might be faulty. Beware!

This folder contains all information and scripts to bootstrap the HITL setup. Setting
up should be done manually by running `setup.sh`, CI should only check if the setup is
correct. The setup script sets up a DHCP server.

In addition to that, you should configure the interface such that the host is located
at 10.0.0.1. In the GUI that comes with Ubuntu you should set "IPv4 Method" to "Manual"
and enter address "10.0.0.1" with a netmask of "255.255.255.0". You can disable IPv6
entirely.
