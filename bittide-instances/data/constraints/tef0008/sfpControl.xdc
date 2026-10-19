# SPDX-FileCopyrightText: 2026 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# SFP control signals of the Trenz TEF0008 FMC card, on the FMC HPC connector.
# See https://wiki.trenz-electronic.de/display/PD/TEF0008+MAX10.
#
# FPGA pin | FMC pin | Signal (inverted by the card's MAX10)
# A13      | LA03_P  | TX disable of all SFPs
# A12      | LA03_N  | RS0 (RX rate select) of all SFPs
# L12      | LA04_P  | RS1 (TX rate select) of all SFPs
set_property -dict {IOSTANDARD LVCMOS18 PACKAGE_PIN A13} [get_ports {FMC_HPC_LA03_P}]
set_property -dict {IOSTANDARD LVCMOS18 PACKAGE_PIN A12} [get_ports {FMC_HPC_LA03_N}]
set_property -dict {IOSTANDARD LVCMOS18 PACKAGE_PIN L12} [get_ports {FMC_HPC_LA04_P}]
