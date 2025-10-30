# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# FPGA pin  | Color   | LVLSHFT       | Connection
# ----------| --------|---------------|-------------
# PMOD0_1   | Blue    | IO2           | FINC
# PMOD0_5   | Purple  | IO6           | FDEC

# PMOD0
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AN21} [get_ports {FINC}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AF25} [get_ports {FDEC}]
