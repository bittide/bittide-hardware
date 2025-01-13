# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# PMOD0_[0..7]
# Note that AH18 is a global clock capable pin
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AH18} [get_ports {JTAG_TCK}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM19} [get_ports {JTAG_TDI}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AE26} [get_ports {JTAG_RST}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AF25} [get_ports {JTAG_TMS}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AE21} [get_ports {JTAG_TDO}]
