# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# FPGA pin  | Color   | LVLSHFT       | Connection
# ----------| --------|---------------|-------------
# PMOD0_2   | Yellow  | IO3           | MOSI
# PMOD0_3   | Red     | IO4           | SCLK
# PMOD0_6   | Green   | IO7           | CSB
# PMOD0_7   | Orange  | IO8           | MISO
# PMOD_GND  | Brown   | GND           | GND (SPI)

# PMOD0
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AH18} [get_ports {MOSI}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM19} [get_ports {SCLK}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AE21} [get_ports {CSB}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM17} [get_ports {MISO}]
