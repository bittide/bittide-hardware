# SPDX-FileCopyrightText: 2023 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set_property BOARD_PART_PIN sysclk_300_n [get_ports SYSCLK_300_n]
set_property BOARD_PART_PIN sysclk_300_p [get_ports SYSCLK_300_p]

# Color   | FPGA pin      | LVLSHFT       | Connection
# --------|---------------|---------------|---------
# Grey    | PMOD0_0       | IO1           | SYNC_OUT (connected on node 0 to sync_in of all nodes)
# Blue    | PMOD0_1       | IO2           | FINC
# Yellow  | PMOD0_2       | IO3           | MOSI/SDIO
# Red     | PMOD0_3       | IO4           | SCLK
# White   | PMOD0_4       | IO5           | SYNC_IN
# Purple  | PMOD0_5       | IO6           | FDEC
# Green   | PMOD0_6       | IO7           | CSB
# Orange  | PMOD0_7       | IO8           | MISO/SDO
# Black   | Not connected | Not connected |
# Brown   | PMOD_GND      | GND           | GND (SPI)

# PMOD1_0
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AK25} [get_ports {SYNC_OUT}]
# PMOD1_4
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AE26} [get_ports {SYNC_IN}]
