# SPDX-FileCopyrightText: 2023 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set_property BOARD_PART_PIN sysclk_300_n [get_ports SYSCLK_300_n]
set_property BOARD_PART_PIN sysclk_300_p [get_ports SYSCLK_300_p]
set_property BOARD_PART_PIN sma_mgt_refclk_n [get_ports SMA_MGT_REFCLK_C_n]
set_property BOARD_PART_PIN sma_mgt_refclk_p [get_ports SMA_MGT_REFCLK_C_p]

set_property BOARD_PART_PIN GPIO_LED_0_LS [get_ports spiDone]

set_clock_groups \
  -asynchronous \
  -group [get_clocks -include_generated_clocks {SYSCLK_300_p}] \
  -group [get_clocks -include_generated_clocks {SMA_MGT_REFCLK_C_p}]

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

# PMOD1_[0..7]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AK25} [get_ports {SYNC_OUT}]
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AN21} [get_ports {FINC}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AH18} [get_ports {MOSI}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM19} [get_ports {SCLK}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AE26} [get_ports {SYNC_IN}]
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AF25} [get_ports {FDEC}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AE21} [get_ports {CSB}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM17} [get_ports {MISO}]

# PMOD0_3
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM19} [get_ports {shared_reset_btn}]
