# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set_property BOARD_PART_PIN sysclk_300_n [get_ports SYSCLK_300_n]
set_property BOARD_PART_PIN sysclk_300_p [get_ports SYSCLK_300_p]

set_property BOARD_PART_PIN sma_mgt_refclk_n [get_ports SMA_MGT_REFCLK_C_n]
set_property BOARD_PART_PIN sma_mgt_refclk_p [get_ports SMA_MGT_REFCLK_C_p]

set_property BOARD_PART_PIN pcie_mgt_clkn [get_ports PCIE_CLK_Q0_n]
set_property BOARD_PART_PIN pcie_mgt_clkp [get_ports PCIE_CLK_Q0_p]

set_property BOARD_PART_PIN GPIO_LED_0_LS [get_ports spiDone]

set_clock_groups \
  -asynchronous \
  -group [get_clocks -include_generated_clocks {SYSCLK_300_p}] \
  -group [get_clocks -include_generated_clocks {SMA_MGT_REFCLK_C_p}] \
  -group [get_clocks -include_generated_clocks {PCIE_CLK_Q0_p}]

# Color   | FPGA pin      | LVLSHFT       | Connection
# --------|---------------|---------------|------------------
# Grey    | PMOD0_0       | IO1           | SYNC_OUT (legacy)
# Blue    | PMOD0_1       | IO2           | FINC
# Yellow  | PMOD0_2       | IO3           | MOSI/SDIO
# Red     | PMOD0_3       | IO4           | SCLK
# White   | PMOD0_4       | IO5           | SYNC_IN (legacy)
# Purple  | PMOD0_5       | IO6           | FDEC
# Green   | PMOD0_6       | IO7           | CSB
# Orange  | PMOD0_7       | IO8           | MISO/SDO
# Black   | Not connected | Not connected |
# Brown   | PMOD_GND      | GND           | GND (SPI)

# PMOD0_[0..7]
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AN21} [get_ports {FINC_0}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AH18} [get_ports {MOSI_0}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM19} [get_ports {SCLK_0}]
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AF25} [get_ports {FDEC_0}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AE21} [get_ports {CSB_0}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM17} [get_ports {MISO_0}]

# PMOD1_[0..7]
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM15} [get_ports {FINC_1}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AN18} [get_ports {MOSI_1}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AN17} [get_ports {SCLK_1}]
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM14} [get_ports {FDEC_1}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AP16} [get_ports {CSB_1}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AP15} [get_ports {MISO_1}]

# USER SMA GPIO_P
# set_property -dict {IOSTANDARD LVCMOS18 PACKAGE_PIN H27} [get_ports {SYNC_IN}]
# USER_SMA_GPIO_N (connected on node 0 to SYNC_IN of all nodes)
# set_property -dict {IOSTANDARD LVCMOS18 PACKAGE_PIN G27} [get_ports {SYNC_OUT}]
