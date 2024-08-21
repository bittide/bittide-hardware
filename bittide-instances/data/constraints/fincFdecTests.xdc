# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
#
# NOTE: This configuration is only valid for the leftmost FPGA in the demo rack.
#
# Color   | FPGA pin      | LVLShift      | Connection
# --------|---------------|---------------|---------
# Grey    | PMOD0_0       | IO1           | SWDIO
# Blue    | PMOD0_1       | IO2           | FINC
# Yellow  | PMOD0_2       | IO3           | MOSI/SDIO
# Red     | PMOD0_3       | IO4           | SCLK
# White   | PMOD0_4       | IO5           | SWCLK
# Purple  | PMOD0_5       | IO6           | FDEC
# Green   | PMOD0_6       | IO7           | CSB
# Orange  | PMOD0_7       | IO8           | MISO/SDO
# Black   | Not connected | Not connected | GND (SWD)
# Brown   | PMOD_GND      | GND           | GND (SPI)
#
# The data wire of the external reset button is connected to PMOD1_3.

set_property BOARD_PART_PIN sysclk_125_p [get_ports {CLK_125MHZ_p}]
set_property BOARD_PART_PIN sysclk_125_n [get_ports {CLK_125MHZ_n}]
set_property BOARD_PART_PIN sma_mgt_refclk_n [get_ports {SMA_MGT_REFCLK_C_n}]
set_property BOARD_PART_PIN sma_mgt_refclk_p [get_ports {SMA_MGT_REFCLK_C_p}]

# Vivado marks all clocks as related by default. Our external clocks are not
# though, which means that we need to explicitly mark them as unrelated (or
# "asynchronous").
set_clock_groups \
    -asynchronous \
    -group [get_clocks -include_generated_clocks {CLK_125MHZ_p}] \
    -group [get_clocks -include_generated_clocks {SMA_MGT_REFCLK_C_p}]

# GPIO_LED_0_LS
set_property BOARD_PART_PIN GPIO_LED_0_LS [get_ports {done}]
# GPIO_LED_1_LS
set_property BOARD_PART_PIN GPIO_LED_1_LS [get_ports {success}]

# PMOD0_[0..7]
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AK25} [get_ports {SWDIO}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AN21} [get_ports {FINC}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AH18} [get_ports {MOSI}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM19} [get_ports {SCLK}]
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AE26} [get_ports {SWCLK}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AF25} [get_ports {FDEC}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AE21} [get_ports {CSB}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM17} [get_ports {MISO}]
