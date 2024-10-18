# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0


# CLK_125MHZ
set_property BOARD_PART_PIN sysclk_125_p [get_ports {CLK_125MHZ_p}]
set_property BOARD_PART_PIN sysclk_125_n [get_ports {CLK_125MHZ_n}]

set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AL14} [get_ports {USB_UART_RXD}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM14} [get_ports {USB_UART_TXD}]

set_clock_groups \
  -asynchronous \
  -group [get_clocks -include_generated_clocks {CLK_125MHZ_p}]
