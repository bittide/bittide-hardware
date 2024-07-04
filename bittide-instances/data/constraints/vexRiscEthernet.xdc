# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set_clock_groups -asynchronous -group [get_clocks -include_generated_clocks CLK_125MHZ_P] -group [get_clocks -include_generated_clocks SGMIICLK_P]

set_property BOARD_PIN {CPU_RESET} [get_ports CPU_RESET]
set_property BOARD_PIN {sysclk_125_p} [get_ports CLK_125MHZ_P]
set_property BOARD_PIN {sysclk_125_n} [get_ports CLK_125MHZ_N]

# PMOD1_[0..7]
# Note that there are no clock capable pins in this list
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AL14} [get_ports {USB_UART_RXD}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM14} [get_ports {USB_UART_TXD}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AP16} [get_ports {JTAG_TCK}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AP15} [get_ports {JTAG_TDI}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM16} [get_ports {JTAG_RST}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM15} [get_ports {JTAG_TMS}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AN18} [get_ports {JTAG_TDO}]
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AN17} [get_ports {}]

# PMOD1 does not have a clock capable pin. To Vivado's credit, it refuses to
# produce a bitstream if we try to use a non-clock capable pin as a clock. With
# the following line, we tell Vivado to ignore this warning.
set_property CLOCK_DEDICATED_ROUTE FALSE [get_nets JTAG_TCK]
