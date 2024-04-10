# SPDX-FileCopyrightText: 2022-2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0


# CLK_125MHZ
set_property BOARD_PART_PIN sysclk_125_p [get_ports {CLK_125MHZ_p}]
set_property BOARD_PART_PIN sysclk_125_n [get_ports {CLK_125MHZ_n}]

# GPIO_LED_0_LS
set_property BOARD_PART_PIN GPIO_LED_0_LS [get_ports {done}]
# GPIO_LED_1_LS
set_property BOARD_PART_PIN GPIO_LED_1_LS [get_ports {success}]

# PMOD0_[0..7]
# Note that AH18 is a global clock capable pin
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AK25} [get_ports {USB_UART_RXD}]
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AN21} [get_ports {USB_UART_TXD}]
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AH18} [get_ports {JTAG_TCK}]
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM19} [get_ports {JTAG_TDI}]
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AE26} [get_ports {JTAG_RST}]
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AF25} [get_ports {JTAG_TMS}]
# set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AE21} [get_ports {JTAG_TDO}]
# # set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM17} [get_ports {}]

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

set_clock_groups \
  -asynchronous \
  -group [get_clocks -include_generated_clocks {CLK_125MHZ_p}]

# Usually JTAG_TCK would be appended to the command above, but Intel JTAG config
# (see jtag.xdc) already does this for us:
#
# -group [get_clocks -include_generated_clocks {JTAG_TCK}]
