# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# PMOD1_[0..7]
# Note that there are no clock capable pins in this list
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AL14} [get_ports {USB_UART_RXD}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AM14} [get_ports {USB_UART_TXD}]
