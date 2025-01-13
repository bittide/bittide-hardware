# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# PMOD0_[0..7]
# Note that AH18 is a global clock capable pin
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AK25} [get_ports {USB_UART_RXD}]
set_property -dict {IOSTANDARD LVCMOS12 PACKAGE_PIN AN21} [get_ports {USB_UART_TXD}]
