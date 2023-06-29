# SPDX-FileCopyrightText: 2022-2023 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# CLK_125MHZ_P
set_property PACKAGE_PIN G10 [get_ports "CLK_125MHZ_P"]
set_property IOSTANDARD LVDS [get_ports "CLK_125MHZ_P"]
# CLK_125MHZ_P
set_property PACKAGE_PIN F10 [get_ports "CLK_125MHZ_N"]
set_property IOSTANDARD LVDS [get_ports "CLK_125MHZ_N"]


# GPIO_LED_0_LS
set_property PACKAGE_PIN AP8      [get_ports "done"]
set_property IOSTANDARD  LVCMOS18 [get_ports "done"]
# GPIO_LED_1_LS
set_property PACKAGE_PIN H23      [get_ports "success"]
set_property IOSTANDARD  LVCMOS18 [get_ports "success"]
