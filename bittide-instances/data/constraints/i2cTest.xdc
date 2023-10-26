# SPDX-FileCopyrightText: 2022-2023 Google LLC
#
# SPDX-License-Identifier: Apache-2.0


# CLK_125MHZ
set_property BOARD_PART_PIN sysclk_125_p [get_ports sys_125_p]
set_property BOARD_PART_PIN sysclk_125_n [get_ports sys_125_n]

#FMC_HPC_LA01_CC_N
set_property PACKAGE_PIN F9       [get_ports mux_select[0]]
set_property IOSTANDARD  LVCMOS18 [get_ports mux_select[0]]

#FMC_HPC_LA02_P
set_property PACKAGE_PIN K10      [get_ports mux_select[1]]
set_property IOSTANDARD  LVCMOS18 [get_ports mux_select[1]]

#FMC_HPC_LA02_N
set_property PACKAGE_PIN J10      [get_ports mux_select[2]]
set_property IOSTANDARD  LVCMOS18 [get_ports mux_select[2]]

#FMC_HPC_LA00_CC_P
set_property PACKAGE_PIN H11      [get_ports "sclOut"]
set_property IOSTANDARD  LVCMOS18 [get_ports "sclOut"]

# FMC_HPC_LA00_CC_N
set_property PACKAGE_PIN G11      [get_ports "sdaOut"]
set_property IOSTANDARD  LVCMOS18 [get_ports "sdaOut"]

# FMC_HPC_LA01_CC_P
set_property PACKAGE_PIN G9       [get_ports "sdaIn"]
set_property IOSTANDARD  LVCMOS18 [get_ports "sdaIn"]
