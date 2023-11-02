# SPDX-FileCopyrightText: 2023 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# Bank  45 VCCO - VCC1V2_FPGA_3A - IO_L12P_T1U_N10_GC_45
set_property IOSTANDARD DIFF_SSTL12 [get_ports SYSCLK_300_p]
set_property ODT RTT_48 [get_ports SYSCLK_300_p]
#
# Bank  45 VCCO - VCC1V2_FPGA_3A - IO_L12N_T1U_N11_GC_45
set_property PACKAGE_PIN AK17 [get_ports SYSCLK_300_p]
set_property PACKAGE_PIN AK16 [get_ports SYSCLK_300_n]
set_property IOSTANDARD DIFF_SSTL12 [get_ports SYSCLK_300_n]
set_property ODT RTT_48 [get_ports SYSCLK_300_n]

set_property PACKAGE_PIN AN8 [get_ports CPU_RESET]
set_property IOSTANDARD LVCMOS18 [get_ports CPU_RESET]

set_property PACKAGE_PIN K26 [get_ports USB_UART_RX]
set_property IOSTANDARD LVCMOS18 [get_ports USB_UART_RX]
# Bank  95 VCCO -          - IO_L3N_T0L_N5_AD15N_A27_65
set_property PACKAGE_PIN G25 [get_ports USB_UART_TX]
set_property IOSTANDARD LVCMOS18 [get_ports USB_UART_TX]


#FMC_HPC_LA01_CC_N
set_property PACKAGE_PIN F9       [get_ports mux_select[0]]
set_property IOSTANDARD  LVCMOS18     [get_ports mux_select[0]]

#FMC_HPC_LA02_P
set_property PACKAGE_PIN K10      [get_ports mux_select[1]]
set_property IOSTANDARD  LVCMOS18     [get_ports mux_select[1]]

#FMC_HPC_LA02_N
set_property PACKAGE_PIN J10      [get_ports mux_select[2]]
set_property IOSTANDARD  LVCMOS18     [get_ports mux_select[2]]

#FMC_HPC_LA00_CC_P
set_property PACKAGE_PIN H11      [get_ports "sclBs"]
set_property IOSTANDARD  LVCMOS18 [get_ports "sclBs"]

# FMC_HPC_LA00_CC_N
set_property PACKAGE_PIN G11      [get_ports "sdaOut"]
set_property IOSTANDARD  LVCMOS18 [get_ports "sdaOut"]

# FMC_HPC_LA01_CC_P
set_property PACKAGE_PIN G9       [get_ports "sdaIn"]
set_property IOSTANDARD  LVCMOS18     [get_ports "sdaIn"]
