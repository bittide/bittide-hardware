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
