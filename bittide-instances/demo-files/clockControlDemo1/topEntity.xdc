# SPDX-FileCopyrightText: 2022-2023 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# create_clock -name {SYSCLK_300_P} -period 3.333 -waveform {0.000 1.667} [get_ports {SYSCLK_300_P}]
# create_clock -name {USER_SMA_CLOCK_P} -period 5.000 -waveform {0.000 2.500} [get_ports {USER_SMA_CLOCK_P}]

#set_property IOSTANDARD DIFF_SSTL12 [get_ports SYSCLK_300_P]
#set_property ODT RTT_48 [get_ports SYSCLK_300_P]
#
# Bank  45 VCCO - VCC1V2_FPGA_3A - IO_L12N_T1U_N11_GC_45
#set_property PACKAGE_PIN AK17 [get_ports SYSCLK_300_P]
#set_property PACKAGE_PIN AK16 [get_ports SYSCLK_300_N]
#set_property IOSTANDARD DIFF_SSTL12 [get_ports SYSCLK_300_N]
#set_property ODT RTT_48 [get_ports SYSCLK_300_N]

# Bank  67 VCCO - VADJ_1V8_FPGA_10A - IO_L13P_T2L_N0_GC_QBC_67
set_property IOSTANDARD LVDS [get_ports "USER_SMA_CLOCK_P"]
set_property PACKAGE_PIN D23 [get_ports "USER_SMA_CLOCK_P"]
set_property DIFF_TERM  TRUE [get_ports "USER_SMA_CLOCK_P"]
#
# Bank  67 VCCO - VADJ_1V8_FPGA_10A - IO_L13N_T2L_N1_GC_QBC_67
set_property PACKAGE_PIN C23 [get_ports "USER_SMA_CLOCK_N"]
set_property IOSTANDARD LVDS [get_ports "USER_SMA_CLOCK_N"]
set_property DIFF_TERM  TRUE [get_ports "USER_SMA_CLOCK_N"]

# Bank  67 VCCO - VADJ_1V8_FPGA_10A - IO_L11P_T1U_N8_GC_67
set_property PACKAGE_PIN E25      [get_ports "FMC_HPC_CLK1_M2C_P"]
set_property IOSTANDARD  LVDS     [get_ports "FMC_HPC_CLK1_M2C_P"]
set_property DIFF_TERM   TRUE     [get_ports "FMC_HPC_CLK1_M2C_P"]
#
# Bank  67 VCCO - VADJ_1V8_FPGA_10A - IO_L11N_T1U_N9_GC_67
set_property PACKAGE_PIN D25      [get_ports "FMC_HPC_CLK1_M2C_N"]
set_property IOSTANDARD  LVDS     [get_ports "FMC_HPC_CLK1_M2C_N"]
set_property DIFF_TERM   TRUE     [get_ports "FMC_HPC_CLK1_M2C_N"]


set_clock_groups -asynchronous -group USER_SMA_CLOCK_P -group FMC_HPC_CLK1_M2C_P

# GPIO_SW_E
set_property PACKAGE_PIN AE8 [get_ports "drainFifoA"]
set_property IOSTANDARD LVCMOS18 [get_ports "drainFifoA"]

# GPIO_SW_W
set_property PACKAGE_PIN AF9 [get_ports "drainFifoB"]
set_property IOSTANDARD LVCMOS18 [get_ports "drainFifoB"]

#clkA
set_property PACKAGE_PIN AP16 [get_ports "domA_FDEC"]
set_property IOSTANDARD LVCMOS12 [get_ports "domA_FDEC"]

set_property PACKAGE_PIN AN18 [get_ports "domA_FINC"]
set_property IOSTANDARD LVCMOS12 [get_ports "domA_FINC"]

#clkB
set_property PACKAGE_PIN AL14 [get_ports "domB_FDEC"]
set_property IOSTANDARD LVCMOS12 [get_ports "domB_FDEC"]

set_property PACKAGE_PIN AM16 [get_ports "domB_FINC"]
set_property IOSTANDARD LVCMOS12 [get_ports "domB_FINC"]

#GPIO_LED_0_LS
set_property PACKAGE_PIN AP8 [get_ports "domA_isStable"]
set_property IOSTANDARD LVCMOS18 [get_ports "domA_isStable"]

#GPIO_LED_1_LS
set_property PACKAGE_PIN H23 [get_ports "domB_isStable"]
set_property IOSTANDARD LVCMOS18 [get_ports "domB_isStable"]

#GPIO_LED_1_LS
#set_property PACKAGE_PIN H23 [get_ports Underflowed]
#set_property IOSTANDARD LVCMOS18 [get_ports Underflowed]

#GPIO_LED_2_LS
#set_property PACKAGE_PIN P20 [get_ports Overflowed]
#set_property IOSTANDARD LVCMOS18 [get_ports Overflowed]

# Bank  85 VCCO -          - IO_L20N_T3L_N3_AD1N_D09_65
#set_property PACKAGE_PIN P21      [get_ports "GPIO_LED_3_LS"]
#set_property IOSTANDARD  LVCMOS18 [get_ports "GPIO_LED_3_LS"]
# Bank  85 VCCO -          - IO_L19P_T3L_N0_DBC_AD9P_D10_65
#set_property PACKAGE_PIN N22      [get_ports "GPIO_LED_4_LS"]
#set_property IOSTANDARD  LVCMOS18 [get_ports "GPIO_LED_4_LS"]
# Bank  85 VCCO -          - IO_L19N_T3L_N1_DBC_AD9N_D11_65
#set_property PACKAGE_PIN M22      [get_ports "GPIO_LED_5_LS"]
#set_property IOSTANDARD  LVCMOS18 [get_ports "GPIO_LED_5_LS"]
# Bank  85 VCCO -          - IO_L18P_T2U_N10_AD2P_D12_65
#GPIO_LED_6_LS
#set_property PACKAGE_PIN R23 [get_ports {EbMode[0]}]
#set_property IOSTANDARD LVCMOS18 [get_ports {EbMode[0]}]
# Bank  85 VCCO -          - IO_L18N_T2U_N11_AD2N_D13_65
#GPIO_LED_7_LS
#set_property PACKAGE_PIN P23 [get_ports {EbMode[1]}]
#set_property IOSTANDARD LVCMOS18 [get_ports {EbMode[1]}]
