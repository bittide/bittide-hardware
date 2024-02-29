# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0


set_property BOARD_PART_PIN sysclk_125_p [get_ports {SYSCLK_125_p}]
set_property BOARD_PART_PIN sysclk_125_n [get_ports {SYSCLK_125_n}]

# FMC_HPC_GBTCLK1_M2C
set_property PACKAGE_PIN H6 [get_ports {FMC_HPC_GBTCLK1_M2C_p}]
set_property PACKAGE_PIN H5 [get_ports {FMC_HPC_GBTCLK1_M2C_n}]

set_clock_groups \
    -asynchronous \
    -group [get_clocks -include_generated_clocks {SYSCLK_125_p}] \
    -group [get_clocks -include_generated_clocks {FMC_HPC_GBTCLK1_M2C_p}]

#FMC_HPC_LA01_CC_N
set_property PACKAGE_PIN F9       [get_ports muxSelect[0]]
set_property IOSTANDARD  LVCMOS18 [get_ports muxSelect[0]]
#FMC_HPC_LA02_P
set_property PACKAGE_PIN K10      [get_ports muxSelect[1]]
set_property IOSTANDARD  LVCMOS18 [get_ports muxSelect[1]]
#FMC_HPC_LA02_N
set_property PACKAGE_PIN J10      [get_ports muxSelect[2]]
set_property IOSTANDARD  LVCMOS18 [get_ports muxSelect[2]]
#FMC_HPC_LA00_P
set_property PACKAGE_PIN H11      [get_ports "sclBs"]
set_property IOSTANDARD  LVCMOS18 [get_ports "sclBs"]
# FMC_HPC_LA00_N
set_property PACKAGE_PIN G11      [get_ports "sdaOut"]
set_property IOSTANDARD  LVCMOS18 [get_ports "sdaOut"]
# FMC_HPC_LA01_P
set_property PACKAGE_PIN G9       [get_ports "sdaIn"]
set_property IOSTANDARD  LVCMOS18 [get_ports "sdaIn"]

# GPIO_LED_0_LS
set_property PACKAGE_PIN AP8      [get_ports "done"]
set_property IOSTANDARD  LVCMOS18 [get_ports "done"]
# GPIO_LED_1_LS
set_property PACKAGE_PIN H23      [get_ports "success"]
set_property IOSTANDARD  LVCMOS18 [get_ports "success"]

set_property PACKAGE_PIN K26      [get_ports "USB_UART_RX"]
set_property IOSTANDARD  LVCMOS18 [get_ports "USB_UART_RX"]

set_property PACKAGE_PIN G25      [get_ports "USB_UART_TX"]
set_property IOSTANDARD  LVCMOS18 [get_ports "USB_UART_TX"]
