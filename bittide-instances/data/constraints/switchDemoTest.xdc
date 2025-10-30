# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set_property BOARD_PART_PIN sysclk_125_n [get_ports SYSCLK_125_n]
set_property BOARD_PART_PIN sysclk_125_p [get_ports SYSCLK_125_p]
set_property BOARD_PART_PIN sma_mgt_refclk_n [get_ports SMA_MGT_REFCLK_C_n]
set_property BOARD_PART_PIN sma_mgt_refclk_p [get_ports SMA_MGT_REFCLK_C_p]

set_property BOARD_PART_PIN GPIO_LED_0_LS [get_ports spiDone]

set_clock_groups \
  -asynchronous \
  -group [get_clocks -include_generated_clocks {SYSCLK_125_p}] \
  -group [get_clocks -include_generated_clocks {SMA_MGT_REFCLK_C_p}]

# USER SMA GPIO_P
set_property -dict {IOSTANDARD LVCMOS18 PACKAGE_PIN H27} [get_ports {SYNC_IN}]
# USER_SMA_GPIO_N (connected on node 0 to SYNC_IN of all nodes)
set_property -dict {IOSTANDARD LVCMOS18 PACKAGE_PIN G27} [get_ports {SYNC_OUT}]
