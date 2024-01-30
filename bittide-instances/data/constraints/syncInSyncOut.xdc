# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set_property BOARD_PART_PIN sysclk_300_n [get_ports SYSCLK_300_n]
set_property BOARD_PART_PIN sysclk_300_p [get_ports SYSCLK_300_p]

# USER SMA GPIO_P
set_property -dict {IOSTANDARD LVCMOS18 PACKAGE_PIN H27} [get_ports {SYNC_IN}]
# USER_SMA_GPIO_N (connected on node 0 to SYNC_IN of all nodes)
set_property -dict {IOSTANDARD LVCMOS18 PACKAGE_PIN G27} [get_ports {SYNC_OUT}]
