# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# SYSCLK_125
set_property BOARD_PIN sysclk_125_p [get_ports {CLK_125MHZ_p}]
set_property BOARD_PIN sysclk_125_n [get_ports {CLK_125MHZ_n}]

set_clock_groups \
  -asynchronous \
  -group [get_clocks -include_generated_clocks {CLK_125MHZ_p}] \
  -group [get_clocks -include_generated_clocks {SGMIICLK_p}]

# CPU_RESET
set_property BOARD_PIN CPU_RESET [get_ports {CPU_RESET}]
