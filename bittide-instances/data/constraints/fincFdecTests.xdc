# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# SYSCLK_125
set_property BOARD_PART_PIN sysclk_125_p [get_ports {CLK_125MHZ_p}]
set_property BOARD_PART_PIN sysclk_125_n [get_ports {CLK_125MHZ_n}]
# SMA_MGT_REFCLK
set_property BOARD_PART_PIN sma_mgt_refclk_p [get_ports {SMA_MGT_REFCLK_C_p}]
set_property BOARD_PART_PIN sma_mgt_refclk_n [get_ports {SMA_MGT_REFCLK_C_n}]

# Vivado marks all clocks as related by default. Our external clocks are not
# though, which means that we need to explicitly mark them as unrelated (or
# "asynchronous").
set_clock_groups \
    -asynchronous \
    -group [get_clocks -include_generated_clocks {CLK_125MHZ_p}] \
    -group [get_clocks -include_generated_clocks {SMA_MGT_REFCLK_C_p}]

# GPIO_LED_0_LS
set_property BOARD_PART_PIN GPIO_LED_0_LS [get_ports {done}]
# GPIO_LED_1_LS
set_property BOARD_PART_PIN GPIO_LED_1_LS [get_ports {success}]
