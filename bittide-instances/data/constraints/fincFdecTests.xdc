# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
#
# NOTE: This configuration is only valid for the leftmost FPGA in the demo rack.

# CLK_125MHZ
set_property BOARD_PART_PIN sysclk_125_p [get_ports {CLK_125MHZ_p}]
set_property BOARD_PART_PIN sysclk_125_n [get_ports {CLK_125MHZ_n}]

# USER_SMA_CLOCK
set_property -dict {IOSTANDARD LVDS PACKAGE_PIN D23} [get_ports {USER_SMA_CLOCK_p}]
set_property -dict {IOSTANDARD LVDS PACKAGE_PIN C23} [get_ports {USER_SMA_CLOCK_n}]

# Vivado marks all clocks as related by default. Our external clocks are not
# though, which means that we need to explicitly mark them as unrelated (or
# "asynchronous").
set_clock_groups \
    -asynchronous \
    -group [get_clocks -include_generated_clocks {CLK_125MHZ_p}] \
    -group [get_clocks -include_generated_clocks {USER_SMA_CLOCK_p}]

# GPIO_LED_0_LS
set_property BOARD_PART_PIN GPIO_LED_0_LS [get_ports {done}]
# GPIO_LED_1_LS
set_property BOARD_PART_PIN GPIO_LED_1_LS [get_ports {success}]
