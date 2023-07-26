# SPDX-FileCopyrightText: 2022-2023 Google LLC
#
# SPDX-License-Identifier: Apache-2.0


# CLK_125MHZ
set_property BOARD_PART_PIN sysclk_125_p [get_ports {CLK_125MHZ_p}]
set_property BOARD_PART_PIN sysclk_125_n [get_ports {CLK_125MHZ_n}]

# GPIO_LED_0_LS
set_property BOARD_PART_PIN GPIO_LED_0_LS [get_ports {done}]
# GPIO_LED_1_LS
set_property BOARD_PART_PIN GPIO_LED_1_LS [get_ports {success}]
