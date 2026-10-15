# SPDX-FileCopyrightText: 2026 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# SYSCLK_125
set_property BOARD_PART_PIN sysclk_125_p [get_ports {CLK_125MHZ_p}]
set_property BOARD_PART_PIN sysclk_125_n [get_ports {CLK_125MHZ_n}]

# Main I2C bus of the KCU105, through level shifter U77. See UG917, section
# "I2C Bus, Topology, and Switches". Pins as in the KCU105 board files. Not
# assigned with BOARD_PART_PIN: Vivado then infers an OBUFT instead of an IOBUF
# for these bidirectional pins.
set_property -dict {PACKAGE_PIN J24 IOSTANDARD LVCMOS18} [get_ports {IIC_SCL_MAIN_LS}]
set_property -dict {PACKAGE_PIN J25 IOSTANDARD LVCMOS18} [get_ports {IIC_SDA_MAIN_LS}]
set_property PULLUP true [get_ports {IIC_SCL_MAIN_LS IIC_SDA_MAIN_LS}]

# Active-low reset of the TCA9548 I2C switch (U28).
set_property PACKAGE_PIN AP10 [get_ports {IIC_MUX_RESET_B_LS}]
set_property IOSTANDARD LVCMOS18 [get_ports {IIC_MUX_RESET_B_LS}]
