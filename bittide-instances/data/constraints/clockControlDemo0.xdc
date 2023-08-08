# SPDX-FileCopyrightText: 2022-2023 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# SYSCLK_300_p
set_property PACKAGE_PIN AK17        [get_ports "SYSCLK_300_p"]
set_property IOSTANDARD  DIFF_SSTL12 [get_ports "SYSCLK_300_p"]
set_property ODT         RTT_48      [get_ports "SYSCLK_300_p"]
# SYSCLK_300_n
set_property PACKAGE_PIN AK16        [get_ports "SYSCLK_300_n"]
set_property IOSTANDARD  DIFF_SSTL12 [get_ports "SYSCLK_300_n"]
set_property ODT         RTT_48      [get_ports "SYSCLK_300_n"]

# USER_SMA_CLOCK_p
set_property PACKAGE_PIN D23      [get_ports "USER_SMA_CLOCK_p"]
set_property IOSTANDARD  LVDS     [get_ports "USER_SMA_CLOCK_p"]
# USER_SMA_CLOCK_n
set_property PACKAGE_PIN C23      [get_ports "USER_SMA_CLOCK_n"]
set_property IOSTANDARD  LVDS     [get_ports "USER_SMA_CLOCK_n"]

# Vivado marks all clocks as related by default. Our external clocks are not
# though, which means that we need to explicitly mark them as unrelated (or
# "asynchronous").
set_clock_groups \
    -asynchronous \
    -group [get_clocks -include_generated_clocks {SYSCLK_300_p}] \
    -group [get_clocks -include_generated_clocks {USER_SMA_CLOCK_p}]

# GPIO_SW_E
set_property PACKAGE_PIN AE8      [get_ports "drainFifo"]
set_property IOSTANDARD  LVCMOS18 [get_ports "drainFifo"]
# GPIO_SW_C
set_property PACKAGE_PIN AE10     [get_ports "rstExternal"]
set_property IOSTANDARD  LVCMOS18 [get_ports "rstExternal"]
# GPIO_SW_W
set_property PACKAGE_PIN AF9      [get_ports "stabilityCheckReset"]
set_property IOSTANDARD  LVCMOS18 [get_ports "stabilityCheckReset"]


# PMOD1_2_LS
set_property PACKAGE_PIN AP16     [get_ports "FDEC"]
set_property IOSTANDARD  LVCMOS12 [get_ports "FDEC"]
# PMOD1_6_LS
set_property PACKAGE_PIN AN18     [get_ports "FINC"]
set_property IOSTANDARD  LVCMOS12 [get_ports "FINC"]


# GPIO_LED_0_LS
set_property PACKAGE_PIN AP8      [get_ports "isStable"]
set_property IOSTANDARD  LVCMOS18 [get_ports "isStable"]
# GPIO_LED_1_LS
set_property PACKAGE_PIN H23      [get_ports "Underflowed"]
set_property IOSTANDARD  LVCMOS18 [get_ports "Underflowed"]
# GPIO_LED_2_LS
set_property PACKAGE_PIN P20      [get_ports "Overflowed"]
set_property IOSTANDARD  LVCMOS18 [get_ports "Overflowed"]
#GPIO_LED_6_LS
set_property PACKAGE_PIN R23      [get_ports {EbMode[0]}]
set_property IOSTANDARD  LVCMOS18 [get_ports {EbMode[0]}]
#GPIO_LED_7_LS
set_property PACKAGE_PIN P23      [get_ports {EbMode[1]}]
set_property IOSTANDARD  LVCMOS18 [get_ports {EbMode[1]}]
