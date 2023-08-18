# SPDX-FileCopyrightText: 2022-2023 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# USER_SMA_CLOCK_p
set_property PACKAGE_PIN D23      [get_ports "USER_SMA_CLOCK_p"]
set_property IOSTANDARD  LVDS     [get_ports "USER_SMA_CLOCK_p"]
# USER_SMA_CLOCK_n
set_property PACKAGE_PIN C23      [get_ports "USER_SMA_CLOCK_n"]
set_property IOSTANDARD  LVDS     [get_ports "USER_SMA_CLOCK_n"]

# FMC_HPC_CLK1_M2C_p
set_property PACKAGE_PIN E25      [get_ports "FMC_HPC_CLK1_M2C_p"]
set_property IOSTANDARD  LVDS     [get_ports "FMC_HPC_CLK1_M2C_p"]
set_property DIFF_TERM   TRUE     [get_ports "FMC_HPC_CLK1_M2C_p"]
# FMC_HPC_CLK1_M2C_n
set_property PACKAGE_PIN D25      [get_ports "FMC_HPC_CLK1_M2C_n"]
set_property IOSTANDARD  LVDS     [get_ports "FMC_HPC_CLK1_M2C_n"]
set_property DIFF_TERM   TRUE     [get_ports "FMC_HPC_CLK1_M2C_n"]

# Vivado marks all clocks as related by default. Our external clocks are not
# though, which means that we need to explicitly mark them as unrelated (or
# "asynchronous").
set_clock_groups \
    -asynchronous \
    -group [get_clocks -include_generated_clocks {USER_SMA_CLOCK_p}] \
    -group [get_clocks -include_generated_clocks {FMC_HPC_CLK1_M2C_p}]

# GPIO_SW_E
set_property PACKAGE_PIN AE8      [get_ports "drainFifoA"]
set_property IOSTANDARD  LVCMOS18 [get_ports "drainFifoA"]
# GPIO_SW_W
set_property PACKAGE_PIN AF9      [get_ports "drainFifoB"]
set_property IOSTANDARD  LVCMOS18 [get_ports "drainFifoB"]


# PMOD1_2_LS
set_property PACKAGE_PIN AP16     [get_ports "domA_FDEC"]
set_property IOSTANDARD  LVCMOS12 [get_ports "domA_FDEC"]
# PMOD1_6_LS
set_property PACKAGE_PIN AN18     [get_ports "domA_FINC"]
set_property IOSTANDARD  LVCMOS12 [get_ports "domA_FINC"]

# PMOD0_2_LS
set_property PACKAGE_PIN AH18      [get_ports "domB_FDEC"]
set_property IOSTANDARD  LVCMOS12  [get_ports "domB_FDEC"]
# PMOD0_6_LS
set_property PACKAGE_PIN AE21      [get_ports "domB_FINC"]
set_property IOSTANDARD  LVCMOS12  [get_ports "domB_FINC"]


# GPIO_LED_0_LS
set_property PACKAGE_PIN AP8      [get_ports "domA_isStable"]
set_property IOSTANDARD  LVCMOS18 [get_ports "domA_isStable"]
# GPIO_LED_1_LS
set_property PACKAGE_PIN H23      [get_ports "domB_isStable"]
set_property IOSTANDARD  LVCMOS18 [get_ports "domB_isStable"]
