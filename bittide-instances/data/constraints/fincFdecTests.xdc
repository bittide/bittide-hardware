# SPDX-FileCopyrightText: 2026 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# SYSCLK_125
set_property BOARD_PART_PIN sysclk_125_p [get_ports {CLK_125MHZ_p}]
set_property BOARD_PART_PIN sysclk_125_n [get_ports {CLK_125MHZ_n}]
# FMC_HPC_GBTCLK1_M2C (MGTREFCLK1 of quad 228), driven by the Si5345 on the FMC card
set_property PACKAGE_PIN H6 [get_ports {FMC_HPC_GBTCLK1_M2C_C_p}]
set_property PACKAGE_PIN H5 [get_ports {FMC_HPC_GBTCLK1_M2C_C_n}]

# Vivado marks all clocks as related by default. Our external clocks are not
# though, which means that we need to explicitly mark them as unrelated (or
# "asynchronous").
set_clock_groups \
    -asynchronous \
    -group [get_clocks -include_generated_clocks {CLK_125MHZ_p}] \
    -group [get_clocks -include_generated_clocks {FMC_HPC_GBTCLK1_M2C_C_p}]
