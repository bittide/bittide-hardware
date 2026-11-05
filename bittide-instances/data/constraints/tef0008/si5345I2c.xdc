# SPDX-FileCopyrightText: 2026 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# I2C to the Si5345 on the FMC card. SCL is bidirectional, SDA uses a separate
# output and input pin.
#
# FPGA pin | FMC HPC pin | Connection
# ---------|-------------|-----------
# H11      | LA00_CC_P   | SCL
# G11      | LA00_CC_N   | SDA (out)
# G9       | LA01_CC_P   | SDA (in)
# F9       | LA01_CC_N   | MUX_SELECT[0]
# K10      | LA02_P      | MUX_SELECT[1]
# J10      | LA02_N      | MUX_SELECT[2]
set_property -dict {IOSTANDARD LVCMOS18 PACKAGE_PIN H11 PULLUP TRUE} [get_ports {FMC_SCL}]
set_property -dict {IOSTANDARD LVCMOS18 PACKAGE_PIN G11} [get_ports {FMC_SDA_OUT}]
set_property -dict {IOSTANDARD LVCMOS18 PACKAGE_PIN G9 PULLUP TRUE} [get_ports {FMC_SDA_IN}]
set_property -dict {IOSTANDARD LVCMOS18 PACKAGE_PIN F9} [get_ports {FMC_MUX_SELECT[0]}]
set_property -dict {IOSTANDARD LVCMOS18 PACKAGE_PIN K10} [get_ports {FMC_MUX_SELECT[1]}]
set_property -dict {IOSTANDARD LVCMOS18 PACKAGE_PIN J10} [get_ports {FMC_MUX_SELECT[2]}]
