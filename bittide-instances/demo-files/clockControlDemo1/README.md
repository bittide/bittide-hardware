<!--
SPDX-FileCopyrightText: 2022-2023 Google LLC

SPDX-License-Identifier: Apache-2.0
-->
## How to set up clockControlDemo1
1. Generate HDL for clockControlDemo1 as by calling: `cabal run bittide-instances:shake -- clockControlDemo1:hdl`
2. Start new Vivado project.
3. Import the following files in Vivado:
  * all generated verilog files from `_build/clash/Bittide_Instances_MVPs_clockControlDemo1`
  * `topEntity.v`
  * `topEntity.xdc`
4. Set `topEntity.v` as top entity.
5. Set `topEntity.xdc` as target constraint file.
6. Generate `clashConnector.tcl` using `cabal run -- clash-lib:static-files --tcl-connector=clashConnector.tcl`.
7. In Vivado's TCL console, source the `clashConnector.tcl` file
8. In Vivado's TCL console, read the generated metadata file with `clash::readMetaData _build/clash/Bittide_Instances_MVPs_clockControlDemo1`
9. In Vivado's TCL console, generate all the IPs in the design with `clash::createIp`
10. In Vivado's IP catalog, use the clock wizard to generate a clock buffer for the incoming `FMC_HPC_CLK1_M2S clock`, see `topEntity.v` for enabled ports.
11. In Vivado's IP catalog, use the clock wizard to generate a clock buffer for the incoming `USER_SMA_CLOCK`, see `topEntity.v` for enabled ports.

See the constraint file, `topEntity.xdc`, for all pin mappings.
You should be able to create a bitstream now.
