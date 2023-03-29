<!--
SPDX-FileCopyrightText: 2022-2023 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# bittide-instances
Instances of Bittide components with "realistic" type variable instances. These instances can be synthesized using Vivado - by default for `xcku035-ffva1156-2-e`. Note that this is the smaller cousin of the FPGA we've bought - but it comes with a free license.

This package uses [Shake](https://shakebuild.com/) as a build system. Although the rules are handrolled, they should find their way up to [clash-shake](https://hackage.haskell.org/package/clash-shake) in the future.

## General approach
We synthesize each component in the package `Bittide` and their composites on their own. Each should meet the target frequency of 200 MHz. (Note that when two components meet timing, their composite might not.) Not every component is suitable for top-level integration - due to the sheer number of pins required for some. To get timing information still, we can either synthesize using [Out of Context commands](https://docs.xilinx.com/r/2021.2-English/ug905-vivado-hierarchical-design/Synthesis?tocId=vkakVL_suw7wlNgcaeVIYQ) (TODO: Implement) or go through the whole flow using `Bittide.Instances.Hacks.reducePins`. This wrapper makes sure that a design with any number of pins is mapped to a single input and a single output pin - while making sure synthesis can't eliminate any logic due to this. This way, we can run Vivado all the way up to implementation:

![reducePins architecture](imgs/reducePins.svg)

The build system automatically generates _false path_ constraints for all input and output pins. Hence, any paths from the input pin to the shift registers, and any paths from the output pin to the output of a flipflop are dismissed from timing analysis. This makes sure only the design under test is analyzed, not the logic inserted to map to a single input/output pin.

**Note that false path constraints are also generated for any reset lines. You therefore need to make sure to properly synchronize your resets before feeding them to a circuit.**


## Prerequisites
* We have tested the build system with Vivado 2022.1
* To change the part for which the instances are synthesized, set the environment variable `SYNTHESIS_PART`. For the part we've bought use `SYNTHESIS_PART=xcku040-ffva1156-2-e`. Note that for this part you need to use Vivado Enterprise.
* For the step Bitstream generation an XDC file with pinmappings is required. This file must have the same name as the instance, and be located in the `data/constraints/` directory.


## Shake
The build rules are defined in `bin/Shake.hs`. Shake can be called using:

```
cabal run -- bittide-instances:shake
```

You can list all build targets like this:

```
cabal run -- bittide-instances:shake --help
```

All build results end up in `_build`.

## HDL generation
Example:

```
cabal run -- bittide-instances:shake scatterUnitWb:hdl
```

## Synthesis
Example:

```
cabal run -- bittide-instances:shake scatterUnitWb:synth
```

## Implementation
Example:

```bash
# For just placement:
cabal run -- bittide-instances:shake scatterUnitWb:place

# For place & route:
cabal run -- bittide-instances:shake scatterUnitWb:route
```

## Netlist
Example:

```
cabal run -- bittide-instances:shake scatterUnitWb:netlist
```

## Bitstream generation
Example:

```
cabal run -- bittide-instances:shake clockControlDemo0:bitstream
```
