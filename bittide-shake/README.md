<!--
SPDX-FileCopyrightText: 2022-2023 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# bittide-shake
Build system based on Haskell's [shake](https://hackage.haskell.org/package/shake).
This system can be build to build targets in `bittide-instances` to various levels. Shake will make sure that all required steps are performed for the relevant build level, e.g. `<target>:place` will first make sure `<target>:hdl` and `<target>:synth` are up-to-date.

Different build levels:
* \<target>:hdl => Generate Verilog code for the Clash target.
* \<target>:synth => Perform synthesis for the target instance.
* \<target>:pnr => Perform place, route, and netlist generation for the target instance.
* \<target>:bitstream => Perform bitstream generation for the target instance.
* \<target>:program => Program the FPGA board connected to your PC.
* \<target>:test => Run hardware-in-the-loop test.
* \<target>:post-process => Run ILA data post processing.

## Prerequisites
* We have tested the build system with Vivado 2022.1
* To change the part for which the instances are synthesized, set either environment variable `SYNTHESIS_BOARD` or `SYNTHESIS_PART`.
  * For the part we've bought use either `SYNTHESIS_BOARD=xilinx.com:kcu105:part0:1.7` or `SYNTHESIS_PART=xcku040-ffva1156-2-e`. Note that for this board/part you need to use Vivado Enterprise.
  * If neither is set, instances are synthesized for `SYNTHESIS_PART=xcku035-ffva1156-2-e`, which is the smaller cousing of the FPGA we've bought, but which comes with a free license.
* Only targets which have the flag `targetHasXdc` can be used to generate a bitstream. This XDC file must have the same name as the instance, and be located in the `data/constraints/` directory.
* For targets which have the flag `targetHasVio`, a probes file is generated alongside the bitstream.
* Only targets which have the flag `targetHasTest` can be used to perform hardware tests.


## Shake
The build rules are defined in `bin/Shake.hs`. Shake can be called using:

```
shake
```

Which is a command alias for:

```
cabal run shake -- $@
```


You can list all build targets like this:

```
shake --help
```

All build results end up in `_build`.

## HDL generation
Example:

```
shake scatterUnitWb:hdl
```

## Synthesis
Example:

```
shake scatterUnitWb:synth
```

## Place, route, and netlist generation
Example:

```
shake scatterUnitWb:pnr
```

## Bitstream generation
Example:

```
shake clockControlDemo0:bitstream
```

## Board programming
Example:

```
shake clockControlDemo0:program --hardware-targets=OneAny
```

## Hardware testing
Example:

```
shake boardTestExtended:test --hardware-targets=OneAny
```

## ILA data post processing
Example:

```
shake boardTestExtended:post-process --hardware-targets=OneAny
```
