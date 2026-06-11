<!--
SPDX-FileCopyrightText: 2022 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# bittide-instances
Collection of monomorphic instances of realistic Bittide components. These instances are meant
to be handled by bittide-shake.

This collection contains instances with various purposes:
* CI tests that ensure all components will meet timing.
* Instances that can be loaded onto the KCU105 target board.
* Hardware in the loop tests.

## General approach
We synthesize each component in the package `Bittide` and their composites on their own. Each should meet the target frequency of 200 MHz. (Note that when two components meet timing, their composite might not.) Not every component is suitable for top-level integration - due to the sheer number of pins required for some. To get timing information still, we can either synthesize using [Out of Context commands](https://docs.xilinx.com/r/2021.2-English/ug905-vivado-hierarchical-design/Synthesis?tocId=vkakVL_suw7wlNgcaeVIYQ) (TODO: Implement) or go through the whole flow using `Bittide.Instances.Hacks.reducePins`. This wrapper makes sure that a design with any number of pins is mapped to a single input and a single output pin - while making sure synthesis can't eliminate any logic due to this. This way, we can run Vivado all the way up to implementation:

![reducePins architecture](imgs/reducePins.svg)

The build system automatically generates _false path_ constraints for all input and output pins. Hence, any paths from the input pin to the shift registers, and any paths from the output pin to the output of a flipflop are dismissed from timing analysis. This makes sure only the design under test is analyzed, not the logic inserted to map to a single input/output pin.

**Note that false path constraints are also generated for any reset lines. You therefore need to make sure to properly synchronize your resets before feeding them to a circuit.**

To learn more about using these instances, go to the [`README.md` in `bittide-shake`](../bittide-shake/README.md).


## Testing components at higher frequencies
The Bittide demo designs currently run at 125 MHz. The table below shows how far each core component can be pushed in isolation, by testing place-and-route at 300, 350, and 400 MHz. Components that are FPGA-specific, or that add no logic, are omitted.

| Name pnr target         | 300 | 350 | 400 |
|:------------------------|:---:|:---:|:---:|
|`arbiter`                |  X  |  X  |  X  |
|`asciiDebugMuxFast`      |  X  |  X  |  X  |
|`autoCenterFast`         |  X  |  X  |  X  |
|`captureUgnFast`         |  X  |  X  |  X  |
|`delayWishbone`          |  X  |  X  |  X  |
|`domainDiffCounterFast`  |  X  |  X  |  X  |
|`freezeFast`             |  X  |  X  |  X  |
|`handshakesWbFast`       |  X  |     |     |
|`programmableMuxFast`    |  X  |  X  |  X  |
|`receiveRingBufferFast`  |  X  |  X  |     |
|`sendUgnFast`            |  X  |  X  |  X  |
|`si5391SpiFast`          |  X  |  X  |  X  |
|`timeWbFast`             |  X  |  X  |     |
|`transmitRingBufferFast` <sup> 1 </sup> |  X  |    |     |
|`uartExampleFast`<sup> 2 </sup> |  X  |  X  |     |
|`wbStorageFast`          |  X  |     |     |
|`wireDemoPeFast`         |  X  |  X  |  X  |

<sup> 1 </sup> Fails at 350 MHz, but does run at 340 MHz
<sup> 2 </sup> Contains: `uartInterfaceWb`, `uartBytes`, `uartDf`, `asciiDebugMux` and `dcFifoDf`

Note that the table above is missing some components which are used in demos. The table below lists the missing components and the reason there is not yet a high-frequency pnr target for it.

Component                 | Reason
--------------------------|----------
`elasticBuffer`           | Only test elastic buffer control and autocenter, not EB itself
`readDnaPortE2`           | Did not reach 250 MHz, uses an FPGA-specific primitive
`processingElement`       | Did not reach 200 MHz
`callistoSwClockControlC` | Did not reach 200 MHz (contains a lot of components)
`jtagChain`               | Doesn't add logic
`unsafeJtagSynchronizer`  | Doesn't add logic
`wireDemoPeConfig`        | Only wraps `deviceWb` with no other logic
`syncInCounterC`          | To be removed in issue https://github.com/bittide/bittide-hardware/issues/1155
`syncOutGenerateWbC`      | To be removed in issue https://github.com/bittide/bittide-hardware/issues/1155
