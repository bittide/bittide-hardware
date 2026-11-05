<!--
SPDX-FileCopyrightText: 2026 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

## Boot PE

The Boot PE is the processing element that runs on the static clock. It configures the clock chip that makes the bittide clock, and then brings up the transceivers. See [Booting the bittide clock](../architecture.md#step-1-booting-the-bittide-clock) for where it fits in a bittide node.

Most designs use `bootPe`. The wire and soft UGN demos build their own Boot PE, `demoBootPe`, see [below](#demobootpe).

### `bootPe`

{{#drawio path="diagrams/boot-pe-versions.drawio" page=0}}

`bootPe` (`bittide/src/Bittide/BootPe.hs`) is the Boot PE for designs that use the Si5345 on the Trenz TEF0008 FMC card as their clock chip. Its CPU must:
1.  set VADJ to 1.8 V over the main I2C bus of the KCU105, then
2.  configure the Si5345 over I2C on the FMC pins.

VADJ powers the Si5345 and the I/O of the card through which we reach it, so the order is important.

`bootPe` contains:
-   **`processingElement`**: a VexRiscv RV32IMC CPU, an interconnect, and instruction and data memory.
-   **`timeWb`**: a timer.
-   **`uartInterfaceWb`**: a UART. Its bytes go out on the `UART_BYTES` output.
-   **`si5345I2cWb`**: the driver of the Si5345, at I2C address `0x69`. SCL runs at 400 kHz (`Si5345SclPeriod`), the maximum of the Si5345. A frequency change of the clock control then takes less than the 200 µs between two updates.
-   **`i2cControllerWb`**: an I2C controller on the main I2C bus of the KCU105, at 100 kHz. It is only used to set VADJ.

`bootPe` exposes optional extra busses for callers to connect their own devices, for example the transceivers.

`defBootPeConfig` has 16 KiB of instruction memory and 8 KiB of data memory. The instruction memory must be this large: setting VADJ, configuring the Si5345 with retries and printing errors takes 9 to 15 KiB of code, depending on the firmware and the build mode. The data memory holds the Si5345 configuration (about 2.5 KiB), and the rest is stack.

#### Firmware

`firmware-support/bittide-cpus/src/boot.rs` has the firmware for a `bootPe`:
-   **`start_fmc_clock`**: sets VADJ (`fmc_power.rs`), then writes a configuration to the clock chip, with retries. It returns an error when this fails, so the caller can decide what to do.
-   **`program_fmc_clock`**: calls `start_fmc_clock`, and panics when it fails.

The Si5345 configurations are made with ClockBuilder Pro. Each firmware binary has its own configuration, for example `CONFIG_200` in `wire-demo-boot` and `CONFIG_125` in `finc-fdec`.

#### Pins

Each design connects the pins of `bootPe` in its top entity. `Bittide.Instances.Tef0008` has the constant pin values that go with it:
-   **`si5345MuxSelect`**: `0b100` selects the Si5345 on the I2C mux in the MAX10 of the card.
-   **`iicMuxResetB`**: keeps the TCA9548 I2C switch of the KCU105 out of reset, so it does not hold the main I2C bus.

The constraints are in `bittide-instances/data/constraints/tef0008/`: `si5345I2c.xdc` for the I2C to the Si5345, and `mainI2c.xdc` for the main I2C bus.

### Users

| Design | Boot PE | Extra busses of the caller |
| --- | --- | --- |
| `fincFdecTests` | `bootPe`, `defBootPeConfig` | domain difference counters |
| `transceiversUpTest`, `linkConfigurationTest` | `bootPe`, `defBootPeConfig` | transceivers, check device |
| Wire and soft UGN demos | `demoBootPe`, `defBootPeConfig` | see below |

### `demoBootPe`

{{#drawio path="diagrams/boot-pe-versions.drawio" page=1}}

The wire and soft UGN demos do not use `bootPe`. After boot, the clock control CPU in the bittide domain also uses the Si5345 driver to adjust the frequency, and `bootPe` does not export this driver. The demos therefore use `demoBootPe` (`Bittide.Instances.Hitl.GenericDemo.BringUp`). It has the same devices as `bootPe`, but:
-   it has an input `CC_CLOCK_CHIP_BUS` for the bus of the clock control CPU, crossed to the static clock domain. An `arbiterMm` shares `si5345I2cWb` between the two CPUs.
-   it has one extra bus, `TRANSCEIVER_BUS`. `bringUp` shares it with the management unit through another `arbiterMm`.

The boot firmware of the demos calls `boot::run`: it calls `program_fmc_clock`, and then enables the transceivers.
