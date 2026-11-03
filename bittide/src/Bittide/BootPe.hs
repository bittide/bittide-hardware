-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.BootPe (
  BootPeBusses,
  bootPe,
  defBootPeConfig,
  Si5345SclPeriod,
) where

import Clash.Prelude
import Protocols

import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Mm)
import VexRiscv

import Bittide.ClockControl.Si5345I2c (si5345I2cWb)
import Bittide.I2c (I2c, i2cControllerWb)
import Bittide.ProcessingElement (
  PeConfig (..),
  PrefixWidth,
  RemainingBusWidth,
  processingElement,
 )
import Bittide.SharedTypes (BitboneMm)
import Bittide.Wishbone (timeWb, uartBytes, uartInterfaceWb)

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Protocols.Vec as Vec

{- | Number of busses used by 'bootPe' itself, including the busses internal to
the 'processingElement'.
-}
type BootPeBusses = 6

{- | Default configuration for 'bootPe': 16 KiB instruction memory, 8 KiB data
memory.
-}
defBootPeConfig :: (KnownNat n, 2 <= n, PrefixWidth n <= 30) => PeConfig n
defBootPeConfig =
  PeConfig
    { cpu = Riscv32imc.vexRiscv0
    , depthI = SNat @(Div (16 * 1024) 4)
    , depthD = SNat @(Div (8 * 1024) 4)
    , initI = Nothing
    , initD = Nothing
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

{- | Minimum SCL period of the I2C to the Si5345 in 'bootPe': 400 kHz, the
maximum of the Si5345. A shorter period makes frequency adjustments faster.
-}
type Si5345SclPeriod = Nanoseconds 2500

{- | Processing element for designs that use the Si5345 on the Trenz TEF0008 FMC
card as their clock chip. Besides a timer and a UART, it has:

  * an I2C controller on the main I2C bus of the KCU105, to set VADJ to 1.8 V. VADJ
    powers the Si5345 and the I/O of the card through which we reach it (see
    @firmware-support/bittide-cpus/src/fmc_power.rs@).
  * a driver for the Si5345, over the I2C on the FMC pins.

It exports @extra@ busses for additional peripherals, such as the transceivers.
-}
bootPe ::
  forall extra dom.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , ?byteOrder :: ByteOrder
  , KnownNat extra
  , PrefixWidth (BootPeBusses + extra) <= 30
  , 4 <= RemainingBusWidth (BootPeBusses + extra)
  ) =>
  PeConfig (BootPeBusses + extra) ->
  Circuit
    ( ToConstBwd Mm
    , Jtag dom
    )
    ( "UART_BYTES" ::: Df dom (BitVector 8)
    , "CLOCK_CHIP_DONE" ::: CSignal dom Bool
    , "MAIN_I2C" ::: I2c dom
    , "FMC_I2C" ::: I2c dom
    , Vec extra (BitboneMm dom (RemainingBusWidth (BootPeBusses + extra)))
    )
bootPe peConfig = circuit $ \(mm, jtag) -> do
  busses <- processingElement NoDumpVcd peConfig -< (mm, jtag)
  ([timeBus, uartBus, clockChipBus, mainI2cBus], extraBusses) <- Vec.split -< busses

  Fwd _localCounter <- timeWb Nothing -< timeBus
  (uartOut, _uartStatus) <-
    uartInterfaceWb d16 d16 uartBytes -< (uartBus, Fwd (pure Nothing))

  (clockChipDone, fmcI2c) <- si5345I2cWb (SNat @Si5345SclPeriod) 0x69 -< clockChipBus

  -- The I2C switches on the KCU105 support up to 400 kHz, but we only use this
  -- bus to set VADJ once. Run at 100 kHz to be safe.
  mainI2c <- i2cControllerWb (SNat @(Microseconds 10)) -< mainI2cBus

  idC -< (uartOut, clockChipDone, mainI2c, fmcI2c, extraBusses)
