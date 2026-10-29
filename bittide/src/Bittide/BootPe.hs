-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.BootPe (
  BootPeBusses,
  bootPe,
  defBootPeConfig,
  fmcBootPe,
  defFmcBootPeConfig,
  DefSi5345SclPeriod,
) where

import Clash.Prelude
import Protocols

import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Mm)
import VexRiscv

import Bittide.ClockControl.Si5345I2c (si5345I2cWb)
import Bittide.I2c (I2cIn, I2cOut, i2cControllerWb)
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
type BootPeBusses = 5

-- | Default configuration for 'bootPe': 4 KiB instruction memory, 16 KiB data memory.
defBootPeConfig :: (KnownNat n, 2 <= n, PrefixWidth n <= 30) => PeConfig n
defBootPeConfig =
  PeConfig
    { cpu = Riscv32imc.vexRiscv0
    , depthI = SNat @(Div (4 * 1024) 4)
    , depthD = SNat @(Div (16 * 1024) 4)
    , initI = Nothing
    , initD = Nothing
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

{- | Processing element with builtin time component and UART interface. It exports
a bus for the SI539x SPI interface, so it can be shared with other CPUs, and @extra@
busses for additional peripherals, such as the transceivers.
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
    , "SI539X_SPI" ::: BitboneMm dom (RemainingBusWidth (BootPeBusses + extra))
    , Vec extra (BitboneMm dom (RemainingBusWidth (BootPeBusses + extra)))
    )
bootPe peConfig = circuit $ \(mm, jtag) -> do
  busses <- processingElement NoDumpVcd peConfig -< (mm, jtag)
  ([timeBus, uartBus, siBus], extraBusses) <- Vec.split -< busses

  Fwd _localCounter <- timeWb Nothing -< timeBus
  (uartOut, _uartStatus) <-
    uartInterfaceWb d16 d16 uartBytes -< (uartBus, Fwd (pure Nothing))

  idC -< (uartOut, siBus, extraBusses)

{- | Default configuration for 'fmcBootPe': 16 KiB instruction memory, 16 KiB data
memory. Setting VADJ and configuring the Si5345 takes more code than fits in the
instruction memory of 'defBootPeConfig'.
-}
defFmcBootPeConfig :: (KnownNat n, 2 <= n, PrefixWidth n <= 30) => PeConfig n
defFmcBootPeConfig =
  PeConfig
    { cpu = Riscv32imc.vexRiscv0
    , depthI = SNat @(Div (16 * 1024) 4)
    , depthD = SNat @(Div (16 * 1024) 4)
    , initI = Nothing
    , initD = Nothing
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

{- | Default minimum SCL period of the I2C to the Si5345 in 'fmcBootPe': 100 kHz.
The Si5345 supports up to 400 kHz. A shorter period makes frequency adjustments
faster.
-}
type DefSi5345SclPeriod = Microseconds 10

{- | 'bootPe' for designs that use the Si5345 on the Trenz TEF0008 FMC card as
their clock chip. Its boot CPU:

  1. sets VADJ to 1.8 V over the main I2C bus of the KCU105, as VADJ powers the
     Si5345 and the I/O of the card through which we reach it (see
     @firmware-support/bittide-cpus/src/fmc_power.rs@), then
  2. configures the Si5345 over the I2C on the FMC pins.
-}
fmcBootPe ::
  forall extra si5345SclPeriodPs dom.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , ?byteOrder :: ByteOrder
  , KnownNat extra
  , PrefixWidth (BootPeBusses + 1 + extra) <= 30
  , 4 <= RemainingBusWidth (BootPeBusses + 1 + extra)
  ) =>
  -- | Minimum SCL period of the I2C to the Si5345, see 'DefSi5345SclPeriod'
  SNat si5345SclPeriodPs ->
  PeConfig (BootPeBusses + 1 + extra) ->
  Circuit
    ( ToConstBwd Mm
    , Jtag dom
    , "MAIN_I2C" ::: CSignal dom I2cIn
    , "FMC_I2C" ::: CSignal dom I2cIn
    )
    ( "UART_BYTES" ::: Df dom (BitVector 8)
    , "CLOCK_CHIP_DONE" ::: CSignal dom Bool
    , "MAIN_I2C" ::: CSignal dom I2cOut
    , "FMC_I2C" ::: CSignal dom I2cOut
    , Vec extra (BitboneMm dom (RemainingBusWidth (BootPeBusses + 1 + extra)))
    )
fmcBootPe si5345SclPeriod peConfig = circuit $ \(mm, jtag, mainI2cIn, fmcI2cIn) -> do
  (uartBytes, clockChipBus, busses) <- bootPe @(1 + extra) peConfig -< (mm, jtag)
  ([mainI2cBus], extraBusses) <- Vec.split -< busses

  (clockChipDone, fmcI2cOut) <- si5345I2cWb si5345SclPeriod 0x69 -< (clockChipBus, fmcI2cIn)

  -- The I2C switches on the KCU105 support up to 400 kHz, but we only use this
  -- bus to set VADJ once. Run at 100 kHz to be safe.
  mainI2cOut <- i2cControllerWb (SNat @(Microseconds 10)) -< (mainI2cBus, mainI2cIn)

  idC -< (uartBytes, clockChipDone, mainI2cOut, fmcI2cOut, extraBusses)
