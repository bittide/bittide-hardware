-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.BootPe (BootPeBusses, bootPe, defBootPeConfig) where

import Clash.Prelude
import Protocols

import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Mm)
import Protocols.Spi (Spi)
import VexRiscv

import Bittide.ClockControl.Si539xSpi (si539xSpiWb)
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

{- | Processing element with builtin time component, UART interface and SI539x SPI
interface. It exports @extra@ busses for additional peripherals, such as the transceivers.
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
    , "SPI_DONE" ::: CSignal dom Bool
    , Spi dom
    , Vec extra (BitboneMm dom (RemainingBusWidth (BootPeBusses + extra)))
    )
bootPe peConfig = circuit $ \(mm, jtag) -> do
  busses <- processingElement NoDumpVcd peConfig -< (mm, jtag)
  ([timeBus, uartBus, siBus], extraBusses) <- Vec.split -< busses

  Fwd _localCounter <- timeWb Nothing -< timeBus
  (uartOut, _uartStatus) <-
    uartInterfaceWb d16 d16 uartBytes -< (uartBus, Fwd (pure Nothing))
  (spiDone, spiOut) <- si539xSpiWb (SNat @(Microseconds 10)) -< siBus

  idC -< (uartOut, spiDone, spiOut, extraBusses)
