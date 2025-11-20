-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.BootPe (bootPe) where

import Clash.Prelude

import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.MemoryMap (ConstBwd, MM)
import VexRiscv

import Bittide.ClockControl.Si539xSpi
import Bittide.DoubleBufferedRam (InitialContent (..))
import Bittide.ProcessingElement
import Bittide.Wishbone (timeWb, uartBytes, uartInterfaceWb, whoAmIC)

bootPe ::
  forall dom.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | WhoAmI identifier
  BitVector 32 ->
  Circuit
    ( ConstBwd MM
    , ( Jtag dom
      , "MISO" ::: CSignal dom Bit
      )
    )
    ( "UART_BYTES" ::: Df dom (BitVector 8)
    , "SPI_DONE" ::: CSignal dom Bool
    , ( "SCLK" ::: CSignal dom Bool
      , "MOSI" ::: CSignal dom Bit
      , "CSB" ::: CSignal dom Bool
      )
    )
bootPe bootWhoAmId = circuit $ \(mm, (jtag, miso)) -> do
  [timeBus, whoAmIBus, uartBus, siBus] <- processingElement NoDumpVcd peConfig -< (mm, jtag)

  Fwd _localCounter <- timeWb -< timeBus
  whoAmIC bootWhoAmId -< whoAmIBus
  (uartOut, _uartStatus) <-
    uartInterfaceWb d16 d16 uartBytes -< (uartBus, Fwd (pure Nothing))
  (spiDone, spiOut) <- si539xSpiWb (SNat @(Microseconds 10)) -< (siBus, miso)

  idC -< (uartOut, spiDone, spiOut)
 where
  peConfig =
    PeConfig
      { initI = Undefined @IMemWords
      , initD = Undefined @DMemWords
      , iBusTimeout = d0
      , dBusTimeout = d0
      , includeIlaWb = False
      }

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (32 * 1024) 4
