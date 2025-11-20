-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.BootPe (BootPeInternalBusses, bootPe) where

import Clash.Prelude

import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.MemoryMap (ConstBwd, MM)
import VexRiscv

import Bittide.ClockControl.Si539xSpi (si539xSpiWb)
import Bittide.ProcessingElement
import Bittide.Wishbone (timeWb, uartBytes, uartInterfaceWb, whoAmIC)

type BootPeInternalBusses = 6

bootPe ::
  forall dom.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  PeConfig BootPeInternalBusses ->
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
bootPe peConfig bootWhoAmId = circuit $ \(mm, (jtag, miso)) -> do
  [timeBus, whoAmIBus, uartBus, siBus] <- processingElement NoDumpVcd peConfig -< (mm, jtag)

  Fwd _localCounter <- timeWb -< timeBus
  whoAmIC bootWhoAmId -< whoAmIBus
  (uartOut, _uartStatus) <-
    uartInterfaceWb d16 d16 uartBytes -< (uartBus, Fwd (pure Nothing))
  (spiDone, spiOut) <- si539xSpiWb (SNat @(Microseconds 10)) -< (siBus, miso)

  idC -< (uartOut, spiDone, spiOut)
