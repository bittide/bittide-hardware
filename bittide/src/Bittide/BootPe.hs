-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.BootPe (BootPeBusses, bootPe) where

import Clash.Prelude
import Protocols

import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Mm)
import VexRiscv

import Bittide.ProcessingElement (PeConfig (..), RemainingBusWidth, processingElement)
import Bittide.SharedTypes (BitboneMm)
import Bittide.Wishbone (timeWb, uartBytes, uartInterfaceWb)

type BootPeBusses = 6

{- | Processing element with builtin time component and UART interface. It exports
one bus for the SI539x SPI interface and one bus for the transceiver component.
Both are exported so they can be shared with other CPUs.
-}
bootPe ::
  forall dom.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , ?byteOrder :: ByteOrder
  ) =>
  PeConfig BootPeBusses ->
  Circuit
    ( ToConstBwd Mm
    , Jtag dom
    )
    ( "UART_BYTES" ::: Df dom (BitVector 8)
    , "SI539X_SPI" ::: BitboneMm dom (RemainingBusWidth BootPeBusses)
    , "TRANSCEIVER" ::: BitboneMm dom (RemainingBusWidth BootPeBusses)
    )
bootPe peConfig = circuit $ \(mm, jtag) -> do
  [timeBus, uartBus, siBus, transceiverBus] <-
    processingElement NoDumpVcd peConfig -< (mm, jtag)

  Fwd _localCounter <- timeWb Nothing -< timeBus
  (uartOut, _uartStatus) <-
    uartInterfaceWb d16 d16 uartBytes -< (uartBus, Fwd (pure Nothing))

  -- XXX: Should the transceiver just be part of the PE? This would add a whooole
  --      bunch of constraints to it.
  idC -< (uartOut, siBus, transceiverBus)
