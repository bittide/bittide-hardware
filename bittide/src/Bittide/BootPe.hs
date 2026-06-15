-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.BootPe (BootPeBusses, bootPe, simpleBootPe) where

import Clash.Prelude
import Protocols

import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Uart (ValidBaud)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Mm)
import Protocols.Spi (Spi)
import VexRiscv

import Bittide.ClockControl.Si539xSpi (si539xSpiWb)
import Bittide.ProcessingElement (PeConfig (..), RemainingBusWidth, processingElement)
import Bittide.SharedTypes (BitboneMm)
import Bittide.Transceiver (COutputs)
import Bittide.Wishbone (timeWb, uartBytes, uartDf, uartInterfaceWb)

import qualified Bittide.Transceiver as Transceiver
import qualified Bittide.Transceiver.Wishbone as Transceiver
import qualified Clash.Cores.Xilinx.Gth as Gth

type BootPeBusses = 6

{- | Processing element with builtin time component, UART interface and SI539x SPI
interface. It exports one bus for the transceiver component.
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
    , "SPI_DONE" ::: CSignal dom Bool
    , Spi dom
    , "TRANSCEIVER" ::: BitboneMm dom (RemainingBusWidth BootPeBusses)
    )
bootPe peConfig = circuit $ \(mm, jtag) -> do
  [timeBus, uartBus, siBus, transceiverBus] <-
    processingElement NoDumpVcd peConfig -< (mm, jtag)

  Fwd _localCounter <- timeWb Nothing -< timeBus
  (uartOut, _uartStatus) <-
    uartInterfaceWb d16 d16 uartBytes -< (uartBus, Fwd (pure Nothing))
  (spiDone, spiOut) <- si539xSpiWb (SNat @(Microseconds 10)) -< siBus

  -- XXX: Should the transceiver just be part of the PE? This would add a whooole
  --      bunch of constraints to it.
  idC -< (uartOut, spiDone, spiOut, transceiverBus)

{- | Similar to 'bootPe', but with 2 key differences so it can be used standalone to get
the transceivers up and running. First, it directly exposes the UART as a @Bit@. Second,
it integrates the transceivers instead of exposing the bus.
-}
simpleBootPe ::
  forall tx rx ref free txS rxS n m baud.
  ( HasCallStack
  , HasSynchronousReset tx
  , HasDefinedInitialValues tx
  , HasSynchronousReset rx
  , HasDefinedInitialValues rx
  , HasSynchronousReset free
  , HasDefinedInitialValues free
  , KnownDomain rxS
  , KnownDomain txS
  , KnownDomain ref
  , KnownDomain free
  , KnownNat baud
  , ValidBaud free baud
  , KnownNat n
  , n <= 1024
  , n ~ m + 1
  , ?byteOrder :: ByteOrder
  ) =>
  PeConfig BootPeBusses ->
  SNat baud ->
  Clock free ->
  Reset free ->
  Circuit
    ( ToConstBwd Mm
    , Jtag free
    , Gth.Gths rx rxS tx txS ref n
    , CSignal tx (Vec n (BitVector 64))
    )
    ( "UART_TX" ::: CSignal free Bit
    , "SPI_DONE" ::: CSignal free Bool
    , Spi free
    , COutputs n tx rx free
    )
simpleBootPe peConfig baud refClk refRst = circuit $ \(mm, jtag, gths, txDatas) -> do
  [timeBus, uartBus, siBus, transceiverBus] <-
    withRefClockResetEnable $ processingElement NoDumpVcd peConfig -< (mm, jtag)

  Fwd _localCounter <- withRefClockResetEnable $ timeWb Nothing -< timeBus
  (uartTx, _uartStatus) <-
    withRefClockResetEnable $ uartInterfaceWb d16 d16 $ uartDf baud -< (uartBus, Fwd (pure 0))
  (spiDone, spi) <- withRefClockResetEnable $ si539xSpiWb (SNat @(Microseconds 10)) -< siBus

  tOutputs <-
    Transceiver.transceiverPrbsNWb
      refClk
      refRst
      Transceiver.defConfig
      -< (transceiverBus, gths, txDatas)

  idC -< (uartTx, spiDone, spi, tOutputs)
 where
  withRefClockResetEnable :: forall r. ((HiddenClockResetEnable free) => r) -> r
  withRefClockResetEnable = withClockResetEnable refClk refRst enableGen
