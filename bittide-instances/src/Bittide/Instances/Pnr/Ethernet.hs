-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -fplugin=Protocols.Plugin #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Bittide.Instances.Pnr.Ethernet where

import Clash.Explicit.Prelude
import Clash.Explicit.Reset.Extra
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import Clash.Cores.UART (ValidBaud)
import Clash.Cores.Xilinx.Ethernet.Gmii
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (simDna2)
import Protocols
import VexRiscv

import Bittide.Axi4
import Bittide.DoubleBufferedRam
import Bittide.Ethernet.Mac
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.Wishbone
import Protocols.Idle

type Baud = 921_600

baud :: SNat Baud
baud = SNat

{- | Instance containing:
* VexRiscv CPU
* UART
* Free running timer
* GPIO
* Ethernet MAC
-}
vexRiscGmii ::
  forall logic rx tx gpioWidth.
  ( KnownDomain logic
  , KnownDomain rx
  , KnownDomain tx
  , KnownNat (DomainPeriod logic)
  , 1 <= DomainPeriod logic
  , ValidBaud logic 921600
  ) =>
  SNat gpioWidth ->
  Clock logic ->
  Reset logic ->
  Clock rx ->
  Reset rx ->
  Clock tx ->
  Reset tx ->
  ( Signal logic Bit
  , Signal rx Gmii
  , Signal logic JtagIn
  ) ->
  ( Signal logic Bit
  , Signal tx Gmii
  , Signal logic JtagOut
  , Signal logic (BitVector gpioWidth)
  )
vexRiscGmii SNat sysClk sysRst rxClk rxRst txClk txRst fwd =
  (\((_, _, jtagBwd), (uartFwd, gmiiFwd, gpioFwd)) -> (uartFwd, gmiiFwd, jtagBwd, gpioFwd))
    $ toSignals
      ( circuit $ \(uartTx, gmiiRx, jtag) -> do
          [uartBus, timeBus, wbAxiRx, wbAxiTx, dnaWb, gpioWb, macWb] <- pe -< jtag
          (uartRx, _uartStatus) <- uart -< (uartBus, uartTx)
          time -< timeBus
          dna -< dnaWb
          macStatIf -< (macWb, macStatus)
          gpioDf <- idleSource -< ()
          gpioOut <- gpio -< (gpioWb, gpioDf)
          (axiRx0, gmiiTx, macStatus) <- mac -< (axiTx1, gmiiRx)
          axiRx1 <- axiRxPipe -< axiRx0
          axiTx0 <- wbToAxiTx' -< wbAxiTx
          axiTx1 <- axiTxPipe -< axiTx0
          _rxBufStatus <- wbAxiRxBuffer -< (wbAxiRx, axiRx1)

          idC -< (uartRx, gmiiTx, gpioOut)
      )
      (fwd, (pure (), pure (), pure ()))
 where
  time = wcre timeWb
  dna = wcre readDnaPortE2Wb simDna2
  mac =
    ethMac1GFifoC
      (SNat @1500)
      (SNat @1500)
      sysClk
      sysRst
      txClk
      txRst
      rxClk
      rxRst
      miiSel
      txClkEna
      rxClkEna
  macStatIf = wcre $ macStatusInterfaceWb d16
  uart = wcre $ uartInterfaceWb d32 d2 (uartDf baud)
  pe = wcre processingElement peConfig
  wbToAxiTx' = wcre wbToAxiTx
  wbAxiRxBuffer = wcre wbAxisRxBufferCircuit (SNat @2048)
  axiTxPipe = wcre (axiUserMapC (const False) <| axiStreamToByteStream)
  axiRxPipe = wcre (axiUserMapC or <| axiStreamFromByteStream)
  gpio = wcre $ registerWbC WishbonePriority (0 :: BitVector gpioWidth)
  miiSel = pure False
  rxClkEna = pure True
  txClkEna = pure True
  wcre :: (((HiddenClockResetEnable logic) => a) -> a)
  wcre = withClockResetEnable sysClk sysRst enableGen

  peConfig =
    PeConfig
      ( 0b1000
          :> 0b0001
          :> 0b0010
          :> 0b0011
          :> 0b0101
          :> 0b0110
          :> 0b0111
          :> 0b0100
          :> 0b1001
          :> Nil
      )
      (Undefined @(256 * 1024))
      (Undefined @(64 * 1024))

vexRiscEthernet ::
  Clock Basic125B ->
  Reset Basic125B ->
  DiffClock Basic625 ->
  ( Signal Basic125B JtagIn
  , Signal Basic125B Bit
  , Signal Basic625 Lvds
  ) ->
  ( Signal Basic125B JtagOut
  , Signal Basic125B Bit
  , Signal Basic625 Lvds
  , Signal Basic125B (BitVector 32)
  )
vexRiscEthernet sysClk sysRst sgmiiPhyClk (jtagin, uartIn, sgmiiIn) =
  (jtagOut, uartOut, bridgeLvdsOut, gpioOut)
 where
  BridgeOutput{..} = bridge sgmiiIn gmiiOut
  signalDetect = pure True
  anRestart = pure False
  conf = pure def{cAutoNegEnable = True}
  anConf =
    pure
      def
        { cAcknowledge = True
        , cDuplexMode = FullDuplex
        , cLinkSpeed = Speed1000
        , cPhyLinkStatus = True
        }
  bridge = gmiiSgmiiBridge sgmiiPhyClk bridgeRst signalDetect conf anConf anRestart
  rxClk = bridgeClk125 :: Clock Basic125A
  rxRst = bridgeRst125
  bridgeRst = unsafeResetDesynchronizer sysClk sysRst
  (uartOut, gmiiOut, jtagOut, gpioOut) = vexRiscGmii SNat sysClk sysRst rxClk rxRst rxClk rxRst (uartIn, bridgeGmiiRx, jtagin)

{- | Take a synchronous reset from one domain and convert it to an asynchronous reset.
This inserts a register in the source domain to prevent glitching and then converts the domain.
Note that the target domain is merely an implementation detail imposed by the digital
abstraction. The resulting reset is not synchronous to the target domain.
-}
unsafeResetDesynchronizer ::
  forall domA domS.
  (KnownDomain domA, KnownDomain domS, HasSynchronousReset domS, HasAsynchronousReset domA) =>
  -- | Clock in the source domain
  Clock domS ->
  -- | Synchronous reset in the source domain
  Reset domS ->
  -- | Asynchronous reset in the "target" domain
  Reset domA
unsafeResetDesynchronizer clkIn =
  unsafeFromActiveHigh
    . unsafeSynchronizer clkIn clockGen
    . unsafeToActiveHigh
    . delayReset Asserted clkIn

-- unsafeSynchronizer needs a clock in the target domain for simulation purposes, we
-- can only use clockGen here because the black box of unsafeSynchronizer does
-- not use the clock (it becomes a wire in the generated HDL).
