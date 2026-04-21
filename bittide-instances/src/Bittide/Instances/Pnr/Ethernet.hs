-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Bittide.Instances.Pnr.Ethernet where

import Clash.Explicit.Prelude
import Clash.Explicit.Reset.Extra
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import Clash.Cores.UART (ValidBaud)
import Clash.Cores.UART.Extra
import Clash.Cores.Xilinx.Ethernet.Gmii
import Clash.Cores.Xilinx.Unisim.DnaPortE2 (simDna2)
import Clash.Explicit.Testbench
import Project.FilePath (CargoBuildType (Release))
import Protocols
import Protocols.MemoryMap as Mm
import VexRiscv

import Bittide.Axi4
import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.Ethernet.Mac
import Bittide.Instances.Common (PeConfigElfSource (NameOnly), emptyPeConfig, peConfigFromElf)
import Bittide.Instances.Domains
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone

import qualified Protocols.ToConst as ToConst

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
#else
type Baud = 921_600
#endif

baud :: SNat Baud
baud = SNat

peConfigSim :: IO (PeConfig 8)
peConfigSim =
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    (NameOnly "smoltcp_client")
    Release
    d0
    d0
    False
    vexRiscv0

peConfigRtl :: PeConfig 8
peConfigRtl = emptyPeConfig (SNat @IMemWords) (SNat @DMemWords) d0 d0 True vexRiscv0

sim :: IO ()
sim = do
  peConfig <- peConfigSim
  let
    go (uartRx, _) = ((), uartTx)
     where
      (_, uartTx, _) =
        vexRiscEthernet
          peConfig
          clockGen
          (resetGenN d2)
          (clockToDiffClock clockGen)
          (pure $ unpack 0, uartRx, pure $ unpack 0)
  uartIO @Basic125B stdin stdout baud $ Circuit go

{- | Instance containing:
* VexRiscv CPU
* UART
* Free running timer
* Ethernet MAC
-}
vexRiscGmiiC ::
  forall logic rx tx.
  ( KnownDomain logic
  , KnownDomain rx
  , KnownDomain tx
  , KnownNat (DomainPeriod logic)
  , 1 <= DomainPeriod logic
  , ValidBaud logic Baud
  ) =>
  Clock logic ->
  Reset logic ->
  Clock rx ->
  Reset rx ->
  Clock tx ->
  Reset tx ->
  PeConfig 8 ->
  Circuit
    ( ToConstBwd Mm
    , ( CSignal logic Bit
      , CSignal rx Gmii
      , Jtag logic
      )
    )
    ( CSignal logic Bit
    , CSignal tx Gmii
    )
vexRiscGmiiC sysClk sysRst rxClk rxRst txClk txRst peConfig =
  circuit $ \(mm, (uartTx, gmiiRx, jtag)) -> do
    [ uartBus
      , (mmTime, timeBus)
      , (mmAxiRx, wbAxiRx)
      , (mmAxiTx, wbAxiTx)
      , (mmDna, dnaWb)
      , (mmMac, macWb)
      ] <-
      pe -< (mm, jtag)
    (uartRx, _uartStatus) <- uart -< (uartBus, uartTx)
    _localCounter <- time -< (mmTime, timeBus)
    _dna <- dnaC -< (mmDna, dnaWb)
    macStatIf -< (mmMac, (macWb, macStatus))
    (axiRx0, gmiiTx, macStatus) <- mac -< (axiTx1, gmiiRx)
    axiRx1 <- axiRxPipe -< axiRx0
    axiTx0 <- wbToAxi4StreamTx' -< wbAxiTx
    axiTx1 <- axiTxPipe -< axiTx0
    _rxBufStatus <- wbAxiRxBuffer -< (wbAxiRx, axiRx1)
    ToConst.toBwd todoMM -< mmAxiRx
    ToConst.toBwd todoMM -< mmAxiTx

    idC -< (uartRx, gmiiTx)
 where
  time = withLittleEndian $ wcre $ timeWb Nothing
  dnaC = withLittleEndian $ wcre readDnaPortE2Wb simDna2
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
  pe = withLittleEndian $ wcre processingElement NoDumpVcd peConfig
  wbToAxi4StreamTx' = wcre wbToAxi4StreamTx
  wbAxiRxBuffer = wcre wbAxisRxBufferCircuit (SNat @2048)
  axiTxPipe = wcre (axiUserMapC (const False) <| axiStreamToByteStream)
  axiRxPipe = wcre (axiUserMapC or <| axiStreamFromByteStream)
  miiSel = pure False
  rxClkEna = pure True
  txClkEna = pure True
  wcre :: (((HiddenClockResetEnable logic) => a) -> a)
  wcre = withClockResetEnable sysClk sysRst enableGen

type IMemWords = DivRU (80 * 1024) 4
type DMemWords = DivRU (88 * 1024) 4

vexRiscvEthernetMM :: Mm.MemoryMap
vexRiscvEthernetMM =
  getMMAny
    $ vexRiscGmiiC @Basic125B @Basic125A @Basic125A
      clockGen
      resetGen
      clockGen
      resetGen
      clockGen
      resetGen
      peConfigRtl

vexRiscEthernet ::
  PeConfig 8 ->
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
  )
vexRiscEthernet peConfig sysClk sysRst sgmiiPhyClk (jtagin, uartIn, sgmiiIn) =
  (jtagOut, uartOut, bridgeLvdsOut)
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
  circFn = toSignals $ vexRiscGmiiC sysClk sysRst rxClk rxRst rxClk rxRst peConfig

  ( ((SimOnly _mm, (_, _, jtagOut)))
    , (uartOut, gmiiOut)
    ) = circFn (((), (uartIn, bridgeGmiiRx, jtagin)), ((), ()))

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
