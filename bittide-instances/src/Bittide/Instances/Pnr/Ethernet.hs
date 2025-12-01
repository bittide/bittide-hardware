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
import Protocols
import Protocols.Idle
import Protocols.MemoryMap as Mm
import VexRiscv

import Bittide.Axi4
import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.DoubleBufferedRam
import Bittide.Ethernet.Mac
import Bittide.Instances.Domains
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.ProcessingElement.Util (vecFromElfData, vecFromElfInstr)
import Bittide.SharedTypes (withBittideByteOrder)
import Bittide.Wishbone
import Clash.Class.BitPackC (ByteOrder (BigEndian))

import Project.FilePath (
  CargoBuildType (Release),
  findParentContaining,
  firmwareBinariesDir,
 )
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
#else
type Baud = 921_600
#endif

baud :: SNat Baud
baud = SNat

sim :: IO ()
sim =
  uartIO @Basic125B stdin stdout baud $ Circuit go
 where
  go (uartRx, _) = ((), uartTx)
   where
    (_, uartTx, _, _) =
      vexRiscEthernet
        clockGen
        (resetGenN d2)
        (clockToDiffClock clockGen)
        (pure $ unpack 0, uartRx, pure $ unpack 0)

{- | Instance containing:
* VexRiscv CPU
* UART
* Free running timer
* GPIO
* Ethernet MAC
-}
vexRiscGmiiC ::
  forall logic rx tx gpioWidth.
  ( KnownDomain logic
  , KnownDomain rx
  , KnownDomain tx
  , KnownNat (DomainPeriod logic)
  , 1 <= DomainPeriod logic
  , ValidBaud logic Baud
  ) =>
  SNat gpioWidth ->
  Clock logic ->
  Reset logic ->
  Clock rx ->
  Reset rx ->
  Clock tx ->
  Reset tx ->
  Circuit
    ( ConstBwd Mm
    , ( CSignal logic Bit
      , CSignal rx Gmii
      , Jtag logic
      )
    )
    ( CSignal logic Bit
    , CSignal tx Gmii
    , CSignal logic (BitVector gpioWidth)
    )
vexRiscGmiiC SNat sysClk sysRst rxClk rxRst txClk txRst =
  circuit $ \(mm, (uartTx, gmiiRx, jtag)) -> do
    [ uartBus
      , (mmTime, timeBus)
      , (mmAxiRx, wbAxiRx)
      , (mmAxiTx, wbAxiTx)
      , (mmDna, dnaWb)
      , (mmGpio, gpioWb)
      , (mmMac, macWb)
      ] <-
      pe -< (mm, jtag)
    (uartRx, _uartStatus) <- uart -< (uartBus, uartTx)
    _localCounter <- time -< (mmTime, timeBus)
    _dna <- dnaC -< (mmDna, dnaWb)
    macStatIf -< (mmMac, (macWb, macStatus))
    gpioDf <- idleSource
    gpioOut <- gpio -< (gpioWb, gpioDf)
    constBwd todoMM -< mmGpio
    (axiRx0, gmiiTx, macStatus) <- mac -< (axiTx1, gmiiRx)
    axiRx1 <- axiRxPipe -< axiRx0
    axiTx0 <- wbToAxi4StreamTx' -< wbAxiTx
    axiTx1 <- axiTxPipe -< axiTx0
    _rxBufStatus <- wbAxiRxBuffer -< (wbAxiRx, axiRx1)
    constBwd todoMM -< mmAxiRx
    constBwd todoMM -< mmAxiTx

    idC -< (uartRx, gmiiTx, gpioOut)
 where
  time = withBittideByteOrder $ wcre timeWb
  dnaC = withBittideByteOrder $ wcre readDnaPortE2Wb simDna2
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
  pe = withBittideByteOrder $ wcre processingElement NoDumpVcd peConfig
  wbToAxi4StreamTx' = wcre wbToAxi4StreamTx
  wbAxiRxBuffer = wcre wbAxisRxBufferCircuit (SNat @2048)
  axiTxPipe = wcre (axiUserMapC (const False) <| axiStreamToByteStream)
  axiRxPipe = wcre (axiUserMapC or <| axiStreamFromByteStream)
  gpio = wcre $ registerWbC WishbonePriority (0 :: BitVector gpioWidth)
  miiSel = pure False
  rxClkEna = pure True
  txClkEna = pure True
  wcre :: (((HiddenClockResetEnable logic) => a) -> a)
  wcre = withClockResetEnable sysClk sysRst enableGen

  peConfig
    | clashSimulation = peConfigSim
    | otherwise = peConfigRtl

  peConfigSim = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let
      elfPath = root </> firmwareBinariesDir "riscv32imc" Release </> "smoltcp_client"
    pure
      $ PeConfig
        { cpu = vexRiscv0
        , initI =
            Reloadable
              ( Vec
                  $ unsafePerformIO
                  $ vecFromElfInstr @IMemWords BigEndian elfPath
              )
        , initD =
            Reloadable
              ( Vec
                  $ unsafePerformIO
                  $ vecFromElfData @DMemWords BigEndian elfPath
              )
        , iBusTimeout = d0
        , dBusTimeout = d0
        , includeIlaWb = False
        }

  peConfigRtl =
    PeConfig
      { cpu = vexRiscv0
      , initI = Undefined @IMemWords
      , initD = Undefined @DMemWords
      , iBusTimeout = d0
      , dBusTimeout = d0
      , includeIlaWb = True
      }

type IMemWords = DivRU (280 * 1024) 4
type DMemWords = DivRU (88 * 1024) 4

vexRiscvEthernetMM :: Mm.MemoryMap
vexRiscvEthernetMM =
  getMMAny
    $ vexRiscGmiiC @Basic125B @Basic125A @Basic125A @32
      SNat
      clockGen
      resetGen
      clockGen
      resetGen
      clockGen
      resetGen

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
  circFn = toSignals $ vexRiscGmiiC SNat sysClk sysRst rxClk rxRst rxClk rxRst

  ( ((SimOnly _mm, (_, _, jtagOut)))
    , (uartOut, gmiiOut, gpioOut)
    ) = circFn (((), (uartIn, bridgeGmiiRx, jtagin)), ((), (), ()))

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
