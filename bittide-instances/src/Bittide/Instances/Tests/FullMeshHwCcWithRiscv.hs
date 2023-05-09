-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=6 #-}

-- | Tests the same thing as "Bittide.Instances.Tests.FullMeshHwCc" but uses
-- a 'processingElement' to drive clock control.
module Bittide.Instances.Tests.FullMeshHwCcWithRiscv where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Clash.Cores.Xilinx.Extra (ibufds_gte3)
import Clash.Cores.Xilinx.VIO (vioProbe)
import Clash.Cores.Xilinx.Ila
import Clash.Cores.Xilinx.Xpm.Cdc.Single (xpmCdcSingle)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Bittide.Instances.MVPs(speedChangeToPins)
import Bittide.Instances.Tests.FullMeshHwCc
import Bittide.Arithmetic.Time
import Bittide.ClockControl
import Bittide.ClockControl.Callisto
import Bittide.ClockControl.Registers
import Bittide.DoubleBufferedRam
  ( registerWb, RegisterWritePriority (CircuitPriority), InitialContent(..)
  , ContentType(..) )
import Bittide.Instances.Domains
import Bittide.Instances.Tests.FullMeshHwCc (TransceiverWires, trueFor5s, goFullMeshHwCcTest)
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util (memBlobsFromElf)
import Bittide.SharedTypes
import Bittide.Transceiver
import Protocols
import Protocols.Wishbone
import Protocols.Internal
import Bittide.Extra.Maybe
import Clash.Cores.Xilinx.Xpm.Cdc.Handshake.Extra

import Language.Haskell.TH (runIO)
import System.Directory
import System.FilePath

unitCS :: CSignal dom ()
unitCS = CSignal (pure ())


-- | Worker function of 'fullMeshHwCcWithRiscvTest'. See module documentation
-- for more information.
clockControlRegistersInner ::
  "SMA_MGT_REFCLK_C" ::: Clock Basic200 ->
  "SYSCLK" ::: Clock Basic125 ->
  "RST_LOCAL" ::: Reset Basic125 ->
  "GTH_RX_NS" ::: TransceiverWires GthRx ->
  "GTH_RX_PS" ::: TransceiverWires GthRx ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTx
  , "GTH_TX_PS" ::: TransceiverWires GthTx
  , "FINC_FDEC" ::: Signal GthTx (FINC, FDEC)
  , "CALLISTO_CLOCK" ::: Clock GthTx
  , "CALLISTO_RESULT" ::: Signal GthTx (CallistoResult 7)
  , "CALLISTO_RESET" ::: Reset GthTx
  , "DATA_COUNTERS" ::: Vec 7 (Signal GthTx (DataCount 32))
  , "stats" ::: Vec 7 (Signal Basic125 GthResetStats)
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK" ::: Signal Basic125 Bool
      , "MOSI" ::: Signal Basic125 Bit
      , "CSB"  ::: Signal Basic125 Bool
      )
  , "transceiversFailedAfterUp" ::: Signal Basic125 Bool
  , "ALL_STABLE"   ::: Signal Basic125 Bool
  , "linkUps" ::: Vec 7 (Signal Basic125 Bool)
  )
goFullMeshHwCcWithRiscvTest clk rst callistoResult dataCounts = unbundle fIncDec
 where
  (_, CSignal fIncDec) = toSignals
    ( circuit $ \ unit -> do
      [wbA, wbB] <- (withClockResetEnable clk rst enableGen $ processingElement @dom peConfig) -< unit
      fIncDecCallisto -< wbA
      fIncDec <- withClockResetEnable clk rst enableGen $
        clockControlWb margin framesize (pure $ complement 0) dataCounts -< wbB
      idC -< fIncDec
    ) ((), unitCS)

  fIncDecCallisto ::
    forall aw nBytes .
    (KnownNat aw, 2 <= aw, nBytes ~ 4) =>
    Circuit
      (Wishbone dom 'Standard aw (Bytes nBytes))
      ()
  fIncDecCallisto = Circuit goFIncDecCallisto
   where
    goFIncDecCallisto (wbM2S, _) = (wbS2M, ())
     where
      (_, wbS2M) = withClockResetEnable clk rst enableGen $
        registerWb
          CircuitPriority
          (0 :: Signed 32, 0 :: Bytes nBytes)
          wbM2S
          (fmap (Just . (,0)) fincfdecCount)
      fincfdecCount = countfIncfDecs clk rst enableGen $ unbundle $ speedChangeToPins . speedChange <$> callistoResult

  margin = d2

  framesize = SNat @(PeriodToCycles dom (Seconds 1))

  (   (_iStart, _iSize, iMem)
    , (_dStart, _dSize, dMem)) = $(do

    let
      findProjectRoot :: IO FilePath
      findProjectRoot = goUp =<< getCurrentDirectory
        where
          goUp :: FilePath -> IO FilePath
          goUp path
            | isDrive path = error "Could not find 'cabal.project'"
            | otherwise = do
                exists <- doesFileExist (path </> projectFilename)
                if exists then
                  return path
                else
                  goUp (takeDirectory path)

          projectFilename = "cabal.project"

    root <- runIO findProjectRoot

    let elfPath = root </> "_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf/release/clock-control"

    memBlobsFromElf BigEndian elfPath Nothing)

  {-
    0b10xxxxx_xxxxxxxx 0b10 0x8x instruction memory
    0b01xxxxx_xxxxxxxx 0b01 0x4x data memory
    0b00xxxxx_xxxxxxxx 0b00 0x0x FINC/FDEC register
    0b11xxxxx_xxxxxxxx 0b11 0xCx memory mapped hardware clock control
  -}
  peConfig =
    PeConfig
      (0b10 :> 0b01 :> 0b00 :> 0b11 :> Nil)
      (Reloadable $ Blob iMem)
      (Reloadable $ Blob dMem)

-- | Top entity for this test. See module documentation for more information.
fullMeshHwCcWithRiscvTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Basic200 ->
  "SYSCLK_300" ::: DiffClock Basic300 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "GTH_RX_NS" ::: TransceiverWires GthRx ->
  "GTH_RX_PS" ::: TransceiverWires GthRx ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_NS" ::: TransceiverWires GthTx
  , "GTH_TX_PS" ::: TransceiverWires GthTx
  , "" :::
      ( "FINC"      ::: Signal GthTx Bool
      , "FDEC"      ::: Signal GthTx Bool
      )
  , "SYNC_OUT" ::: Signal Basic125 Bool
  , "spiDone" ::: Signal Basic125 Bool
  , "" :::
      ( "SCLK"      ::: Signal Basic125 Bool
      , "MOSI"      ::: Signal Basic125 Bit
      , "CSB"       ::: Signal Basic125 Bool
      )
  , "linkUps"       ::: Vec 7 (Signal Basic125 Bool)
  )
fullMeshHwCcWithRiscvTest refClkDiff sysClkDiff syncIn rxns rxps miso = fincFdecIla `hwSeqX`
  (txns, txps, fincfdecOut, syncOut, spiDone, spiOut, linkUps)
 where
  refClk = ibufds_gte3 refClkDiff :: Clock Basic200

  (sysClk, sysLock0) = clockWizardDifferential (SSymbol @"SysClk") sysClkDiff noReset
  sysLock1 = xpmCdcSingle sysClk sysClk sysLock0 -- improvised reset syncer
  sysRst = unsafeFromActiveLow sysLock1
  testRst = sysRst `orReset` unsafeFromActiveLow syncOut `orReset` syncInRst
  syncOut =  or <$> startTests
  syncInRst =
      resetGlitchFilter (SNat @1024) sysClk
    $ unsafeFromActiveLow
    $ xpmCdcSingle sysClk sysClk syncIn

  ( txns
   , txps
   , _
   , callistoClock
   , callistoResult
   , callistoReset
   , dataCounts
   , stats
   , spiDone
   , spiOut
   , _
   , allStable1
   , linkUps
   ) =
    goFullMeshHwCcTest refClk sysClk testRst rxns rxps miso

  fincFdecs = clockControlRegistersInner callistoClock callistoReset callistoResult dataCounts

  fincFdecIla :: Signal Basic125 ()
  fincFdecIla = ila
    (ilaConfig $
         "trigger"
      :> "capture"
      :> "softfIncfDecCount"
      :> "hardfIncfDecCount"
      :> "dataCount_0"
      :> "dataCount_1"
      :> "dataCount_2"
      :> "dataCount_3"
      :> "dataCount_4"
      :> "dataCount_5"
      :> "dataCount_6"
      :> Nil
    ){depth = D65536}
    sysClk

    -- Trigger as soon as we come out of reset
    (unsafeToActiveLow sysRst)

    -- Capture on interesting on SCLK or whenever we're in reset to fill the
    -- buffers. If we don't do the latter, the VCDs get very confusing.
    (isEdge softFinc .||. isEdge softFdec)

    -- Debug probes
    (countfIncfDecs @_ @32 sysClk sysRst enableGen (softFinc, softFdec))
    (countfIncfDecs @_ @32 sysClk sysRst enableGen (hardFinc, hardFdec))
    dataCounts0
    dataCounts1
    dataCounts2
    dataCounts3
    dataCounts4
    dataCounts5
    dataCounts6

  ( dataCounts0
    :> dataCounts1
    :> dataCounts2
    :> dataCounts3
    :> dataCounts4
    :> dataCounts5
    :> dataCounts6
    :> Nil) = fmap dataCountCdc dataCounts

  dataCountCdc = regMaybe sysClk sysRst enableGen 0 . xpmCdcMaybeLossy callistoClock sysClk . fmap Just
  isEdge b = isRising sysClk sysRst enableGen False b .||. isFalling sysClk sysRst enableGen False b

  softFinc = xpmCdcSingle callistoClock sysClk $ fst fincFdecs
  softFdec = xpmCdcSingle callistoClock sysClk $ snd fincFdecs
  hardFinc = xpmCdcSingle callistoClock sysClk $ fst callistoFincFdec
  hardFdec = xpmCdcSingle callistoClock sysClk $ snd callistoFincFdec

  callistoFincFdec = unbundle $ speedChangeToPins . speedChange <$> callistoResult
  callistoResetBool = unsafeToActiveHigh callistoReset

  stats0 :> stats1 :> stats2 :> stats3 :> stats4 :> stats5 :> stats6 :> Nil = stats

  testDone = trueFor5s sysClk testRst $ mux (head <$> startTests) noControlDone allStable1

  noControlDone = xpmCdcSingle callistoClock sysClk $ unsafeToActiveLow callistoReset

  fincfdecOut = unbundle $ dflipflop callistoClock $
    fromMaybesL (False, False) <$>
    (zipWith orNothing <$> startTestsTx <*> bundle (pure (False, False) :> bundle callistoFincFdec :> bundle fincFdecs  :> Nil ))

  startTestsTx = bundle $ fmap (xpmCdcSingle sysClk callistoClock) (unbundle startTests)
  startTests :: Signal Basic125 (Vec 3 Bool)
  startTests =
    setName @"vioHitlt" $
    vioProbe
      (  "probe_test_done"
      :> "probe_test_success"

      -- Debug probes
      :> "stats0_txRetries"
      :> "stats0_rxRetries"
      :> "stats0_rxFullRetries"
      :> "stats0_failAfterUps"
      :> "stats1_txRetries"
      :> "stats1_rxRetries"
      :> "stats1_rxFullRetries"
      :> "stats1_failAfterUps"
      :> "stats2_txRetries"
      :> "stats2_rxRetries"
      :> "stats2_rxFullRetries"
      :> "stats2_failAfterUps"
      :> "stats3_txRetries"
      :> "stats3_rxRetries"
      :> "stats3_rxFullRetries"
      :> "stats3_failAfterUps"
      :> "stats4_txRetries"
      :> "stats4_rxRetries"
      :> "stats4_rxFullRetries"
      :> "stats4_failAfterUps"
      :> "stats5_txRetries"
      :> "stats5_rxRetries"
      :> "stats5_rxFullRetries"
      :> "stats5_failAfterUps"
      :> "stats6_txRetries"
      :> "stats6_rxRetries"
      :> "stats6_rxFullRetries"
      :> "stats6_failAfterUps"

      :> "callistoResetBool"

      :> Nil)
      (  "probe_test_start_no_control"
      :> "probe_test_start_hard_control"
      :> "probe_test_start_soft_control"
      :> Nil)
      (repeat False)
      sysClk

      -- Consider test done if clock control has been stable consistently for 5 s
      testDone

      -- This test either succeeds or times out, so success is set to a static
      -- 'True'. If you want to see statistics, consider setting it to 'False' -
      -- it will make the test TCL print out all probe values.
      (pure True :: Signal Basic125 Bool)

      -- Debug probes
      (txRetries     <$> stats0)
      (rxRetries     <$> stats0)
      (rxFullRetries <$> stats0)
      (failAfterUps  <$> stats0)
      (txRetries     <$> stats1)
      (rxRetries     <$> stats1)
      (rxFullRetries <$> stats1)
      (failAfterUps  <$> stats1)
      (txRetries     <$> stats2)
      (rxRetries     <$> stats2)
      (rxFullRetries <$> stats2)
      (failAfterUps  <$> stats2)
      (txRetries     <$> stats3)
      (rxRetries     <$> stats3)
      (rxFullRetries <$> stats3)
      (failAfterUps  <$> stats3)
      (txRetries     <$> stats4)
      (rxRetries     <$> stats4)
      (rxFullRetries <$> stats4)
      (failAfterUps  <$> stats4)
      (txRetries     <$> stats5)
      (rxRetries     <$> stats5)
      (rxFullRetries <$> stats5)
      (failAfterUps  <$> stats5)
      (txRetries     <$> stats6)
      (rxRetries     <$> stats6)
      (rxFullRetries <$> stats6)
      (failAfterUps  <$> stats6)

      (xpmCdcSingle callistoClock sysClk callistoResetBool)

-- | Count the number of FINC / FDEC Pulses, FINC => +1 , FDEC => -1.
countfIncfDecs ::
  (KnownDomain dom, KnownNat n) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  (Signal dom Bool, Signal dom Bool) ->
  Signal dom (Signed n)
countfIncfDecs clk rst ena (fInc, fDec) = cnt
 where
  cntCond = isRising clk rst ena False fInc .||. isRising clk rst ena False fDec
  cnt = regEn clk rst ena 0 cntCond cnt'
  cnt' = satAdd SatError <$> cnt <*> mux fInc 1 (-1)

-- XXX: We use an explicit top entity annotation here, as 'makeTopEntity'
--      generates warnings in combination with 'Vec'.
{-# ANN fullMeshHwCcWithRiscvTest Synthesize
  { t_name = "fullMeshHwCcWithRiscvTest"
  , t_inputs =
    [ (PortProduct "SMA_MGT_REFCLK_C") [PortName "p", PortName "n"]
    , (PortProduct "SYSCLK_300") [PortName "p", PortName "n"]
    , PortName "SYNC_IN"
    , PortName "GTH_RX_NS"
    , PortName "GTH_RX_PS"
    , PortName "MISO"
    ]
  , t_output =
    (PortProduct "")
      [ PortName "GTH_TX_NS"
      , PortName "GTH_TX_PS"
      , PortProduct "" [PortName "FINC", PortName "FDEC"]
      , PortName "SYNC_OUT"
      , PortName "spiDone"
      , (PortProduct "") [PortName "SCLK", PortName "MOSI", PortName "CSB"]
      , PortName "linkUps"
      ]
  } #-}
