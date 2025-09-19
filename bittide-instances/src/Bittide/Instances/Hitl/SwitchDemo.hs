-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

{- | Switch demo for a Bittide system. In concert with its driver file, this device under
test should demonstrate the predictability of a Bittide system once it has achieved logical
synchronicity.

For more details, see [QBayLogic's presentation](https://docs.google.com/presentation/d/1AGbAJQ1zhTPtrekKnQcthd0TUPyQs-zowQpV1ux4k-Y)
on the topic.
-}
module Bittide.Instances.Hitl.SwitchDemo (
  switchDemoDut,
  switchDemoTest,
  tests,
) where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Bittide.ClockControl
import Bittide.ClockControl.Callisto.Types (CallistoResult (..))
import Bittide.ClockControl.Si539xSpi (ConfigState (Error, Finished), si539xSpi)
import Bittide.ElasticBuffer (
  EbMode (Pass),
  Overflow,
  Underflow,
  resettableXilinxElasticBuffer,
  sticky,
 )
import Bittide.Hitl (
  HitlTestCase (..),
  HitlTestGroup (..),
  paramForHwTargets,
 )
import Bittide.Instances.Common (commonSpiConfig)
import Bittide.Instances.Domains (
  Basic125,
  Bittide,
  Ext125,
  Ext200,
  GthRx,
  GthRxS,
  GthTxS,
 )
import Bittide.Instances.Hitl.Dut.SwitchDemo (circuitFnC)
import Bittide.Instances.Hitl.Setup (LinkCount, allHwTargets, channelNames, clockPaths)
import Bittide.SharedTypes (withBittideByteOrder)
import Bittide.SwitchDemoProcessingElement (SimplePeState (Idle))
import Bittide.Transceiver (transceiverPrbsN)

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.Ila (Depth (..), IlaConfig (..), ila, ilaConfig)
import Clash.Cores.Xilinx.VIO (vioProbe)
import Clash.Cores.Xilinx.Xpm.Cdc (xpmCdcArraySingle, xpmCdcSingle)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Protocols
import System.FilePath ((</>))
import VexRiscv (JtagIn (..), JtagOut (..))
#ifdef SIM_BAUD_RATE
import Clash.Cores.UART.Extra
#endif

import qualified Bittide.Instances.Hitl.Driver.SwitchDemo as D
import qualified Bittide.Transceiver as Transceiver
import qualified Clash.Cores.Xilinx.GTH as Gth
import qualified Protocols.MemoryMap as MM

type FifoSize = 5 -- = 2^5 = 32

{- | Reset logic:

HW:

  1. Wait for SPI
  2. Wait for transceivers handshakes (=> all domains are up after this)
  3. Send local counter for one cycle, connect to switch after (=> in parallel
     with steps 4 and onwards, just wait until the transceiver says it's sampling from the
     transmit data input (@txDatas@))
  4a. Deassert CC CPU reset
  4b. Deassert Bittide domain reset (=> MU CPU, PE)
  5. Wait for stable buffers
  6. Wait for elastic buffer initialization (=> signal we're ready to receive data)

SW (MU):

  1. Wait for all UGNs to be captured
-}
switchDemoDut ::
  "REFCLK" ::: Clock Basic125 ->
  "TEST_RST" ::: Reset Basic125 ->
  "SKYCLK" ::: Clock Ext200 ->
  "GTH_RX_S" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  "JTAG_IN" ::: Signal Bittide JtagIn ->
  "SYNC_IN" ::: Signal Bittide Bit ->
  ( "CC_MEMORYMAP" ::: MM.MM
  , "MU_MEMORYMAP" ::: MM.MM
  , "GTH_TX_S" ::: Gth.SimWires Bittide LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
  , "handshakesDone" ::: Signal Basic125 Bool
  , "FINC_FDEC" ::: Signal Bittide (FINC, FDEC)
  , "spiDone" ::: Signal Basic125 Bool
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  , "JTAG_OUT" ::: Signal Bittide JtagOut
  , "transceiversFailedAfterUp" ::: Signal Basic125 Bool
  , "ALL_STABLE" ::: Signal Basic125 Bool
  , "fifoOverflowsSticky" ::: Signal Basic125 Bool
  , "fifoUnderflowsSticky" ::: Signal Basic125 Bool
  , "UART_TX" ::: Signal Basic125 Bit
  , "SYNC_OUT" ::: Signal Basic125 Bit
  )
switchDemoDut refClk refRst skyClk rxSims rxNs rxPs miso jtagIn syncIn =
  -- Replace 'seqX' with 'hwSeqX' to include ILAs in hardware
  seqX
    (bundle (debugIla, bittidePeIla))
    ( ccMm
    , muMm
    , transceivers.txSims
    , transceivers.txNs
    , transceivers.txPs
    , handshakesDoneFree
    , frequencyAdjustments
    , spiDone
    , spiOut
    , jtagOut
    , transceiversFailedAfterUp
    , allStableFree
    , fifoOverflowsStickyFree
    , fifoUnderflowsStickyFree
    , uartTx
    , syncOut
    )
 where
  debugIla :: Signal Basic125 ()
  debugIla =
    setName @"demoDebugIla"
      ila
      ( ilaConfig
          $ "trigger_fdi_dd"
          :> "capture_fdi_dd"
          -- Important step 1 signals
          :> "dd_spiDone"
          :> "dd_spiErr"
          -- Important step 2 signals
          :> "dd_handshakesDoneFree"
          -- Important step 3 signals
          :> "dd_txStarts"
          -- Important step 4 signals
          -- Important step 5 signals
          :> "dd_allStable"
          -- Important step 6 signals
          :> "dd_ebReadys"
          -- Other
          :> "dd_transceiversFailedAfterUp"
          :> Nil
      )
        { depth = D32768
        }
      refClk
      (unsafeToActiveLow handshakeRstFree)
      captureFlag
      spiDone
      spiErr
      (bundle transceivers.handshakesDoneFree)
      (bundle $ xpmCdcArraySingle bittideClk refClk <$> txStarts)
      (xpmCdcSingle bittideClk refClk allStable)
      (bundle $ xpmCdcArraySingle bittideClk refClk <$> ebReadys)
      transceiversFailedAfterUp

  captureFlag =
    riseEvery
      refClk
      spiRst
      enableGen
      (SNat @(PeriodToCycles Basic125 (Milliseconds 2)))

  -- Step 1, wait for SPI:
  (_, _, spiState, spiOut) =
    withClockResetEnable refClk spiRst enableGen
      $ si539xSpi @Basic125 commonSpiConfig (SNat @(Microseconds 10)) (pure Nothing) miso

  spiDone :: Signal Basic125 Bool
  spiDone = dflipflop refClk $ (== Finished) <$> spiState

  spiErr :: Signal Basic125 Bool
  spiErr = dflipflop refClk $ isErr <$> spiState

  gthAllReset :: Reset Basic125
  gthAllReset = unsafeFromActiveLow spiDone

  spiRst :: Reset Basic125
  spiRst = refRst `orReset` unsafeFromActiveHigh spiErr

  isErr :: ConfigState dom n -> Bool
  isErr (Error _) = True
  isErr _ = False

  transceivers :: Transceiver.Outputs LinkCount Bittide GthRx GthTxS Basic125
  transceivers =
    transceiverPrbsN
      @Bittide
      @GthRx
      @Ext200
      @Basic125
      @GthTxS
      @GthRxS
      Transceiver.defConfig
      Transceiver.Inputs
        { clock = refClk
        , reset = gthAllReset
        , refClock = skyClk
        , channelNames
        , clockPaths
        , rxSims
        , rxNs
        , rxPs
        , txDatas = txDatas
        , txStarts = txStarts
        , rxReadys = ebReadysRx
        }

  bittideClk :: Clock Bittide
  bittideClk = transceivers.txClock

  bittideRst :: Reset Bittide
  bittideRst = transceivers.txReset

  linksSuitableForCc :: Signal Bittide (BitVector LinkCount)
  linksSuitableForCc = fmap pack (bundle transceivers.handshakesDoneTx)

  handshakesDoneFree :: Signal Basic125 Bool
  handshakesDoneFree = and <$> bundle transceivers.handshakesDoneFree

  handshakesDoneTx :: Signal Bittide Bool
  handshakesDoneTx = and <$> bundle transceivers.handshakesDoneTx

  -- Step 3, send local counter for one cycle, connect to switch after:
  txSamplingsDelayed :: Vec 7 (Signal Bittide Bool)
  txSamplingsDelayed =
    register bittideClk handshakeRstTx enableGen False <$> transceivers.txSamplings

  txDatas :: Vec 7 (Signal Bittide (BitVector 64))
  txDatas = mux <$> txSamplingsDelayed <*> switchDataOut <*> repeat (pack <$> localCounter)

  txStarts :: Vec 7 (Signal Bittide Bool)
  txStarts = repeat allStableSticky

  -- Step 4, deassert CC CPU reset, deassert Bittide domain reset:
  handshakeRstFree :: Reset Basic125
  handshakeRstFree = unsafeFromActiveLow handshakesDoneFree

  handshakeRstTx :: Reset Bittide
  handshakeRstTx = unsafeFromActiveLow handshakesDoneTx

  -- Step 5, wait for stable buffers:
  allStable :: Signal Bittide Bool
  allStable = callistoResult.allStable

  allStableSticky :: Signal Bittide Bool
  allStableSticky = sticky bittideClk bittideRst allStable

  allStableFree :: Signal Basic125 Bool
  allStableFree = xpmCdcSingle bittideClk refClk allStableSticky

  -- Step 6, wait for elastic buffer initialization
  --         (=> signal we're ready to receive data):
  ebReset :: Reset Bittide
  ebReset = unsafeFromActiveLow allStableSticky

  ebReadys :: Vec 7 (Signal Bittide Bool)
  ebReadys = map (.==. pure Pass) ebModes

  ebReadysRx :: Vec 7 (Signal GthRx Bool)
  ebReadysRx = xpmCdcArraySingle bittideClk <$> transceivers.rxClocks <*> ebReadys

  -- Connect everything together:
  transceiversFailedAfterUp :: Signal Basic125 Bool
  transceiversFailedAfterUp =
    sticky refClk refRst (isFalling refClk spiRst enableGen False handshakesDoneFree)

  ( (ccMm, muMm, jtagOut, _linkInBwd, _maskBwd, _l4ccBwd, _insBwd, _syncBwd)
    , ( callistoResult
        , switchDataOut
        , localCounter
        , peState
        , peInput
        , peOutput
        , calEntry
        , uartTx
        , syncOut
        )
    ) =
      withBittideByteOrder
        $ toSignals
          ( circuitFnC
              (refClk, handshakeRstFree, enableGen)
              (bittideClk, handshakeRstTx, enableGen)
              transceivers.rxClocks
              (unsafeFromActiveLow <$> transceivers.handshakesDone)
          )
          (
            ( ()
            , ()
            , jtagIn
            , pure 0 -- link in
            , pure maxBound -- enable mask
            , linksSuitableForCc
            , rxDatasEbs
            , syncIn
            )
          ,
            ( pure ()
            , repeat (pure ())
            , pure ()
            , pure ()
            , pure ()
            , pure ()
            , pure ()
            , pure ()
            , pure ()
            )
          )

  peNotIdle :: Signal Bittide Bool
  peNotIdle = (/= Idle) <$> peState
  peNotIdleSticky :: Signal Bittide Bool
  peNotIdleSticky = sticky bittideClk handshakeRstTx peNotIdle
  peNotIdleStickyFree :: Signal Basic125 Bool
  peNotIdleStickyFree = xpmCdcSingle bittideClk refClk peNotIdleSticky

  bittidePeIla :: Signal Basic125 ()
  bittidePeIla =
    setName @"bittidePeIla"
      ila
      ( ilaConfig
          $ "trigger_fdi_pe"
          :> "capture_fdi_pe"
          :> "pe_input"
          :> "pe_state"
          :> "pe_output"
          :> "pe_local_counter"
          :> "pe_active_cal_entry"
          :> "pe_rx_0"
          :> "pe_rx_1"
          :> "pe_rx_2"
          :> "pe_rx_3"
          :> "pe_rx_4"
          :> "pe_rx_5"
          :> "pe_rx_6"
          :> "pe_tx_0"
          :> "pe_tx_1"
          :> "pe_tx_2"
          :> "pe_tx_3"
          :> "pe_tx_4"
          :> "pe_tx_5"
          :> "pe_tx_6"
          :> Nil
      )
        { depth = D4096
        }
      refClk
      peNotIdleStickyFree
      (pure True :: Signal Basic125 Bool)
      (xpmCdcArraySingle bittideClk refClk peInput)
      (pack <$> xpmCdcArraySingle bittideClk refClk peState)
      (xpmCdcArraySingle bittideClk refClk peOutput)
      (xpmCdcArraySingle bittideClk refClk localCounter)
      (xpmCdcArraySingle bittideClk refClk calEntry)
      (xpmCdcArraySingle bittideClk refClk (rxDatasEbs !! (0 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (rxDatasEbs !! (1 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (rxDatasEbs !! (2 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (rxDatasEbs !! (3 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (rxDatasEbs !! (4 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (rxDatasEbs !! (5 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (rxDatasEbs !! (6 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (switchDataOut !! (0 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (switchDataOut !! (1 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (switchDataOut !! (2 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (switchDataOut !! (3 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (switchDataOut !! (4 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (switchDataOut !! (5 :: Index LinkCount)))
      (xpmCdcArraySingle bittideClk refClk (switchDataOut !! (6 :: Index LinkCount)))

  frequencyAdjustments :: Signal Bittide (FINC, FDEC)
  frequencyAdjustments =
    delay bittideClk enableGen minBound
      $ speedChangeToStickyPins
        bittideClk
        bittideRst
        enableGen
        (SNat @Si539xHoldTime)
        callistoResult.maybeSpeedChange

  rxFifos ::
    Vec
      LinkCount
      ( Signal Bittide (RelDataCount FifoSize)
      , Signal Bittide Underflow
      , Signal Bittide Overflow
      , Signal Bittide EbMode
      , Signal Bittide (Maybe (BitVector 64))
      )
  rxFifos = zipWith go transceivers.rxClocks transceivers.rxDatas
   where
    go rxClk rxData = resettableXilinxElasticBuffer bittideClk rxClk ebReset rxData

  fifoUnderflowsTx :: Vec LinkCount (Signal Bittide Underflow)
  fifoOverflowsTx :: Vec LinkCount (Signal Bittide Overflow)
  rxDatasEbs :: Vec LinkCount (Signal Bittide (Maybe (BitVector 64)))
  ebModes :: Vec 7 (Signal Bittide EbMode)
  (_, fifoUnderflowsTx, fifoOverflowsTx, ebModes, rxDatasEbs) = unzip5 rxFifos

  fifoOverflows :: Signal Bittide Overflow
  fifoOverflows = or <$> bundle fifoOverflowsTx

  fifoUnderflows :: Signal Bittide Underflow
  fifoUnderflows = or <$> bundle fifoUnderflowsTx

  fifoOverflowsStickyFree :: Signal Basic125 Bool
  fifoOverflowsStickyFree =
    xpmCdcSingle bittideClk refClk $ sticky bittideClk bittideRst fifoOverflows

  fifoUnderflowsStickyFree :: Signal Basic125 Bool
  fifoUnderflowsStickyFree =
    xpmCdcSingle bittideClk refClk $ sticky bittideClk bittideRst fifoUnderflows

switchDemoTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "GTH_RX_S" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  "JTAG" ::: Signal Bittide JtagIn ->
  "USB_UART_TXD" ::: Signal Basic125 Bit ->
  "SYNC_IN" ::: Signal Bittide Bit ->
  ( "GTH_TX_S" ::: Gth.SimWires Bittide LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
  , ""
      ::: ( "FINC" ::: Signal Bittide Bool
          , "FDEC" ::: Signal Bittide Bool
          )
  , "spiDone" ::: Signal Basic125 Bool
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  , "JTAG" ::: Signal Bittide JtagOut
  , "USB_UART_RXD" ::: Signal Basic125 Bit
  , "SYNC_OUT" ::: Signal Basic125 Bit
  )
switchDemoTest boardClkDiff refClkDiff rxs rxns rxps miso jtagIn _uartRx syncIn =
  -- Replace 'seqX' with 'hwSeqX' to include ILAs in hardware
  seqX
    testIla
    ( txs
    , txns
    , txps
    , unbundle swFincFdecs
    , spiDone
    , spiOut
    , jtagOut
    , uartTx
    , syncOut
    )
 where
  boardClk :: Clock Ext200
  boardClk = Gth.ibufds_gte3 boardClkDiff

  refClk :: Clock Basic125
  refRst :: Reset Basic125
  (refClk, refRst) = clockWizardDifferential refClkDiff noReset

  testStart :: Signal Basic125 Bool
  testStart =
    unbundle
      $ setName @"vioHitlt"
      $ vioProbe
        ("probe_test_done" :> "probe_test_success" :> "probe_handshakes_done" :> Nil)
        ("probe_test_start" :> Nil)
        False
        refClk
        (testStart .&&. testDone) -- done
        (testStart .&&. testSuccess) -- success
        handshakesDone
  testReset :: Reset Basic125
  testReset = unsafeFromActiveLow testStart `orReset` refRst

  ( _ccMm
    , _muMm
    , txs :: Gth.SimWires Bittide LinkCount
    , txns :: Gth.Wires GthTxS LinkCount
    , txps :: Gth.Wires GthTxS LinkCount
    , handshakesDone :: Signal Basic125 Bool
    , swFincFdecs :: Signal Bittide (Bool, Bool)
    , spiDone :: Signal Basic125 Bool
    , spiOut :: (Signal Basic125 Bool, Signal Basic125 Bit, Signal Basic125 Bool)
    , jtagOut :: Signal Bittide JtagOut
    , transceiversFailedAfterUp :: Signal Basic125 Bool
    , allStable :: Signal Basic125 Bool
    , fifoOverflows :: Signal Basic125 Bool
    , fifoUnderflows :: Signal Basic125 Bool
    , uartTx :: Signal Basic125 Bit
    , syncOut :: Signal Basic125 Bit
    ) = switchDemoDut refClk testReset boardClk rxs rxns rxps miso jtagIn syncIn

  fifoSuccess :: Signal Basic125 Bool
  fifoSuccess = not <$> (fifoUnderflows .||. fifoOverflows)

  doneSuccess :: Signal Basic125 Bool
  doneSuccess = allStable .&&. fifoSuccess

  testDone :: Signal Basic125 Bool
  testDone = doneSuccess .||. transceiversFailedAfterUp .||. fmap not fifoSuccess

  testSuccess :: Signal Basic125 Bool
  testSuccess = testDone .&&. fifoSuccess .&&. fmap not transceiversFailedAfterUp

  testIla :: Signal Basic125 ()
  testIla =
    setName @"demoTestIla"
      ila
      ( ilaConfig
          $ "trigger_fdi_dt"
          :> "capture_fdi_dt"
          :> "dt_handshakesDone"
          :> "dt_spiDone"
          :> "dt_spiOut"
          :> "dt_transceiversFailedAfterUp"
          :> "dt_allStable"
          :> "dt_fifoOverflows"
          :> "dt_fifoUnderflows"
          :> Nil
      )
        { depth = D32768
        }
      refClk
      handshakesDone
      captureFlag
      handshakesDone
      spiDone
      (bundle spiOut)
      transceiversFailedAfterUp
      allStable
      fifoOverflows
      fifoUnderflows

  captureFlag :: Signal Basic125 Bool
  captureFlag =
    riseEvery
      refClk
      testReset
      enableGen
      (SNat @(PeriodToCycles Basic125 (Milliseconds 2)))
{-# OPAQUE switchDemoTest #-}
makeTopEntity 'switchDemoTest

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'switchDemoTest
    , extraXdcFiles =
        [ "jtag" </> "config.xdc"
        , "jtag" </> "pmod1.xdc"
        , "uart" </> "pmod1.xdc"
        ]
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "Bittide_Demo_DUT"
            , parameters = paramForHwTargets allHwTargets ()
            , postProcData = ()
            }
        ]
    , mDriverProc = Just D.driver
    , mPostProc = Nothing
    }
