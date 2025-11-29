-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Switch demo for a Bittide system. In concert with its driver file, this device under
test should demonstrate the predictability of a Bittide system once it has achieved logical
synchronicity.

For more details, see [QBayLogic's presentation](https://docs.google.com/presentation/d/1AGbAJQ1zhTPtrekKnQcthd0TUPyQs-zowQpV1ux4k-Y)
on the topic.
-}
module Bittide.Instances.Hitl.SoftUgnDemo (
  softUgnDemoDutC,
  softUgnDemoTest,
  tests,
) where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Protocols

import Bittide.ClockControl
import Bittide.ClockControl.Callisto.Types (CallistoResult (..))
import Bittide.ClockControl.Si539xSpi (ConfigState (Error, Finished), si539xSpiC)
import Bittide.ElasticBuffer (
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
import Bittide.Instances.Hitl.Dut.SoftUgnDemo (softUgnDemoC)
import Bittide.Instances.Hitl.Setup (
  LinkCount,
  allHwTargets,
  channelNames,
  clockPaths,
 )
import Bittide.SharedTypes (withBittideByteOrder)
import Bittide.Sync (Sync)

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.VIO (vioProbe)
import Clash.Cores.Xilinx.Xpm.Cdc (xpmCdcArraySingle, xpmCdcSingle)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Protocols.MemoryMap (ignoreMM)
import Protocols.Spi (Spi)
import qualified Protocols.Spi as Spi
import System.FilePath ((</>))
import VexRiscv (Jtag, JtagIn (..), JtagOut (..))

import qualified Bittide.Instances.Hitl.Driver.SoftUgnDemo as D
import qualified Bittide.Transceiver as Transceiver
import qualified Clash.Cores.Xilinx.Gth as Gth

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
softUgnDemoDutC ::
  "REFCLK" ::: Clock Basic125 ->
  "TEST_RST" ::: Reset Basic125 ->
  Circuit
    ( Jtag Bittide
    , Gth.Gths GthRx GthRxS Bittide GthTxS Ext200 LinkCount
    )
    ( Spi Basic125
    , Sync Bittide Basic125
    , "UART_TX" ::: CSignal Basic125 Bit
    , "handshakesDone" ::: CSignal Basic125 Bool
    , "FINC_FDEC" ::: CSignal Bittide (FINC, FDEC)
    , "spiDone" ::: CSignal Basic125 Bool
    , "transceiversFailedAfterUp" ::: CSignal Basic125 Bool
    , "ALL_STABLE" ::: CSignal Basic125 Bool
    )
softUgnDemoDutC refClk refRst = circuit $ \(jtag, gths) -> do
  (_spiReadBytes, _spiBusy, Fwd spiState, spi) <-
    withClockResetEnable refClk spiRst enableGen
      $ si539xSpiC @Basic125
        commonSpiConfig
        (SNat @(Microseconds 10))
      -< (Fwd (pure Nothing))

  Fwd tOutputs <-
    Transceiver.transceiverPrbsNC @Bittide @GthRx @Ext200 @Basic125 @GthTxS @GthRxS
      refClk
      gthAllReset
      Transceiver.defConfig
      -< (Fwd tInputs, gths)

  (Fwd callistoResult, Fwd switchDataOut, Fwd localCounter, uartTx, Fwd ebStables, sync) <-
    withBittideByteOrder
      $ softUgnDemoC
        (refClk, handshakeRstFree, enableGen)
        (bittideClk, handshakeRstTx, enableGen)
        tOutputs.rxClocks
        (unsafeFromActiveLow <$> tOutputs.handshakesDone)
      -< ( muMm
         , ccMm
         , gppeMm
         , jtag
         , Fwd (pure maxBound)
         , Fwd linksSuitableForCc
         , Fwd tOutputs.rxDatas
         )

  ccMm <- ignoreMM
  muMm <- ignoreMM
  gppeMm <- ignoreMM

  let
    bittideClk :: Clock Bittide
    bittideClk = tOutputs.txClock

    bittideRst :: Reset Bittide
    bittideRst = tOutputs.txReset

    linksSuitableForCc :: Signal Bittide (BitVector LinkCount)
    linksSuitableForCc = fmap pack (bundle tOutputs.handshakesDoneTx)

    handshakesDoneFree :: Signal Basic125 Bool
    handshakesDoneFree = and <$> bundle tOutputs.handshakesDoneFree

    handshakesDoneTx :: Signal Bittide Bool
    handshakesDoneTx = and <$> bundle tOutputs.handshakesDoneTx

    -- Step 3, send local counter for one cycle, connect to switch after:
    txSamplingsDelayed :: Vec 7 (Signal Bittide Bool)
    txSamplingsDelayed =
      register bittideClk handshakeRstTx enableGen False <$> tOutputs.txSamplings

    txDatas :: Vec 7 (Signal Bittide (BitVector 64))
    txDatas = mux <$> txSamplingsDelayed <*> switchDataOut <*> repeat (pack <$> localCounter)

    txStarts :: Vec 7 (Signal Bittide Bool)
    txStarts = ebStables

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
    rxReadys :: Vec 7 (Signal GthRx Bool)
    rxReadys = xpmCdcArraySingle bittideClk <$> tOutputs.rxClocks <*> ebStables

    -- Connect everything together:
    transceiversFailedAfterUp :: Signal Basic125 Bool
    transceiversFailedAfterUp =
      sticky refClk refRst (isFalling refClk spiRst enableGen False handshakesDoneFree)

    frequencyAdjustments :: Signal Bittide (FINC, FDEC)
    frequencyAdjustments =
      delay bittideClk enableGen minBound
        $ speedChangeToStickyPins
          bittideClk
          bittideRst
          enableGen
          (SNat @Si539xHoldTime)
          callistoResult.maybeSpeedChange

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

    tInputs =
      Transceiver.CInputs
        { channelResets = repeat noReset
        , txDatas
        , txStarts
        , rxReadys
        }

  idC
    -< ( spi
       , sync
       , uartTx
       , Fwd handshakesDoneFree
       , Fwd frequencyAdjustments
       , Fwd spiDone
       , Fwd transceiversFailedAfterUp
       , Fwd allStableFree
       )

softUgnDemoTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "GTH_RX_S" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  Signal Basic125 Spi.S2M ->
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
  , "" ::: Signal Basic125 Spi.M2S
  , "JTAG" ::: Signal Bittide JtagOut
  , "USB_UART_RXD" ::: Signal Basic125 Bit
  , "SYNC_OUT" ::: Signal Basic125 Bit
  )
softUgnDemoTest boardClkDiff refClkDiff rxs rxns rxps spiS2M jtagIn _uartRx syncIn =
  ( txs
  , txns
  , txps
  , unbundle swFincFdecs
  , spiDone
  , spiM2S
  , jtagOut
  , uartTx
  , syncOut
  )
 where
  boardClk :: Clock Ext200
  (boardClk, _) = Gth.ibufds_gte3 boardClkDiff

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

  ( (jtagOut, (txs, txns, txps))
    , ( spiM2S
        , syncOut
        , uartTx
        , handshakesDone
        , swFincFdecs
        , spiDone
        , transceiversFailedAfterUp
        , allStable
        )
    ) =
      toSignals
        (softUgnDemoDutC refClk testReset)
        ( (jtagIn, (boardClk, rxs, rxns, rxps, channelNames, clockPaths))
        , (spiS2M, syncIn, (), (), (), (), (), ())
        )

  doneSuccess :: Signal Basic125 Bool
  doneSuccess = allStable

  testDone :: Signal Basic125 Bool
  testDone = doneSuccess .||. transceiversFailedAfterUp

  testSuccess :: Signal Basic125 Bool
  testSuccess = testDone .&&. fmap not transceiversFailedAfterUp
{-# OPAQUE softUgnDemoTest #-}
makeTopEntity 'softUgnDemoTest

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'softUgnDemoTest
    , targetXdcs =
        [ "switchDemoTest.xdc"
        , "jtag" </> "config.xdc"
        , "jtag" </> "pmod1.xdc"
        , "uart" </> "pmod1.xdc"
        , "si539x" </> "fincfdec.xdc"
        , "si539x" </> "spi.xdc"
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
