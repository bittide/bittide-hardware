-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Test whether clock boards are configurable and transceiver links come
online. This assumes to run on a fully connected mesh of 8 FPGAs. Also see
'c_CHANNEL_NAMES' and 'c_CLOCK_PATHS'. It has two tricks up its sleeve:

  1. It uses @SYNC_IN@/@SYNC_OUT@ to make sure each board starts programming
     its clock boards at the same time.

  2. It keeps track of how many times the GTH's reset manager had to reset
     the connection and how often it lost connections after establishing
     them.

This test will succeed if all links have been up for ten seconds.
-}
module Bittide.Instances.Hitl.Transceivers where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Bittide.Arithmetic.Time
import Bittide.ClockControl.Si5395J
import Bittide.ClockControl.Si539xSpi
import Bittide.ElasticBuffer (stickyE)
import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup
import Bittide.Transceiver

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.Vio (vioProbe)
import Clash.Cores.Xilinx.Xpm.Cdc.Single (xpmCdcSingle)
import Clash.Xilinx.ClockGen
import Data.Maybe (isJust)
import System.FilePath ((</>))

import qualified Bittide.Instances.Hitl.Driver.Transceivers as Driver
import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Clash.Cores.Xilinx.Gth as Gth
import qualified Clash.Explicit.Prelude as E
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Protocols.Spi as Spi

{- | Start value of the counters used in 'counter' and 'expectCounter'. This is
a non-zero start value, as a regression test for a bug where the transceivers
would not come up if the counters started at zero.
-}
counterStart :: BitVector 64
counterStart = 0xDEAD_BEEF_0000_0000

-- | A counter starting at 'counterStart'
counter ::
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Signal dom (BitVector 64)
counter clk rst = let c = register clk rst enableGen counterStart (c + 1) in c

{- | Expect a counter starting at 'counterStart' and incrementing by one on each
cycle. Starts running when the received value equals 'counterStart'.
-}
expectCounter ::
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  -- | Received data
  Signal dom (BitVector 64) ->
  -- | Error
  Signal dom Bool
expectCounter clk rst = stickyE clk rst . mealy clk rst enableGen go Nothing
 where
  go Nothing e
    | e == counterStart = (Just (e + 1), False)
    | otherwise = (Nothing, False)
  go (Just c) e = (Just (c + 1), c /= e)

-- | Per-link status, for debugging. Read by 'Driver.driver' through a VIO.
data LinkStatus = LinkStatus
  { rxUps :: Vec LinkCount Bool
  , txUps :: Vec LinkCount Bool
  , errors :: Vec LinkCount Bool
  , stats :: Vec LinkCount ResetManager.Statistics
  }

{- | VIO exposing 'LinkStatus' as separate probes. Link /i/ is bit /i/ of the
bit masks, and bits @[32 * i + 31 : 32 * i]@ of the counters. The VIO has no
outputs, so use 'hwSeqX' to keep it in the design.
-}
linkStatusVio :: Clock Basic125 -> Signal Basic125 LinkStatus -> Signal Basic125 ()
linkStatusVio clk status =
  setName @"vioLinkStatus"
    $ vioProbe
      ( "probe_rx_up"
          :> "probe_tx_up"
          :> "probe_errors"
          :> "probe_fail_after_ups"
          :> "probe_rx_full_retries"
          :> "probe_tx_retries"
          :> Nil
      )
      Nil
      ()
      clk
      (mask (.rxUps) <$> status)
      (mask (.txUps) <$> status)
      (mask (.errors) <$> status)
      (counters (.failAfterUps) <$> status)
      (counters (.rxFullRetries) <$> status)
      (counters (.txRetries) <$> status)
 where
  mask :: (LinkStatus -> Vec LinkCount Bool) -> LinkStatus -> BitVector LinkCount
  mask f = pack . reverse . f

  counters ::
    (ResetManager.Statistics -> Unsigned 32) -> LinkStatus -> BitVector (LinkCount * 32)
  counters f = pack . reverse . fmap f . (.stats)

{- | Worker function for 'transceiversUpTest'. See module documentation for more
information.
-}
goTransceiversUpTest ::
  "SMA_MGT_REFCLK_C" ::: Clock Ext200 ->
  "SYSCLK" ::: Clock Basic125 ->
  "RST_LOCAL" ::: Reset Basic125 ->
  "GTH_RX_N" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  Signal Basic125 Spi.S2M ->
  ( "GTH_TX_S" ::: Gth.SimWires GthTx LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
  , "allUp" ::: Signal Basic125 Bool
  , "anyErrors" ::: Signal Basic125 Bool
  , "linkStatus" ::: Signal Basic125 LinkStatus
  , "stats" ::: Vec LinkCount (Signal Basic125 ResetManager.Statistics)
  , "spiDone" ::: Signal Basic125 Bool
  , "" ::: Signal Basic125 Spi.M2S
  )
goTransceiversUpTest refClk sysClk rst rxs rxNs rxPs spiS2M =
  ( transceivers.txSims
  , transceivers.txNs
  , transceivers.txPs
  , allUp
  , expectCounterErrorSys
  , linkStatus
  , transceivers.stats
  , spiDone
  , spiM2S
  )
 where
  allUp =
    fmap and (bundle transceivers.rxDataInitDonesFree)
      .&&. fmap and (bundle transceivers.txDataInitDonesFree)

  sysRst = orReset rst (unsafeFromActiveLow (fmap not spiErr))

  -- Clock programming
  spiDone = E.dflipflop sysClk $ (== Finished) <$> spiState
  spiErr = E.dflipflop sysClk $ isErr <$> spiState

  isErr (Error _) = True
  isErr _ = False

  (_, _, spiState, spiM2S) =
    withClockResetEnable sysClk sysRst enableGen
      $ si539xSpi
        testConfig6_200_on_0a_1ppb
        (SNat @(Microseconds 10))
        (pure Nothing)
        spiS2M

  -- Transceiver setup
  gthAllReset = unsafeFromActiveLow spiDone

  txCounters =
    counter transceivers.txClock . unsafeFromActiveLow <$> transceivers.txDataInitDones

  rxDataResets = unsafeFromActiveLow <$> transceivers.rxDataInitDones

  expectCounterError =
    zipWith3
      expectCounter
      transceivers.rxClocks
      (zipWith orReset transceivers.rxResets rxDataResets)
      transceivers.rxDatas

  expectCounterErrorsSys =
    zipWith (`xpmCdcSingle` sysClk) transceivers.rxClocks expectCounterError

  expectCounterErrorSys = fmap or $ bundle expectCounterErrorsSys

  linkStatus =
    LinkStatus
      <$> bundle transceivers.rxDataInitDonesFree
      <*> bundle transceivers.txDataInitDonesFree
      <*> bundle expectCounterErrorsSys
      <*> bundle transceivers.stats

  transceivers =
    transceiverPrbsN
      @GthTx
      @GthRx
      @Ext200
      @Basic125
      @GthTxS
      @GthRxS
      defConfig
      Inputs
        { clock = sysClk
        , reset = gthAllReset
        , refClock = refClk
        , channelNames
        , clockPaths
        , rxSims = rxs
        , rxNs
        , rxPs
        , channelResets = repeat noReset
        , txDatas = txCounters
        }

{- | SFP control signals of the Trenz TEF0008 FMC card, which holds the SFP+
cages of links J4 to J7. The card's MAX10 forwards these to all four cages. When
left undriven, the FPGA's default pull-down on unused pins fights the MAX10's
weak pull-up, which can disable the transmitters once the card is powered (VADJ
on). See https://wiki.trenz-electronic.de/display/PD/TEF0008+MAX10.
-}
type Tef0008SfpControl =
  ( "FMC_HPC_LA03_P" ::: Signal Basic125 Bit
  , "FMC_HPC_LA03_N" ::: Signal Basic125 Bit
  , "FMC_HPC_LA04_P" ::: Signal Basic125 Bit
  )

{- | Transmitters on (TX_DISABLE low), and the full rate setting on both rate
select pins (RS0 and RS1 high), as our links run at 10 Gbps. All three
signals are inverted by the MAX10.
-}
tef0008SfpControl :: Tef0008SfpControl
tef0008SfpControl = (pure high, pure low, pure low)

-- | Top entity for this test. See module documentation for more information.
transceiversUpTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "GTH_RX_S" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  Signal Basic125 Spi.S2M ->
  ( "GTH_TX_S" ::: Gth.SimWires GthTx LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
  , "SYNC_OUT" ::: Signal Basic125 Bool
  , "" ::: Signal Basic125 Spi.M2S
  , "" ::: Tef0008SfpControl
  )
transceiversUpTest refClkDiff sysClkDiff syncIn rxs rxns rxps spiS2M =
  linkStatusVio sysClk linkStatus `hwSeqX` (txs, txns, txps, syncOut, spiM2S, tef0008SfpControl)
 where
  (refClk, _) = Gth.ibufds_gte3 refClkDiff

  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset

  testRst = sysRst `orReset` unsafeFromActiveLow startTest `orReset` syncInRst
  syncOut = startTest
  syncInRst =
    resetGlitchFilter (SNat @1024) sysClk
      $ unsafeFromActiveLow
      $ xpmCdcSingle sysClk sysClk syncIn

  (txs, txns, txps, allUp, anyErrors, linkStatus, _stats, _spiDone, spiM2S) =
    goTransceiversUpTest refClk sysClk testRst rxs rxns rxps spiS2M

  failAfterUp = isFalling sysClk testRst enableGen False allUp
  failAfterUpSticky = stickyE sysClk testRst failAfterUp

  startTest = isJust <$> maybeFpgaIndex

  maybeFpgaIndex :: Signal Basic125 (Maybe (Index FpgaCount))
  maybeFpgaIndex =
    hitlVio
      0
      sysClk
      -- Consider test done if links have been up consistently for 40 seconds. This
      -- is just below the test timeout of 60 seconds, so transceivers have ~20
      -- seconds to come online reliably. This should be plenty.
      (trueFor (SNat @(Seconds 40)) sysClk testRst allUp .||. failAfterUpSticky .||. anyErrors)
      -- Success?
      (fmap not failAfterUpSticky .&&. fmap not anyErrors)

makeTopEntity 'transceiversUpTest

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'transceiversUpTest
    , externalHdl = []
    , targetXdcs =
        [ "wireDemoTest.xdc"
        , "si539x" </> "spi.xdc"
        , "tef0008" </> "sfpControl.xdc"
        ]
    , testCases = iters
    , mDriverProc = Just Driver.driver
    , mPostProc = Nothing
    }
 where
  fpgaIndices = [0 ..] :: [Index FpgaCount]
  nIters = 1
  iterNames = ["I" <> show n | n <- [(0 :: Int) .. nIters - 1]]
  iters =
    [ HitlTestCase
        { name = nm
        , parameters =
            Map.fromList (L.zip (HwTargetByIndex . fromIntegral <$> fpgaIndices) fpgaIndices)
        , postProcData = ()
        }
    | nm <- iterNames
    ]
