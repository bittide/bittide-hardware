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
import Bittide.ElasticBuffer (sticky)
import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup
import Bittide.Transceiver

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.Xpm.Cdc.Single (xpmCdcSingle)
import Clash.Xilinx.ClockGen
import Data.Maybe (fromMaybe, isJust)

import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Clash.Cores.Xilinx.GTH as Gth
import qualified Clash.Explicit.Prelude as E
import qualified Data.List as L
import qualified Data.Map as Map

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
cycle.
-}
expectCounter ::
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  -- | Received data
  Signal dom (Maybe (BitVector 64)) ->
  -- | Error
  Signal dom Bool
expectCounter clk rst = sticky clk rst . mealy clk rst enableGen go counterStart
 where
  go c (Just e) = (c + 1, c /= e)
  go c Nothing = (c, False)

{- | Worker function for 'transceiversUpTest'. See module documentation for more
information.
-}
goTransceiversUpTest ::
  Signal Basic125 (Index FpgaCount) ->
  "SMA_MGT_REFCLK_C" ::: Clock Ext200 ->
  "SYSCLK" ::: Clock Basic125 ->
  "RST_LOCAL" ::: Reset Basic125 ->
  "GTH_RX_N" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_S" ::: Gth.SimWires GthTx LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
  , "allUp" ::: Signal Basic125 Bool
  , "anyErrors" ::: Signal Basic125 Bool
  , "stats" ::: Vec LinkCount (Signal Basic125 ResetManager.Statistics)
  , "spiDone" ::: Signal Basic125 Bool
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  )
goTransceiversUpTest fpgaIndex refClk sysClk rst rxs rxNs rxPs miso =
  ( transceivers.txSims
  , transceivers.txNs
  , transceivers.txPs
  , allUp
  , expectCounterErrorSys
  , transceivers.stats
  , spiDone
  , spiOut
  )
 where
  allUp = and <$> bundle transceivers.linkUps

  sysRst = orReset rst (unsafeFromActiveLow (fmap not spiErr))

  -- Clock programming
  spiDone = E.dflipflop sysClk $ (== Finished) <$> spiState
  spiErr = E.dflipflop sysClk $ isErr <$> spiState

  isErr (Error _) = True
  isErr _ = False

  (_, _, spiState, spiOut) =
    withClockResetEnable sysClk sysRst enableGen
      $ si539xSpi testConfig6_200_on_0a_1ppb (SNat @(Microseconds 10)) (pure Nothing) miso

  -- Transceiver setup
  gthAllReset = unsafeFromActiveLow spiDone

  txCounter =
    counter
      transceivers.txClock
      -- We use a single counter for all transmit channels and we start them all
      -- at the same time. This way, the other side will always receive 'counterStart'
      -- as the first value and can verify all samples after that by incrementing
      -- by one each cycle. Note that it doesn't really matter whether we pick
      -- (.||.) or (.&&.) here, as all links should start transmitting at the
      -- same time. I.e., txSamplings should all flip at the same time. Still, we
      -- picked (.||.) to catch situations where some links start transmitting
      -- earlier than others.
      (unsafeFromActiveLow (fold (.||.) transceivers.txSamplings))

  expectCounterError =
    zipWith3
      expectCounter
      transceivers.rxClocks
      transceivers.rxResets
      transceivers.rxDatas

  expectCounterErrorSys =
    fmap or
      $ bundle
      $ zipWith (.&&.) transceivers.linkUps
      $ zipWith (`xpmCdcSingle` sysClk) transceivers.rxClocks expectCounterError

  txStart = fold (.&&.) transceivers.txReadys

  transceivers =
    transceiverPrbsN
      @GthTx
      @GthRx
      @Ext200
      @Basic125
      @GthTxS
      @GthRxS
      defConfig{debugFpgaIndex = bitCoerce <$> fpgaIndex}
      Inputs
        { clock = sysClk
        , reset = gthAllReset
        , refClock = refClk
        , channelNames
        , clockPaths
        , rxSims = rxs
        , rxNs
        , rxPs
        , txDatas = repeat txCounter
        , txStarts = repeat txStart
        , rxReadys = repeat (pure True)
        }

-- | Top entity for this test. See module documentation for more information.
transceiversUpTest ::
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "SYNC_IN" ::: Signal Basic125 Bool ->
  "GTH_RX_S" ::: Gth.SimWires GthRx LinkCount ->
  "GTH_RX_NS" ::: Gth.Wires GthRxS LinkCount ->
  "GTH_RX_PS" ::: Gth.Wires GthRxS LinkCount ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "GTH_TX_S" ::: Gth.SimWires GthTx LinkCount
  , "GTH_TX_NS" ::: Gth.Wires GthTxS LinkCount
  , "GTH_TX_PS" ::: Gth.Wires GthTxS LinkCount
  , "SYNC_OUT" ::: Signal Basic125 Bool
  , "spiDone" ::: Signal Basic125 Bool
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  )
transceiversUpTest refClkDiff sysClkDiff syncIn rxs rxns rxps miso =
  (txs, txns, txps, syncOut, spiDone, spiOut)
 where
  refClk = Gth.ibufds_gte3 refClkDiff :: Clock Ext200

  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset

  testRst = sysRst `orReset` unsafeFromActiveLow startTest `orReset` syncInRst
  syncOut = startTest
  syncInRst =
    resetGlitchFilter (SNat @1024) sysClk
      $ unsafeFromActiveLow
      $ xpmCdcSingle sysClk sysClk syncIn

  (txs, txns, txps, allUp, anyErrors, _stats, spiDone, spiOut) =
    goTransceiversUpTest fpgaIndex refClk sysClk testRst rxs rxns rxps miso

  failAfterUp = isFalling sysClk testRst enableGen False allUp
  failAfterUpSticky = sticky sysClk testRst failAfterUp

  startTest = isJust <$> maybeFpgaIndex
  fpgaIndex = fromMaybe 0 <$> maybeFpgaIndex

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
    , targetXdcs = ["transceiversUpTest.xdc"]
    , testCases = iters
    , mDriverProc = Nothing
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
