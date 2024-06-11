-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A couple of tests testing clock board programming, and subsequently the
-- FINC and FDEC pins.
module Bittide.Instances.Hitl.FincFdec where

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.GTH (ibufds_gte3, gthCore)
import Clash.Explicit.Prelude
import Clash.Explicit.Reset.Extra (Asserted(..), xpmResetSynchronizer, delayReset)
import Clash.Prelude (withClockResetEnable)
import Clash.Xilinx.ClockGen (clockWizardDifferential)

import Bittide.Counter (domainDiffCounter)
import Bittide.ClockControl (SpeedChange(NoChange, SlowDown, SpeedUp), speedChangeToFincFdec)
import Bittide.ClockControl.Si539xSpi (si539xSpi, ConfigState(Finished))
import Bittide.Hitl (HitlTests, testsFromEnum, hitlVio, allFpgas)
import Bittide.Instances.Domains

import Data.Maybe (isJust)

import qualified Bittide.ClockControl.Si5395J as Si5395J
import qualified Bittide.Transceiver.Cdc as Cdc
import qualified Bittide.Transceiver.ResetManager as ResetManager

data TestState = Busy | Fail | Success
data Test
  -- | Keep pressing FDEC, see if counter falls below certain threshold
  = FDec
  -- | Keep pressing FINC, see if counter exceeds certain threshold
  | FInc
  -- | 'FDec' test followed by an 'FInc' one
  | FDecInc
  -- | 'FInc' test followed by an 'FDec' one
  | FIncDec
  deriving (Enum, Generic, NFDataX, Bounded, BitPack, ShowX, Show)

-- | Reset manager for transceivers which ignores the rx domain. Use this
-- alternative to `ResetManager.resetManager` when the transceivers are only
-- used to extract a clock from a transceiver-only clock input (e.g.
-- `SMA_MGT_REFCLK_C`).
gthResetManagerWithoutRx ::
  forall dom .
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  "tx_init_done" ::: Signal dom Bool ->
  ( "reset_all_out" ::: Reset dom
  , "stats" ::: Signal dom ResetManager.Statistics
  )
gthResetManagerWithoutRx clk rst tx_init_done =
  ( reset_all_out_sig
  , statistics
  )
 where
  (reset_all_out_sig, _reset_rx_sig, statistics) =
    ResetManager.resetManager
      ResetManager.defConfig
      clk rst tx_init_done (pure True) (pure True)

-- | Counter threshold after which a test is considered passed/failed. In theory
-- clocks can diverge at +-12.5 kHz (at 125 MHz), which gives the tests 500 ms
-- to adjust their clocks - which should be plenty.
threshold :: Signed 32
threshold = 12_500

testStateToDoneSuccess :: TestState -> (Bool, Bool)
testStateToDoneSuccess = \case
  Busy -> (False, False)
  Fail -> (True, False)
  Success -> (True, True)

goFincFdecTests ::
  Clock Basic125 ->
  Reset Basic125 ->
  Clock GthTx ->
  Reset Basic125 ->
  Signal Basic125 Test ->
  "MISO" ::: Signal Basic125 Bit -> -- SPI
  "" :::
    ( Signal Basic125 TestState

    -- Freq increase / freq decrease request to clock board
    , "" :::
      ( "FINC"    ::: Signal Basic125 Bool
      , "FDEC"    ::: Signal Basic125 Bool
      )

      -- SPI to clock board:
    , "" :::
      ( "SCLK" ::: Signal Basic125 Bool
      , "MOSI" ::: Signal Basic125 Bit
      , "CSB"  ::: Signal Basic125 Bool
      )

      -- Debug signals:
    , "" :::
      ( "SPI_BUSY" ::: Signal Basic125 Bool
      , "SPI_STATE" ::: Signal Basic125 (BitVector 40)
      , "SI_LOCKED" ::: Signal Basic125 Bool
      , "COUNTER_ACTIVE" ::: Signal Basic125 Bool
      , "COUNTER" ::: Signal Basic125 (Signed 32)
      )
    )
goFincFdecTests clk rst clkControlled resetControlledFree testSelect miso =
  (testResult, fIncDec, spiOut, debugSignals)
 where
  debugSignals = (spiBusy, pack <$> spiState, spiDone, counterActive, counter)

  (_, spiBusy, spiState@(fmap (==Finished) -> spiDone), spiOut) =
    withClockResetEnable clk rst enableGen $
      si539xSpi
        Si5395J.testConfig6_200_on_0a_1ppb_and_0
        (SNat @(Microseconds 1))
        (pure Nothing)
        miso

  rstControlled = xpmResetSynchronizer Asserted clk clkControlled rst

  (counter, counterActive) =
    unbundle $
      -- Note that in a "real" Bittide system the clocks would be wired up the
      -- other way around: the controlled domain would be the target domain. We
      -- don't do that here because we know 'rstControlled' will come out of
      -- reset much earlier than 'resetControlledFree'. Doing it the "proper"
      -- way would therefore introduce extra complexity, without adding to the
      -- test's coverage.
      domainDiffCounter clkControlled rstControlled clk resetControlledFree

  fIncDec = unbundle $ speedChangeToFincFdec clk resetControlledFree fIncDecRequest

  (fIncDecRequest, testResult) = unbundle $
    (!!)
      <$> bundle (fDecResult :> fIncResult :> fDecIncResult :> fIncDecResult :> Nil)
      <*> fmap fromEnum testSelect

  fDecResult = goFdec <$> counter
  fIncResult = goFinc <$> counter
  fDecIncResult = mealy clk resetControlledFree enableGen goFdecFinc FDec counter
  fIncDecResult = mealy clk resetControlledFree enableGen goFincFdec FInc counter

  -- Keep pressing FDEC, expect counter to go below -@threshold@
  goFdec :: Signed 32 -> (SpeedChange, TestState)
  goFdec n
    | n > threshold  = (NoChange, Fail)
    | n < -threshold = (NoChange, Success)
    | otherwise      = (SlowDown, Busy)

  -- Keep pressing FINC, expect counter to go above @threshold@
  goFinc :: Signed 32 -> (SpeedChange, TestState)
  goFinc n
    | n > threshold  = (NoChange, Success)
    | n < -threshold = (NoChange, Fail)
    | otherwise      = (SpeedUp, Busy)

  -- Keep pressing FDEC, expect counter to go below -@threshold@, then keep pressing
  -- FINC, expect counter to go above 0.
  goFdecFinc :: Test -> Signed 32 -> (Test, (SpeedChange, TestState))
  goFdecFinc FDec n
    | n > threshold = (FDec, (NoChange, Fail))
    | n < -threshold = (FInc, (NoChange, Busy))
    | otherwise   = (FDec, (SlowDown, Busy))
  goFdecFinc FInc n
    | n > 0              = (FInc, (NoChange, Success))
    | n < -(3*threshold) = (FInc, (NoChange, Fail))
    | otherwise          = (FInc, (SpeedUp, Busy))
  goFdecFinc s _ = (s, (NoChange, Fail)) -- Illegal state

  -- Keep pressing FINC, expect counter to go above @threshold@, then keep pressing
  -- FDEC, expect counter to go below 0.
  goFincFdec :: Test -> Signed 32 -> (Test, (SpeedChange, TestState))
  goFincFdec FInc n
    | n > threshold  = (FDec, (NoChange, Busy))
    | n < -threshold = (FInc, (NoChange, Fail))
    | otherwise      = (FInc, (SpeedUp, Busy))
  goFincFdec FDec n
    | n > (3 * threshold) = (FDec, (NoChange, Fail))
    | n < 0               = (FDec, (NoChange, Success))
    | otherwise           = (FDec, (SlowDown, Busy))
  goFincFdec s _ = (s, (NoChange, Fail)) -- Illegal state

fincFdecTests ::
  -- Pins from internal oscillator:
  "CLK_125MHZ" ::: DiffClock Ext125 ->

  -- Pins from clock board:
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "MISO" ::: Signal Basic125 Bit -> -- SPI

  "" :::
    ( "" :::
      ( "done"    ::: Signal Basic125 Bool
      , "success" ::: Signal Basic125 Bool
      )

    -- Freq increase / freq decrease request to clock board
    , "" :::
      ( "FINC"    ::: Signal Basic125 Bool
      , "FDEC"    ::: Signal Basic125 Bool
      )

      -- SPI to clock board:
    , "" :::
      ( "SCLK" ::: Signal Basic125 Bool
      , "MOSI" ::: Signal Basic125 Bit
      , "CSB"  ::: Signal Basic125 Bool
      )
    )
fincFdecTests diffClk controlledDiffClock spiIn =
  ((testDone, testSuccess), fIncDec, spiOut)
 where
  (clk, clkStableRst :: Reset Basic125) = clockWizardDifferential diffClk noReset

  clkControlled = ibufds_gte3 controlledDiffClock :: Clock Ext200

  (   _txn
    , _txp
    , txClock
    , _rxClock
    , _rx_data
    , reset_tx_done
    , _reset_rx_done
    , txActive
    , _rxCtrl0, _rxCtrl1, _rxCtrl2, _rxCtrl3
    ) =
    gthCore
      @GthTx @GthRx @Ext200 @Basic125 @GthTx @GthRx
      "X0Y10" "clk0"
      (pure 0) (pure 0) -- rxN and rxP
      clk
      (delayReset Asserted clk reset_all_out_sig) -- reset_all
      noReset -- gtwiz_reset_rx_datapath_in
      (pure 0) (pure 0) -- userdata_tx_in and txctrl
      clkControlled

  (reset_all_out_sig, _gthStats) =
    gthResetManagerWithoutRx
      clk (unsafeFromActiveLow spiDone)
      resetTxDoneFree

  resetControlledFree =
      orReset clkStableRst
    $ orReset (unsafeFromActiveLow spiDone)
    $ orReset (unsafeFromActiveLow resetTxDoneFree)
              (unsafeFromActiveLow txActiveFree)

  resetTxDoneFree = Cdc.withLock txClock (unpack <$> reset_tx_done) clk clkStableRst (pure True)
  txActiveFree    = Cdc.withLock txClock (unpack <$> txActive) clk clkStableRst (pure True)

  started = isJust <$> testInput
  testRst = clkStableRst `orReset` (unsafeFromActiveLow started)

  (testResult, fIncDec, spiOut, debugSignals) =
    goFincFdecTests
      clk testRst
      txClock resetControlledFree
      (fromJustX <$> testInput)
      spiIn

  (testDone, testSuccess) = unbundle $ testStateToDoneSuccess <$> testResult

  (_spiBusy, _spiState, spiDone, _diffCounterActive, _diffCounter) = debugSignals

  testInput :: Signal Basic125 (Maybe Test)
  testInput = hitlVio FDec clk testDone testSuccess
{-# NOINLINE fincFdecTests #-}
makeTopEntity 'fincFdecTests

tests :: HitlTests Test
tests = testsFromEnum allFpgas
