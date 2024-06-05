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

-- | Counter threshold after which a test is considered passed/failed. In theory
-- clocks can diverge at +-20 kHz (at 200 MHz), which gives the tests 500 ms to
-- adjust their clocks - which should be plenty.
threshold :: Signed 32
threshold = 20_000

testStateToDoneSuccess :: TestState -> (Bool, Bool)
testStateToDoneSuccess = \case
  Busy -> (False, False)
  Fail -> (True, False)
  Success -> (True, True)

goFincFdecTests ::
  Clock Basic200 ->
  Reset Basic200 ->
  Clock GthTx ->
  Reset GthTx ->
  Signal Basic200 Test ->
  "MISO" ::: Signal Basic200 Bit -> -- SPI
  "" :::
    ( Signal Basic200 TestState

    -- Freq increase / freq decrease request to clock board
    , "" :::
      ( "FINC"    ::: Signal Basic200 Bool
      , "FDEC"    ::: Signal Basic200 Bool
      )

      -- SPI to clock board:
    , "" :::
      ( "SCLK" ::: Signal Basic200 Bool
      , "MOSI" ::: Signal Basic200 Bit
      , "CSB"  ::: Signal Basic200 Bool
      )

      -- Debug signals:
    , "" :::
      ( "SPI_BUSY" ::: Signal Basic200 Bool
      , "SPI_STATE" ::: Signal Basic200 (BitVector 40)
      , "SI_LOCKED" ::: Signal Basic200 Bool
      , "COUNTER_ACTIVE" ::: Signal Basic200 Bool
      , "COUNTER" ::: Signal Basic200 (Signed 32)
      )
    )
goFincFdecTests clk rst clkControlled clkControlledRst testSelect miso =
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

  rstTest =
    unsafeFromActiveLow spiDone `orReset`
    (xpmResetSynchronizer Asserted clkControlled clk clkControlledRst)
  rstControlled = convertReset clk clkControlled rst

  (counter, counterActive) =
    unbundle $
      -- Note that in a "real" Bittide system the clocks would be wired up the
      -- other way around: the controlled domain would be the target domain. We
      -- don't do that here because we know 'rstControlled' will come out of
      -- reset much earlier than 'rstTest'. Doing it the "proper" way would
      -- therefore introduce extra complexity, without adding to the test's
      -- coverage.
      domainDiffCounter clkControlled rstControlled clk rstTest

  fIncDec = unbundle $ speedChangeToFincFdec clk rstTest fIncDecRequest

  (fIncDecRequest, testResult) = unbundle $
    (!!)
      <$> bundle (fDecResult :> fIncResult :> fDecIncResult :> fIncDecResult :> Nil)
      <*> fmap fromEnum testSelect

  fDecResult = goFdec <$> counter
  fIncResult = goFinc <$> counter
  fDecIncResult = mealy clk rstTest enableGen goFdecFinc FDec counter
  fIncDecResult = mealy clk rstTest enableGen goFincFdec FInc counter

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
  "MISO" ::: Signal Basic200 Bit -> -- SPI

  "" :::
    ( "" :::
      ( "done"    ::: Signal Basic200 Bool
      , "success" ::: Signal Basic200 Bool
      )

    -- Freq increase / freq decrease request to clock board
    , "" :::
      ( "FINC"    ::: Signal Basic200 Bool
      , "FDEC"    ::: Signal Basic200 Bool
      )

      -- SPI to clock board:
    , "" :::
      ( "SCLK" ::: Signal Basic200 Bool
      , "MOSI" ::: Signal Basic200 Bit
      , "CSB"  ::: Signal Basic200 Bool
      )
    )
fincFdecTests diffClk controlledDiffClock spiIn =
  ((testDone, testSuccess), fIncDec, spiOut)
 where
  (clk, clkStableRst :: Reset Basic200) = clockWizardDifferential diffClk noReset

  clkControlled = ibufds_gte3 controlledDiffClock :: Clock Ext200

  (   _txn
    , _txp
    , txClock
    , _rxClock
    , _rx_data
    , _reset_tx_done
    , _reset_rx_done
    , txActive
    , _rxCtrl0, _rxCtrl1, _rxCtrl2, _rxCtrl3
    ) =
    gthCore
      @GthTx @GthRx @Ext200 @Basic200 @GthTx @GthRx
      "X0Y10" "clk0"
      (pure 0) (pure 0) -- rxN and rxP
      clk
      (delayReset Asserted clk gthRst) -- reset_all
      noReset -- gtwiz_reset_rx_datapath_in
      (pure 0) (pure 0) -- userdata_tx_in and txctrl
      clkControlled

  started = isJust <$> testInput
  testRst = clkStableRst `orReset` (unsafeFromActiveLow started)
  gthRst  = delayReset Asserted clk $
      orReset clkStableRst
    $ orReset (unsafeFromActiveLow started)
              (unsafeFromActiveLow spiDone)

  (testResult, fIncDec, spiOut, debugSignals) =
    goFincFdecTests
      clk testRst
      txClock (xpmResetSynchronizer Asserted txClock txClock $ unsafeFromActiveLow $ unpack <$> txActive)
      (fromJustX <$> testInput)
      spiIn

  (testDone, testSuccess) = unbundle $ testStateToDoneSuccess <$> testResult

  (_spiBusy, _spiState, spiDone, _diffCounterActive, _diffCounter) = debugSignals

  testInput :: Signal Basic200 (Maybe Test)
  testInput = hitlVio FDec clk testDone testSuccess
{-# NOINLINE fincFdecTests #-}
makeTopEntity 'fincFdecTests

tests :: HitlTests Test
tests = testsFromEnum allFpgas
