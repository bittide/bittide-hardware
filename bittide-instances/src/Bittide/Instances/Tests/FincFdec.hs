-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NumericUnderscores #-}

-- | A couple of tests testing clock board programming, and subsequently the
-- FINC and FDEC pins.
module Bittide.Instances.Tests.FincFdec where

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.Extra (ibufds)
import Clash.Cores.Xilinx.VIO (vioProbe)
import Clash.Cores.Xilinx.Xpm.Cdc.Single (xpmCdcSingle)
import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)
import Clash.Sized.Vector.Extra (findWithDefault)
import Clash.Xilinx.ClockGen (clockWizardDifferential)

import Bittide.Arithmetic.Time
import Bittide.Counter (domainDiffCounter)
import Bittide.ClockControl (SpeedChange(NoChange, SlowDown, SpeedUp), speedChangeToFincFdec)
import Bittide.ClockControl.Si539xSpi (si539xSpi, ConfigState(Finished))
import Bittide.Instances.Domains

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
  deriving (Enum, Generic, NFDataX)

-- | Lists all contructors of 'Test'
allTests :: Vec 4 Test
allTests = FDec :> FInc :> FDecInc :> FIncDec :> Nil

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
  Clock Basic200A ->
  Reset Basic200A ->
  Clock Basic200B ->
  Signal Basic200A Test ->
  "MISO" ::: Signal Basic200A Bit -> -- SPI
  "" :::
    ( Signal Basic200A TestState

    -- Freq increase / freq decrease request to clock board
    , "" :::
      ( "FINC"    ::: Signal Basic200A Bool
      , "FDEC"    ::: Signal Basic200A Bool
      )

      -- SPI to clock board:
    , "" :::
      ( "SCLK" ::: Signal Basic200A Bool
      , "MOSI" ::: Signal Basic200A Bit
      , "CSB"  ::: Signal Basic200A Bool
      )

      -- Debug signals:
    , "" :::
      ( "SPI_BUSY" ::: Signal Basic200A Bool
      , "SPI_STATE" ::: Signal Basic200A (BitVector 40)
      , "SI_LOCKED" ::: Signal Basic200A Bool
      , "COUNTER_ACTIVE" ::: Signal Basic200A Bool
      , "COUNTER" ::: Signal Basic200A (Signed 32)
      )
    )
goFincFdecTests clk rst clkControlled testSelect miso =
  (testResult, fIncDec, spiOut, debugSignals)
 where
  debugSignals = (spiBusy, pack <$> spiState, siClkLocked, counterActive, counter)

  (_, spiBusy, spiState@(fmap (==Finished) -> siClkLocked), spiOut) =
    withClockResetEnable clk rst enableGen $
      si539xSpi
        Si5395J.testConfig6_200_on_0a_and_0
        (SNat @(Microseconds 1))
        (pure Nothing)
        miso

  rstTest = unsafeFromActiveLow siClkLocked

  rstControlled =
    unsafeFromActiveLow $
      xpmCdcSingle clk clkControlled $ -- improvised reset syncer
        unsafeToActiveLow rst

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
  "CLK_125MHZ" ::: DiffClock Basic125 ->

  -- Pins from clock board:
  "USER_SMA_CLOCK" ::: DiffClock Basic200B ->
  "MISO" ::: Signal Basic200A Bit -> -- SPI

  "" :::
    ( "" :::
      ( "done"    ::: Signal Basic200A Bool
      , "success" ::: Signal Basic200A Bool
      )

    -- Freq increase / freq decrease request to clock board
    , "" :::
      ( "FINC"    ::: Signal Basic200A Bool
      , "FDEC"    ::: Signal Basic200A Bool
      )

      -- SPI to clock board:
    , "" :::
      ( "SCLK" ::: Signal Basic200A Bool
      , "MOSI" ::: Signal Basic200A Bit
      , "CSB"  ::: Signal Basic200A Bool
      )
    )
fincFdecTests diffClk controlledDiffClock spiIn =
  ((testDone, testSuccess), fIncDec, spiOut)
 where
  clkControlled = ibufds controlledDiffClock

  (clk, clkStable0) = clockWizardDifferential (SSymbol @"pll") diffClk noReset
  clkStable1 = xpmCdcSingle clk clk clkStable0 -- improvised reset syncer

  clkStableRst = unsafeFromActiveLow clkStable1
  anyStarted = fold (||) <$> startTests
  testRst = orReset clkStableRst (unsafeFromActiveLow anyStarted)
  testRstBool = unsafeToActiveHigh testRst

  (fInc, fDec) = fIncDec

  testF = fst . findWithDefault (FDec, True) snd . zip allTests <$> startTests

  (testResult, fIncDec, spiOut, debugSignals) =
    goFincFdecTests clk testRst clkControlled testF spiIn

  (testDone, testSuccess) = unbundle $ testStateToDoneSuccess <$> testResult

  (spiBusy, spiState, siClkLocked, counterActive, counter) = debugSignals

  startTests :: Signal Basic200A (Vec 4 Bool)
  startTests =
    vioProbe
      (  "probe_test_done"
      :> "probe_test_success"

      -- Debug signals:
      :> "probe_clkStable1"
      :> "probe_testRstBool"
      :> "probe_spiBusy"
      :> "probe_spiState"
      :> "probe_siClkLocked"
      :> "probe_counterActive"
      :> "probe_counter"
      :> "probe_fInc"
      :> "probe_fDec"
      :> Nil)
      (  "probe_test_start_fdec"
      :> "probe_test_start_finc"
      :> "probe_test_start_fdecfinc"
      :> "probe_test_start_fincfdec"
      :> Nil)
      (repeat False)
      clk
      testDone
      testSuccess

      -- Debug signals
      clkStable1
      testRstBool
      spiBusy
      spiState
      siClkLocked
      counterActive
      counter
      fInc
      fDec
{-# NOINLINE fincFdecTests #-}
makeTopEntity 'fincFdecTests
