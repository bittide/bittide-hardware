-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

-- | Test to confirm physical connection of SYNC_IN / SYNC_OUT.
--
-- For some tests in Bittide is important that our demo rig (consisting of eight
-- FPGA development boards) start their tests synchronously. To this end, we've
-- wired up these boards as such (shown with just three):
--
--     FPGA 0
--     +------------+
--     |    SYNC_IN |<--------+
--     |            |         |
--     |   SYNC_OUT |--X      |
--     +------------+         |
--                            |
--     FPGA 1                 |
--     +------------+         |
--     |    SYNC_IN |<--------|
--     |            |         |
--     |   SYNC_OUT |--X      |
--     +------------+         |
--                            |
--     FPGA 2                 |
--     +------------+         |
--     |    SYNC_IN |<--------+
--     |            |         |
--     |   SYNC_OUT |>--------+
--     +------------+
--
-- In this setup, all FPGAs can drive their SYNC_OUT to low as a default value
-- while driving it to high as soon as the their tests starts. Because only the
-- last FPGA has its SYNC_OUT physically connected, only its assertion will have
-- effect. In the mean time, all FPGAs should monitor their SYNC_IN and start
-- running when it gets asserted. As long as the last FPGA is the last to be
-- started (through test VIOs), this will make FPGAs start their tests
-- synchronously.
--
-- This test therefore consists of two parts:
--
--   * A component that drives SYNC_OUT low for 1 second, and high indefinitely.
--   * A component that checks whether SYNC_IN is low for at least a second, and
--     that it is asserted within 10 seconds thereafter.
--
-- This gives the TCL ~9 seconds to program all the boards.
--
-- Failure modes:
--
--   1. One or more SYNC_INs are *disconnected*. Boards will read either low or
--      high indefinitely, failing the tests. Alternatively; if no pull up or
--      pull down is available, results will be random - also failing the test.
--
--   2. None of the SYNC_OUTs are connected. Same result as (1).
--
--   3. Multiple SYNC_OUTs are connected. The last board in the chain will see
--      the rising edge "too soon".
--
--   4. Wrong SYNC_OUT is connected. Same result as (3).
--
module Bittide.Instances.Hitl.SyncInSyncOut where

import Clash.Explicit.Prelude hiding (PeriodToCycles)

import Bittide.Arithmetic.Time
import Bittide.Hitl (HitlTests, allFpgas, hitlVioBool, noConfigTest)
import Bittide.Instances.Domains

import Clash.Annotations.TH
import Clash.Cores.Xilinx.Xpm.Cdc.Single
import Clash.Xilinx.ClockGen

-- | An 'Index' counting to /n/ seconds on 'Basic300'
type IndexSeconds n = Index (PeriodToCycles Basic300 (Seconds n))

-- | Status of test. Used to communicate test success/failure to host computer.
data TestStatus
  = Busy
  | Fail
  | Success
  deriving (Generic, Show, ShowX, NFDataX)

-- | State for 'testFsm'
data TestFsmState
  = InReset
  -- ^ Lie low for a bit
  | ExpectLow (IndexSeconds 1)
  -- ^ Expect _low_ for a least 1 second
  | WaitForRising (IndexSeconds 10)
  -- ^ Expect a rising edge within 10 seconds. The edge is expected after _time
  -- it takes to start the remaining tests_.
  | Done TestStatus
  deriving (Generic, Show, ShowX, NFDataX)

-- | State for 'genFsm'
data GenFsmState
  = GInReset
  -- ^ Lie low for a bit
  | GLow (IndexSeconds 1)
  -- ^ Drive SYNC_OUT _low_ for exactly one second
  | GHigh
  -- ^ Drive SYNC_OUT _high_ indefinitely
  deriving (Generic, Show, ShowX, NFDataX)

-- | Check SYNC_IN. See Module documenation for more information.
testFsm :: TestFsmState -> Bool -> (TestFsmState, TestStatus)
testFsm   InReset           _     = (ExpectLow maxBound,     Busy)
testFsm   (ExpectLow _)     True  = (Done Fail,              Busy)
testFsm   (ExpectLow 0)     False = (WaitForRising maxBound, Busy)
testFsm   (ExpectLow n)     False = (ExpectLow (n - 1),      Busy)
testFsm   (WaitForRising _) True  = (Done Success,           Busy)
testFsm   (WaitForRising 0) False = (Done Fail,              Busy)
testFsm   (WaitForRising n) False = (WaitForRising (n - 1),  Busy)
testFsm s@(Done result)     _     = (s,                      result)

-- | Generate SYNC_OUT. See Module documenation for more information.
genFsm :: GenFsmState -> () -> (GenFsmState, Bool)
genFsm GInReset _ = (GLow maxBound, False)
genFsm (GLow 0) _ = (GHigh,         False)
genFsm (GLow n) _ = (GLow (n - 1),  False)
genFsm GHigh    _ = (GHigh,         True)

-- | Convert a 'TestStatus' in to a pair of booleans @done@ and @success@. Used
-- to communicate test status to host computer.
testStatusToDoneSuccess :: TestStatus -> (Bool, Bool)
testStatusToDoneSuccess = \case
  Busy -> (False, False)
  Fail -> (True, False)
  Success -> (True, True)

-- | Entry point for test. See module documentation for more information.
syncInSyncOut ::
  "SYSCLK_300" ::: DiffClock Ext300 ->
  "SYNC_IN" ::: Signal Basic300 Bool ->
  "SYNC_OUT" ::: Signal Basic300 Bool
syncInSyncOut sysClkDiff syncIn0 = syncOut
 where
  (sysClk, sysRst) = clockWizardDifferential sysClkDiff noReset
  testRst = sysRst `orReset` unsafeFromActiveLow startTest
  syncIn1 =
      unsafeToActiveHigh
    $ resetGlitchFilter (SNat @1024) sysClk
    $ unsafeFromActiveHigh
    $ xpmCdcSingle sysClk sysClk syncIn0

  testStatus = mealy sysClk testRst enableGen testFsm InReset syncIn1
  (testDone, testSuccess) = unbundle (testStatusToDoneSuccess <$> testStatus)

  syncOut =
    delay sysClk enableGen False $ -- << filter glitches in output
      mealy sysClk testRst enableGen genFsm GInReset (pure ())

  startTest :: Signal Basic300 Bool
  startTest = hitlVioBool sysClk testDone testSuccess
makeTopEntity 'syncInSyncOut

tests :: HitlTests ()
tests = noConfigTest "SyncInSyncOut" allFpgas
