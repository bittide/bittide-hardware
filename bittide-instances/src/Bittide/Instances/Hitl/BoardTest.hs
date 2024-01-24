-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Checks whether `+` and `-` work as expected, though its real purpose is to
-- check whether we can run hardware-in-the-loop tests.
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Bittide.Instances.Hitl.BoardTest where

import Clash.Explicit.Prelude

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.Ila
import Clash.Cores.Xilinx.Extra (ibufds)

import Bittide.Instances.Domains
import Bittide.Instances.Hitl

type TestStart = Bool
data TestState = Busy | Done TestResult
data TestResult = TestFailed | TestSuccess deriving (Generic, Eq, NFDataX)

toDoneSuccess :: TestState -> (Bool, Bool)
toDoneSuccess Busy = (False, False)
toDoneSuccess (Done s) = (True, s == TestSuccess)

data CheckState n
  = CsChecking (Index n)
  | CsDone TestResult
  deriving (Generic, NFDataX)

check ::
  forall n a b c dom .
  ( KnownDomain dom
  , KnownNat n
  , Eq c
  ) =>
  Clock dom ->
  Reset dom ->
  (a -> b -> c) ->
  Vec n (a, b, c) ->
  Signal dom TestState
check clk rst dut stimuli =
  mealy clk rst enableGen go (CsChecking 0 :: CheckState n) (pure ())
 where
  go :: CheckState n -> () -> (CheckState n, TestState)
  go s@(CsDone testSuccess) () = (s, Done testSuccess)
  go (CsChecking n) () = (s, Busy)
   where
    (a, b, c) = stimuli !! n
    testResult = dut a b == c
    s | not testResult = CsDone TestFailed
      | n == maxBound = CsDone TestSuccess
      | otherwise = CsChecking (n + 1)

-- | Testing circuit for `plus`. Feeds the circuit with inputs and checks
-- the received output against the expected output.
boardTestSimple ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "" ::: Signal Ext125
    ( "done" ::: Bool
    , "success" ::: Bool
    )
boardTestSimple diffClk = bundle (testDone, testSuccess)
 where
  clk = ibufds diffClk
  rst = unsafeFromActiveLow testStart

  testState = check clk rst (+) stimuli
  (testDone, testSuccess) = unbundle $ toDoneSuccess <$> testState

  testStart =
    testActive $ vioHitlt @SimpleTest Nil clk testDone testSuccess

  stimuli :: Vec 4 (Unsigned 8, Unsigned 8, Unsigned 8)
  stimuli = (
       (  0,   0,   0)
    :> (  1,   2,   3)
    :> (255,   0, 255)
    :> (255,   1,   0)
    :> Nil
    )
makeTopEntity 'boardTestSimple

type BoardTestExtended = SimpleTests
  '[ "test_a"
   , "test_b"
   ]

-- | Testing circuit for `plus` and `minus`. Feeds the circuit with inputs and
-- checks the received output against the expected output.
boardTestExtended ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "" ::: Signal Ext125
    ( "done" ::: Bool
    , "success" ::: Bool
    )
boardTestExtended diffClk = hwSeqX boardTestIla $ bundle (testDone, testSuccess)
 where
  clk = ibufds diffClk
  rstA = unsafeFromActiveLow testStartA
  rstB = unsafeFromActiveLow testStartB

  testStateA = check clk rstA (+) stimuliA
  (testDoneA, testSuccessA) = unbundle $ toDoneSuccess <$> testStateA

  testStateB = check clk rstB (-) stimuliB
  (testDoneB, testSuccessB) = unbundle $ toDoneSuccess <$> testStateB

  (testDone, testSuccess) = unbundle $
    mux
      testStartA
      (bundle (testDoneA, testSuccessA))
      (mux
        testStartB
        (bundle (testDoneB, testSuccessB))
        (pure (False, False))
      )


  (testStartA, testStartB) =
    let hitl = vioHitlt @BoardTestExtended Nil clk testDone testSuccess
     in ( testActive @"test_a" hitl
        , testActive @"test_b" hitl
        )

  boardTestIla :: Signal Ext125 ()
  boardTestIla =
    setName @"boardTestIla" $
    ila
      (ilaConfig $
           "trigger_AorB"
        :> "capture"
        :> "testStartA"
        :> "testStartB"
        :> "testDone"
        :> "testSuccess"
        :> Nil
      )
      clk
      -- Trigger when starting either test
      (testStartA .||. testStartB)
      -- Always capture
      (pure True :: Signal Ext125 Bool)

      -- Debug probes
      testStartA
      testStartB
      testDone
      testSuccess


  stimuliA :: Vec 4 (Unsigned 8, Unsigned 8, Unsigned 8)
  stimuliA = (
       (  0,   0,   0)
    :> (  1,   2,   3)
    :> (255,   0, 255)
    :> (255,   1,   0)
    :> Nil
    )
  stimuliB :: Vec 4 (Unsigned 8, Unsigned 8, Unsigned 8)
  stimuliB = (
       (  0,   0,   0)
    :> (  3,   2,   1)
    :> (255,   0, 255)
    :> (  0,   1, 255)
    :> Nil
    )
makeTopEntity 'boardTestExtended
