-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Checks whether `+` and `-` work as expected, though its real purpose is to
-- check whether we can run hardware-in-the-loop tests.
module Bittide.Instances.Tests.BoardTest where

import Clash.Explicit.Prelude

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.VIO
import Clash.Cores.Xilinx.Extra (ibufds)

import Bittide.Instances.Domains

type TestStart = Bool
data TestState = Busy | Done TestSuccess
data TestSuccess = TestFailed | TestSuccess deriving (Generic, Eq, NFDataX)

toDoneSuccess :: TestState -> (Bool, Bool)
toDoneSuccess Busy = (False, False)
toDoneSuccess (Done s) = (True, s == TestSuccess)

data CheckState n
  = CsChecking (Index n)
  | CsDone TestSuccess
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
simpleHardwareInTheLoopTest ::
  "CLK_125MHZ" ::: DiffClock Basic125 ->
  "" ::: Signal Basic125
    ( "done" ::: Bool
    , "success" ::: Bool
    )
simpleHardwareInTheLoopTest diffClk = bundle (testDone, testSuccess)
 where
  clk = ibufds diffClk
  rst = unsafeFromActiveLow testStart

  testState = check clk rst (+) stimuli
  (testDone, testSuccess) = unbundle $ toDoneSuccess <$> testState

  testStart =
    setName @"vioHitlt" $
    vioProbe
      ("probe_test_done" :> "probe_test_success" :> Nil)
      ("probe_test_start" :> Nil)
      False
      clk
      testDone
      testSuccess

  stimuli :: Vec 4 (Unsigned 8, Unsigned 8, Unsigned 8)
  stimuli = (
       (  0,   0,   0)
    :> (  1,   2,   3)
    :> (255,   0, 255)
    :> (255,   1,   0)
    :> Nil
    )
makeTopEntity 'simpleHardwareInTheLoopTest

-- | Testing circuit for `plus` and `minus`. Feeds the circuit with inputs and
-- checks the received output against the expected output.
extendedHardwareInTheLoopTest ::
  "CLK_125MHZ" ::: DiffClock Basic125 ->
  "" ::: Signal Basic125
    ( "done" ::: Bool
    , "success" ::: Bool
    )
extendedHardwareInTheLoopTest diffClk = bundle (testDone, testSuccess)
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
    unbundle $
    setName @"vioHitlt" $
    vioProbe
      (  "probe_test_done"
      :> "probe_test_success"
      :> Nil
      )
      (  "probe_test_start_a"
      :> "probe_test_start_b"
      :> Nil
      )
      (False, False)
      clk
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
makeTopEntity 'extendedHardwareInTheLoopTest
