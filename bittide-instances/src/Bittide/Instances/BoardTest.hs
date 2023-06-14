-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Checks whether `+` works as expected, though its real purpose is to check
-- whether we can run hardware-in-the-loop tests.
module Bittide.Instances.BoardTest where

import Clash.Explicit.Prelude

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.VIO
import Clash.Cores.Xilinx.Extra (ibufds)

import Bittide.Instances.Domains

type StartTest = Bool
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
  "CLK_125MHZ_P" ::: Clock Basic125 ->
  "CLK_125MHZ_N" ::: Clock Basic125 ->
  "" ::: Signal Basic125
    ( "done" ::: Bool
    , "success" ::: Bool
    )
simpleHardwareInTheLoopTest clkP clkN = bundle (testDone, testSuccess)
 where
  clk = ibufds clkP clkN
  rst = unsafeFromLowPolarity startTest

  testState = check clk rst (+) stimuli
  (testDone, testSuccess) = unbundle $ toDoneSuccess <$> testState

  startTest =
    vioProbe
      ("probe_test_done" :> "probe_test_success" :> Nil)
      ("probe_start_test" :> Nil)
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
