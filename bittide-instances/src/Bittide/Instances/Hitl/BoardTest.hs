-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Checks whether `+` and `-` work as expected, though its real purpose is to
-- check whether we can run hardware-in-the-loop tests.
module Bittide.Instances.Hitl.BoardTest where

import Clash.Explicit.Prelude

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.Ila
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
makeTopEntity 'boardTestSimple

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
  rstA = unsafeFromActiveLow (testStart .&&. (not <$> testSelect))
  rstB = unsafeFromActiveLow (testStart .&&. testSelect)

  testStateA = check clk rstA (+) stimuliA
  testStateB = check clk rstB (-) stimuliB

  (testDone, testSuccess) = unbundle $ toDoneSuccess <$> mux testSelect testStateB testStateA
  (testStart, testSelect) =
    unbundle $
    setName @"vioHitlt" $
    vioProbe
      (  "probe_test_done"
      :> "probe_test_success"
      :> Nil
      )
      ( "probe_test_start"
      :> "testSelect"
      :> Nil
      )
      (False, False)
      clk
      testDone
      testSuccess


  boardTestIla :: Signal Ext125 ()
  boardTestIla =
    setName @"boardTestIla" $
    ila
      (ilaConfig $
           "trigger_AorB"
        :> "capture"
        :> "ilaTestSelect"
        :> "ilaTestDone"
        :> "ilaTestSuccess"
        :> Nil
      )
      clk
      -- Trigger when starting either test
      testStart
      -- Always capture
      (pure True :: Signal Ext125 Bool)

      -- Debug probes
      testSelect
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
