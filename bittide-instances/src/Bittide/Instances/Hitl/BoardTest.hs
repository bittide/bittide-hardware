-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

{- | Checks whether `+` and `-` work as expected, though its real purpose is to
check whether we can run hardware-in-the-loop tests.
-}
module Bittide.Instances.Hitl.BoardTest where

import Clash.Explicit.Prelude

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.Extra (ibufds)
import Clash.Cores.Xilinx.Ila

import Bittide.Hitl (
  HitlTestCase (HitlTestCase),
  HitlTestGroup (..),
  hitlVio,
  hitlVioBool,
  paramForHwTargets,
  testCasesFromEnum,
 )
import Bittide.Instances.Domains (Ext125)
import Bittide.Instances.Hitl.Setup (allHwTargets)

type TestStart = Bool
data TestState = Busy | Done TestSuccess
data TestSuccess = TestFailed | TestSuccess deriving (Generic, Eq, NFDataX)

data Test = A | B deriving (Generic, Eq, Show, BitPack, Bounded, Enum, ShowX)

toDoneSuccess :: TestState -> (Bool, Bool)
toDoneSuccess Busy = (False, False)
toDoneSuccess (Done s) = (True, s == TestSuccess)

data CheckState n
  = CsChecking (Index n)
  | CsDone TestSuccess
  deriving (Generic, NFDataX)

check ::
  forall n a b c dom.
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
    s
      | not testResult = CsDone TestFailed
      | n == maxBound = CsDone TestSuccess
      | otherwise = CsChecking (n + 1)

{- | Testing circuit for `plus`. Feeds the circuit with inputs and checks
the received output against the expected output.
-}
boardTestSimple ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  ""
    ::: Signal
          Ext125
          ( "done" ::: Bool
          , "success" ::: Bool
          )
boardTestSimple diffClk = bundle (testDone, testSuccess)
 where
  clk = ibufds diffClk
  rst = unsafeFromActiveLow testStart

  testState = check clk rst (+) stimuli
  (testDone, testSuccess) = unbundle $ toDoneSuccess <$> testState

  testStart = hitlVioBool clk testDone testSuccess

  stimuli :: Vec 4 (Unsigned 8, Unsigned 8, Unsigned 8)
  stimuli =
    ( (0, 0, 0)
        :> (1, 2, 3)
        :> (255, 0, 255)
        :> (255, 1, 0)
        :> Nil
    )

makeTopEntity 'boardTestSimple

{- | Testing circuit for `plus` and `minus`. Feeds the circuit with inputs and
checks the received output against the expected output.
-}
boardTestExtended ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  ""
    ::: Signal
          Ext125
          ( "done" ::: Bool
          , "success" ::: Bool
          )
boardTestExtended diffClk = hwSeqX boardTestIla $ bundle (testDone, testSuccess)
 where
  clk = ibufds diffClk
  rstA = unsafeFromActiveLow testStartA
  rstB = unsafeFromActiveLow testStartB

  testStateA = check clk rstA (+) stimuliA
  testStateB = check clk rstB (-) stimuliB

  (testDone, testSuccess) =
    unbundle $ toDoneSuccess <$> mux testStartA testStateA testStateB

  testStartA = testStartAB .==. pure (Just A)
  testStartB = testStartAB .==. pure (Just B)

  testStartAB = hitlVio A clk testDone testSuccess

  boardTestIla :: Signal Ext125 ()
  boardTestIla =
    setName @"boardTestIla"
      $ ila
        ( ilaConfig
            $ "trigger_AorB"
            :> "capture"
            :> "ilaTestStartA"
            :> "ilaTestStartB"
            :> "ilaTestDone"
            :> "ilaTestSuccess"
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
  stimuliA =
    ( (0, 0, 0)
        :> (1, 2, 3)
        :> (255, 0, 255)
        :> (255, 1, 0)
        :> Nil
    )
  stimuliB :: Vec 4 (Unsigned 8, Unsigned 8, Unsigned 8)
  stimuliB =
    ( (0, 0, 0)
        :> (3, 2, 1)
        :> (255, 0, 255)
        :> (0, 1, 255)
        :> Nil
    )

makeTopEntity 'boardTestExtended

testSimple :: HitlTestGroup
testSimple =
  HitlTestGroup
    { topEntity = 'boardTestSimple
    , extraXdcFiles = []
    , externalHdl = []
    , testCases = [HitlTestCase "Simple" (paramForHwTargets allHwTargets ()) ()]
    , mPostProc = Nothing
    }

testExtended :: HitlTestGroup
testExtended =
  HitlTestGroup
    { topEntity = 'boardTestExtended
    , extraXdcFiles = []
    , externalHdl = []
    , testCases = testCasesFromEnum @Test allHwTargets ()
    , mPostProc = Just "post-board-test-extended"
    }
