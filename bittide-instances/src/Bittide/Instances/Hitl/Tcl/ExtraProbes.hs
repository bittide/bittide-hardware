-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Hitl.Tcl.ExtraProbes where

import Clash.Prelude

import Clash.Cores.Xilinx.Dna
import Bittide.Instances.Domains
import Clash.Annotations.TH ( makeTopEntity )
import Clash.Cores.Xilinx.Extra
import Clash.Cores.Xilinx.VIO
import Data.Maybe

-- | A circuit that verifies the correct behavior of the TCL infrastructure for
-- setting extra probes in Hitl tests.
extraProbesTest ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "success" ::: Signal Ext125 Bool
extraProbesTest diffClk = testSuccess
 where
  clk = ibufds diffClk

  testSuccess = testResult <$> testState <*> extraProbe <*> fpgaId
  testDone = testStart .&&. fmap isJust fpgaId
  rst = unsafeFromActiveLow testStart
  fpgaId = withClockResetEnable clk rst enableGen deviceDna defaultSimDNA
  (testStart, testState, extraProbe) =
    unbundle $
    setName @"vioHitlt" $
    vioProbe
      ("probe_test_done" :> "probe_test_success" :> "fpgaId" :> Nil)
      ("probe_test_start" :> "testState" :> "extraProbe" :> Nil)
      (False, SetDefaultProbes, maxBound)
      clk
      testDone
      testSuccess
      fpgaId
{-# NOINLINE extraProbesTest #-}

-- | Produce the test result based on the test state and the extra probe value.
-- These values should correspond to the yaml configuration.
testResult :: TestState -> BitVector 96 -> Maybe (BitVector 96) -> Bool
testResult s extraProbe fpgaId = case (s, extraProbe) of
  (SetDefaultProbes, 0) -> True
  (SetTestProbes, 0xDEADABBA) -> True
  (SetFpgaSpecificProbes, _) -> extraProbe == fromMaybe defaultSimDNA fpgaId
  _ -> False

data TestState
  = SetDefaultProbes -- Check if the default probes from the yaml file are set
  | SetTestProbes -- Check if the test specific probe values are set
  | SetFpgaSpecificProbes -- Check if the DNA device identifier is set
  deriving (Generic, NFDataX)

makeTopEntity 'extraProbesTest
