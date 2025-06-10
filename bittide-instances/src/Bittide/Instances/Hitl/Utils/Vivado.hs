-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

module Bittide.Instances.Hitl.Utils.Vivado where

import Prelude

import Bittide.Hitl
import Bittide.Instances.Hitl.Setup

import Vivado.Tcl

import Data.Maybe
import "extra" Data.List.Extra (elemIndex, split, (!?))

{- $setup
>>> import Vivado.Tcl
-}

getTestProbeTcl :: String -> String
getTestProbeTcl probeNm =
  "[get_hw_probes -of_objects [get_hw_vios] " <> probeNm <> "]"

{- | Tcl code to get the HITL VIO test start output probe.
Run `verifyHitlVio` beforehand to ensure that the probe is available.
-}
getProbeTestStartTcl :: String
getProbeTestStartTcl = getTestProbeTcl "*vioHitlt/probe_test_start"

{- | Tcl code to get the HITL VIO test data output probe.
Run `verifyHitlVio` beforehand and verify that the HITL test parameter
`BitSize` isn't zero to ensure that the probe is available.
-}
getProbeTestDataTcl :: String
getProbeTestDataTcl = getTestProbeTcl "*vioHitlt/probe_test_data"

{- | Tcl code to get the HITL VIO test done input probe.
Run `verifyHitlVio` beforehand to ensure that the probe is available.
-}
getProbeTestDoneTcl :: String
getProbeTestDoneTcl = getTestProbeTcl "*vioHitlt/probe_test_done"

{- | Tcl code to get the HITL VIO test success input probe.
Run `verifyHitlVio` beforehand to ensure that the probe is available.
-}
getProbeTestSuccessTcl :: String
getProbeTestSuccessTcl = getTestProbeTcl "*vioHitlt/probe_test_success"

{- | Takes the ID part of a Vivado hardware target. This is what Vivado seems to
call the UID minus the vendor string.

==== __Example__
>>> idFromHwT (HwTarget "localhost:3121/xilinx_tcf/Digilent/210308B0B0C2")
"210308B0B0C2"
-}
idFromHwT :: HwTarget -> FpgaId
idFromHwT = fromMaybe err . (!? 3) . split (== '/') . fromHwTarget
 where
  err = error "Unexpected format for hw_target Tcl object"

{- | Attempt to determine the hardware target index/position in the HITL
test setup to prepend it to its prettier name.
-}
prettyShow :: HwTarget -> String
prettyShow hwT =
  let hwTId = idFromHwT hwT
   in case hwTId `elemIndex` knownFpgaIds of
        Just index -> show index <> "_" <> hwTId
        Nothing -> hwTId
