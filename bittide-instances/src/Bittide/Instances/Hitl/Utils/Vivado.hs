-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

module Bittide.Instances.Hitl.Utils.Vivado where

import Prelude

import Bittide.Hitl
import Bittide.Instances.Hitl.Setup

import Vivado (VivadoHandle)
import Vivado.Tcl

import Control.Concurrent (threadDelay)
import Data.Maybe
import "extra" Data.List.Extra (elemIndex, sortOn, split, (!?))

import qualified Data.Map.Strict as Map

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

{- | Determine the FPGA ID associated with a 'HwTargetRef'. This is what Vivado
seems to call the UID minus the vendor string.
-}
idFromHwTRef :: HwTargetRef -> FpgaId
idFromHwTRef (HwTargetByIndex ix) =
  fromMaybe
    ("The given index " <> show ix <> " is out of range for the list of known FPGA IDs")
    (knownFpgaIds !? fromIntegral ix)
idFromHwTRef (HwTargetById targetId _) = targetId

-- | Determine the 'DeviceInfo' associated with a 'HwTargetRef'.
deviceInfoFromHwTRef :: HwTargetRef -> DeviceInfo
deviceInfoFromHwTRef (HwTargetByIndex ix) =
  fromMaybe
    (error $ "The given index " <> show ix <> " is out of range for the list of known FPGA IDs")
    (demoRigInfo !? fromIntegral ix)
deviceInfoFromHwTRef (HwTargetById _ d) = d

{- | Attempt to determine the hardware target index/position in the HITL
test setup to prepend it to its prettier name.
-}
prettyShow :: HwTarget -> String
prettyShow hwT =
  let hwTId = idFromHwT hwT
   in case hwTId `elemIndex` knownFpgaIds of
        Just index -> show index <> "_" <> hwTId
        Nothing -> hwTId

{- | Resolve the synthetic 'HwTarget's handed to a 'HitlDriver' against the
hardware targets actually hosted on the connected hardware server, matching by
FPGA id.

The targets in 'HitlDriverEnv' carry synthetic 'HwTarget' strings (the hardware
server URL and FPGA id, with a placeholder vendor) because they are constructed
statically without a Vivado connection. Vivado will not accept those as hardware
targets, so any driver that opens targets through Vivado must first resolve them
with this function (inside its 'withVivado' region). The 'DeviceInfo' associated
with each requested target is preserved.

The result is returned in rig order ('prettyShow', i.e. the order of
'knownFpgaIds'), regardless of the order of the requested targets. Some tests
critically rely on the HITL VIO start probes being asserted/deasserted in this
order (e.g. @syncInSyncOut@), and this matches the order the old Vivado-resolved
test runner used.

Retries a few times to give the hardware server a chance to enumerate all
targets.
-}
resolveHwTargets ::
  VivadoHandle -> [(HwTarget, DeviceInfo)] -> IO [(HwTarget, DeviceInfo)]
resolveHwTargets v requested = go (10 :: Int)
 where
  requestedIds = [idFromHwT hwT | (hwT, _) <- requested]

  go numTries = do
    foundTargets <- get_hw_targets v []
    putStrLn $
      "The connected hardware server hosts "
        <> show (length foundTargets)
        <> " hardware targets:"
    mapM_ (putStrLn . ('\t' :) . show) foundTargets
    let
      byId = Map.fromList [(idFromHwT hwT, hwT) | hwT <- foundTargets]
      missing = [fid | fid <- requestedIds, not (Map.member fid byId)]
    if null missing
      then
        pure $
          sortOn (prettyShow . fst) $
            [(byId Map.! idFromHwT synthHwT, d) | (synthHwT, d) <- requested]
      else do
        putStrLn $
          "[WARNING] The connected hardware server did not host the requested "
            <> "hardware targets with IDs "
            <> show missing
        if numTries <= 0
          then error "Giving up resolving hardware targets."
          else do
            putStrLn "Retrying..."
            threadDelay 500_000 -- μs
            refresh_hw_server v []
            go (numTries - 1)
