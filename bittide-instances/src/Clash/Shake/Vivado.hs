-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Generate a TCL script to simulate generated VHDL
--
-- Run with @vivado -mode batch -source ...@
--
module Clash.Shake.Vivado
  ( LocatedManifest(..)
  , mkFalsePathXdc
  , mkPlaceTcl
  , mkNetlistTcl
  , mkSynthesisTcl
  , mkRouteTcl
  ) where

import Prelude

import Clash.DataFiles (tclConnector)
import Clash.Driver.Manifest
import Data.String.Interpolate (__i)
import System.FilePath ((</>), dropFileName)

-- TODO: Upstream
data LocatedManifest = LocatedManifest
  { -- | Path pointing to the manifest file itself
    lmPath :: FilePath

    -- | Manifest file corresponding to the one at 'lmPath'
  , lmManifest :: Manifest
  }

-- | Mark inputs and outputs as false path, excluding clocks.
--
-- Note that this is fairly dangerous thing to do and should really only be used
-- in combination with 'Clash.Instances.Hacks.reducePins'. Even then, you should
-- make sure to synchronize any resets properly.
--
mkFalsePathXdc :: Manifest -> String
mkFalsePathXdc manifest = unlines (concatMap goPort (ports manifest))
 where
  goPort port =
    case (mpIsClock port, mpDirection port) of
      (True, _) -> []
      (False, In) -> [[__i|set_false_path -from [get_ports {#{mpName port}}]|]]
      (False, Out) -> [[__i|set_false_path -to [get_ports {#{mpName port}}]|]]
      (False, InOut) -> goPort port{mpDirection=In} <> goPort port{mpDirection=Out}

-- | Generates TCL that generates and reads Xilinx IP and reads constraints and
-- HDL files generated by Clash. The caller is responsible for starting synthesis
-- or simulation.
--
mkBaseTcl ::
  -- | Top entity directory
  LocatedManifest ->
  -- | TCL script
  IO String
mkBaseTcl LocatedManifest{lmPath} = do
  connector <- tclConnector
  let topEntityDir = dropFileName lmPath

  pure [__i|
    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR
    source -notrace {#{connector}}
    clash::readMetadata {#{topEntityDir}}
    clash::createAndReadIp -dir ip
    clash::readHdl
    clash::readXdc {normal}
    set_property TOP $clash::topEntity [current_fileset]
  |]

mkSynthesisTcl ::
  -- | Directory to write logs and checkpoints to
  FilePath ->
  -- | Out of context?
  Bool ->
  -- | Part to synthesize for. E.g., 'xcku040-ffva1156-2-e'.
  String ->
  -- | Manifests of which the first is the top-level to synthesize
  LocatedManifest ->
  -- | Rendered TCL
  IO String
mkSynthesisTcl outputDir outOfContext part manifest@LocatedManifest{lmManifest} = do
  baseTcl <- mkBaseTcl manifest
  pure $ baseTcl <> "\n" <> [__i|
    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR

    file mkdir {#{outputDir </> "reports"}}
    file mkdir {#{outputDir </> "checkpoints"}}

    \# Synthesis
    synth_design -name #{name} -part #{part} -mode #{outOfContextStr}
    report_timing_summary -file {#{outputDir </> "reports" </> "post_synth_timing_summary.rpt"}}
    write_checkpoint -force {#{outputDir </> "checkpoints" </> "post_synth.dcp"}}

    \# Netlist
    file mkdir {#{outputDir </> "netlist"}}
    write_verilog -force {#{outputDir </> "netlist.v"}}
    write_xdc -no_fixed_only -force {#{outputDir </> "netlist.xdc"}}
  |]
 where
  name = topComponent lmManifest
  outOfContextStr
    | outOfContext = "out_of_context" :: String
    | otherwise    = "default"

mkPlaceTcl :: FilePath -> String
mkPlaceTcl outputDir = [__i|
    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR

    \# Pick up where synthesis left off
    open_checkpoint {#{outputDir </> "checkpoints" </> "post_synth.dcp"}}

    \# Run optimization & placement
    opt_design
    place_design
    phys_opt_design
    write_checkpoint -force {#{outputDir </> "checkpoints" </> "post_place.dcp"}}
    report_timing_summary -file {#{outputDir </> "reports" </> "post_place_timing_summary.rpt"}}
|]

mkRouteTcl :: FilePath -> String
mkRouteTcl outputDir = [__i|
    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR

    \# Pick up where placement left off
    open_checkpoint {#{outputDir </> "checkpoints" </> "post_place.dcp"}}

    \# Routing
    route_design
    write_checkpoint -force {#{outputDir </> "checkpoints" </> "post_route.dcp"}}
    report_timing_summary -file {#{outputDir </> "reports" </> "post_route_timing_summary.rpt"}}
    report_timing -sort_by group -max_paths 100 -path_type summary -file {#{outputDir </> "reports" </> "post_route_timing.rpt"}}

    report_clock_utilization -file {#{outputDir </> "reports" </> "post_route_clock_util.rpt"}}
    report_utilization       -file {#{outputDir </> "reports" </> "post_route_util.rpt"}}
    report_power             -file {#{outputDir </> "reports" </> "post_route_power.rpt"}}
    report_drc               -file {#{outputDir </> "reports" </> "post_route_drc.rpt"}}
|]

mkNetlistTcl :: FilePath -> String
mkNetlistTcl outputDir = [__i|
    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR

    \# Pick up where routing left off
    open_checkpoint {#{outputDir </> "checkpoints" </> "post_route.dcp"}}

    \# Generate netlist and constraints
    file mkdir {#{outputDir </> "netlist"}}
    write_verilog -force {#{outputDir </> "netlist.v"}}
    write_xdc -no_fixed_only -force {#{outputDir </> "netlist.xdc"}}
|]