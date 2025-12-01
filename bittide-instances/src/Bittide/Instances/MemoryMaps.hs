-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.MemoryMaps where

import Prelude

import Bittide.Instances.Hitl.VexRiscv (vexRiscvTestMM)
import Bittide.Instances.Pnr.Ethernet (vexRiscvEthernetMM)
import Bittide.Instances.Pnr.Freeze (freezeMM)
import Bittide.Instances.Pnr.ProcessingElement (vexRiscvUartHelloMM)

import Protocols.MemoryMap
import Protocols.MemoryMap.Check

import Project.FilePath (buildDir, findParentContaining)

import Control.Monad
import Language.Haskell.TH (reportError, runIO)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath

import qualified Bittide.Instances.Hitl.Dut.SoftUgnDemo as SoftUgnDemo
import qualified Bittide.Instances.Hitl.Dut.SwitchDemoGppe as SwitchDemoGppe
import qualified Bittide.Instances.Hitl.Si539xConfiguration as Si539xConfiguration
import qualified Bittide.Instances.Hitl.SwitchDemo.MemoryMaps as SwitchDemo
import qualified Bittide.Instances.Tests.ElasticBufferWb as ElasticBufferWb
import qualified Bittide.Instances.Tests.RegisterWb as RegisterWb
import qualified Bittide.Instances.Tests.ScatterGather as ScatterGather
import qualified Bittide.Instances.Tests.SwitchCalendar as SwitchCalendar
import qualified Bittide.Instances.Tests.TimeWb as TimeWb
import qualified Bittide.Instances.Tests.WbToDf as WbToDf
import qualified Data.ByteString.Lazy as BS
import qualified Protocols.MemoryMap.Json as Json

$( do
    -------------------------------
    -- MEMORY MAPS               --
    --                           --
    -- Add new memory maps here  --
    -------------------------------
    let memoryMaps =
          [ ("Ethernet", vexRiscvEthernetMM)
          , ("ElasticBufferWbTest", ElasticBufferWb.dutMM)
          , ("Freeze", freezeMM)
          , ("ProcessingElement", vexRiscvUartHelloMM)
          , ("RegisterWb", RegisterWb.memoryMap)
          , ("ScatterGatherPe", ScatterGather.dutMM)
          , ("Si539xConfiguration", Si539xConfiguration.memoryMap)
          , ("SwitchC", SwitchCalendar.memoryMap)
          , ("SwitchDemoBoot", SwitchDemo.boot)
          , ("SwitchDemoMu", SwitchDemo.mu)
          , ("SwitchDemoCc", SwitchDemo.cc)
          , ("SwitchDemoGppeMu", SwitchDemoGppe.memoryMapMu)
          , ("SwitchDemoGppeCc", SwitchDemoGppe.memoryMapCc)
          , ("SwitchDemoGppeGppe", SwitchDemoGppe.memoryMapGppe)
          , ("TimeWb", TimeWb.timeWbMm)
          , ("WbToDfTest", WbToDf.dutMM)
          , ("VexRiscv", vexRiscvTestMM)
          , ("SoftUgnDemoCc", SoftUgnDemo.memoryMapCc)
          , ("SoftUgnDemoMu", SoftUgnDemo.memoryMapMu)
          , ("SoftUgnDemoGppe", SoftUgnDemo.memoryMapGppe)
          ]

    memMapDir <- runIO $ do
      root <- findParentContaining "cabal.project"
      let dir = root </> buildDir </> "memory_maps"
      -- clean existing memory maps
      removePathForcibly dir

      createDirectoryIfMissing True dir
      pure dir

    let convertedTrees = map (\m -> convert m.tree) (snd <$> memoryMaps)
    let normalizedTrees = map normalizeRelTree convertedTrees

    let absResults =
          flip map (memoryMaps `zip` normalizedTrees) $
            \((_name, mm), normalised) ->
              makeAbsolute mm.deviceDefs (0x0000_0000, 0xFFFF_FFFF) normalised

    forM_ (memoryMaps `zip` absResults) $ \((mmName, mm), (absTree, errors)) -> do
      if not $ null errors
        then do
          -- report errors
          forM_ errors $ \err -> do
            reportError (getErrorMessage err)
          pure ()
        else do
          -- output JSON

          let json = Json.memoryMapJson Json.LocationSeparate mm.deviceDefs absTree
          let jsonPath = memMapDir </> mmName <.> "json"
          runIO $ BS.writeFile jsonPath $ Json.encode json

    pure []
 )
