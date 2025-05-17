-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bittide.Instances.MemoryMaps where

import Prelude

import Bittide.Instances.Hitl.VexRiscv (vexRiscvTestMM)
import Bittide.Instances.Pnr.Ethernet (vexRiscvEthernetMM)
import Bittide.Instances.Pnr.Freeze (freezeMM)
import Bittide.Instances.Pnr.ProcessingElement (vexRiscvUartHelloMM)

import Protocols.MemoryMap
import Protocols.MemoryMap.Check
import Protocols.MemoryMap.Json (memoryMapJson)

import Project.FilePath (buildDir, findParentContaining)

import Control.Monad
import GHC.Stack (SrcLoc (..))
import Language.Haskell.TH (reportError, runIO)
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import Text.Printf (printf)

import qualified Data.Aeson.Encode.Pretty as Ae
import qualified Data.ByteString.Lazy as BS

$( do
    -------------------------------
    -- MEMORY MAPS               --
    --                           --
    -- Add new memory maps here  --
    -------------------------------
    let memoryMaps =
          [ ("Ethernet", vexRiscvEthernetMM)
          , ("ProcessingElement", vexRiscvUartHelloMM)
          , ("VexRiscv", vexRiscvTestMM)
          , ("Freeze", freezeMM)
          ]

    memMapDir <- runIO $ do
      root <- findParentContaining "cabal.project"
      let dir = root </> buildDir </> "memory_maps"
      createDirectoryIfMissing True dir
      pure dir

    let shortLocation s@SrcLoc{} = s.srcLocFile <> ":" <> show s.srcLocStartLine <> ":" <> show s.srcLocStartCol

    let convertedTrees = map (\m -> convert m.tree) (snd <$> memoryMaps)
    let normalisedTrees = map normaliseRelTree convertedTrees

    let absResults =
          flip map (memoryMaps `zip` normalisedTrees) $
            \((_name, mm), normalised) ->
              makeAbsolute mm.deviceDefs (0x0000_0000, 0xFFFF_FFFF) normalised

    forM_ (memoryMaps `zip` absResults) $ \((mmName, mm), (absTree, errors)) -> do
      if not $ null errors
        then do
          -- report errors
          forM_ errors $ \err -> do
            let
              msg =
                case err of
                  SizeExceedsError
                    { startAddr
                    , availableSize
                    , requestedSize
                    , path
                    , location
                    } ->
                      printf
                        "Component %s at address %08X with size %08X exceeds the available size %08X (%s)"
                        (show path)
                        startAddr
                        requestedSize
                        availableSize
                        (shortLocation location)
                  AddressDifferentThanExpected
                    { expected
                    , actual
                    , path
                    , location
                    } ->
                      printf
                        "Component %s has been given an absolute address %08X which is different from the computed one %08X (%s)"
                        (show path)
                        expected
                        actual
                        (shortLocation location)
            reportError msg
          pure ()
        else do
          -- output JSON

          let json = memoryMapJson mm.deviceDefs absTree
          let jsonPath = memMapDir </> mmName <.> "json"
          runIO $ BS.writeFile jsonPath (Ae.encodePretty json)

    -- forM_ errors $ \errs -> forM_ errs $ \err -> do
    --   let msg = case err of
    --     SizeExceedsError
    --       { startAddr
    --       , availableSize
    --       , requestedSize
    --       , path
    --       , location } ->
    --           printf "Component %s at address %08X with size %08X exceeds the available size %08X (%s)"
    --             (show path)
    --             startAddr
    --             requestedSize
    --             availableSize
    --             (shortLocation location)
    --     AddressDifferentThanExpected
    --       { expected
    --       , actual
    --       , path
    --       , location } ->
    --           printf ""
    --   reportError msg

    pure []
 )
