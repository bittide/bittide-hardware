-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

{- | Logic for extracting memory maps from circuits in this project.
This is a separate module from MemoryMaps because of GHC's stage restrictions
around TemplateHaskell.
-}
module Bittide.Instances.MemoryMapLogic where

import Clash.Prelude

import Control.Monad (forM_)

import Bittide.Instances.Hitl.VexRiscv (cpuCircuit)
import Language.Haskell.TH (Q, reportError, runIO)
import Project.FilePath (buildDir, findParentContaining)
import Protocols.MemoryMap (MemoryMap, annotationSnd)
import Protocols.MemoryMap.Check (
  CheckConfiguration (..),
  MemoryMapValidationErrors (..),
  check,
  prettyPrintPath,
  shortLocation,
 )
import Protocols.MemoryMap.Check.AbsAddress (AbsAddressValidateError (..))
import Protocols.MemoryMap.Check.Overlap (OverlapError (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as L
import Protocols.MemoryMap.Json (memoryMapJson)
import Text.Printf (printf)

data MemMapProcessing = MemMapProcessing
  { name :: String
  , memMap :: MemoryMap
  , checkConfig :: CheckConfiguration
  , jsonOutput :: Maybe FilePath
  }

defaultCheckConfig :: CheckConfiguration
defaultCheckConfig =
  CheckConfiguration
    { startAddr = 0x0000_0000
    , endAddr = 0xFFFF_FFFF
    }

unSimOnly :: SimOnly a -> a
unSimOnly (SimOnly x) = x

memMaps :: [MemMapProcessing]
memMaps =
  [ MemMapProcessing
      { name = "VexRiscv"
      , memMap = unSimOnly $ annotationSnd @System cpuCircuit
      , checkConfig = defaultCheckConfig
      , jsonOutput = Just "vexriscv.json"
      }
  ]

processMemoryMaps :: Q ()
processMemoryMaps = do
  memMapDir <- runIO $ do
    root <- findParentContaining "cabal.project"
    let dir = root </> buildDir </> "memory_maps"
    createDirectoryIfMissing True dir
    pure dir

  forM_ memMaps $ \(MemMapProcessing{..}) -> do
    case check checkConfig memMap of
      Left MemoryMapValidationErrors{..} -> do
        let absErrorMsgs = flip L.map absAddrErrors $ \AbsAddressValidateError{..} ->
              let path' = prettyPrintPath path
                  component = case componentName of
                    Just name' -> name'
                    Nothing -> "interconnect " <> path'
               in printf
                    "Expected component %s at %08X but found %08X (%s)"
                    component
                    expected
                    got
                    (shortLocation location)

        let overlapErrorMsgs = flip L.map overlapErrors $ \case
              OverlapError{..} ->
                printf
                  "Component %s (%08X + %08X) overlaps with %s at address (%08X) (%s)"
                  (prettyPrintPath path)
                  startAddr
                  componentSize
                  (prettyPrintPath overlapsWith)
                  overlapsAt
                  (shortLocation location)
              SizeExceedsError{..} ->
                printf
                  "Component %s (%08X + %08X) exceeds available size %08X (%s)"
                  (prettyPrintPath path)
                  startAddr
                  requestedSize
                  availableSize
                  (shortLocation location)
        reportError (unlines $ absErrorMsgs <> overlapErrorMsgs)
      Right checkedMemMap -> do
        case jsonOutput of
          Nothing -> pure ()
          Just path -> do
            let json = memoryMapJson checkedMemMap
            runIO $ BS.writeFile (memMapDir </> path) (encode json)
            pure ()
    pure ()
