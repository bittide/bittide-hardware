-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Protocols.MemoryMap.Check where

import Clash.Prelude

import Protocols

import Protocols.MemoryMap
import Protocols.MemoryMap.Check.AbsAddress
import Protocols.MemoryMap.Check.Overlap
import Protocols.MemoryMap.FieldType (TypeName)
import Protocols.MemoryMap.TypeCollect

import GHC.Stack (SrcLoc (..))
import Language.Haskell.TH hiding (location)
import Text.Printf (printf)

import qualified Data.List as L
import qualified Data.Map.Strict as Map

data MemoryMapValid = MemoryMapValid
  { validMap :: MemoryMap
  , validTypes :: Map.Map TypeName TypeDescription
  }
  deriving (Show)

data MemoryMapValidationErrors = MemoryMapValidationErrors
  { absAddrErrors :: [AbsAddressValidateError]
  , overlapErrors :: [OverlapError]
  }
  deriving (Show)

data CheckConfiguration = CheckConfiguration
  { startAddr :: Address
  , endAddr :: Address
  }

check ::
  CheckConfiguration -> MemoryMap -> Either MemoryMapValidationErrors MemoryMapValid
check CheckConfiguration{..} mm@MemoryMap{..} =
  if not (null absErrors) || not (null overlapErrors)
    then
      Left
        $ MemoryMapValidationErrors
          { absAddrErrors = absErrors
          , overlapErrors
          }
    else Right $ MemoryMapValid{validMap = mm{tree = absTree}, validTypes = types}
 where
  absMm = mm{tree = absTree}
  absTree = makeAbsolute 0x0 tree
  absErrors = validateAbsAddresses Root absTree tree
  overlapErrors = checkOverlap (startAddr, endAddr) absMm
  types = collect absMm

shortLocation :: SrcLoc -> String
shortLocation SrcLoc{..} = srcLocFile <> ":" <> show srcLocStartLine <> ":" <> show srcLocStartCol

checkMemoryMap ::
  forall dom a b.
  (KnownDomain dom, NFDataX (Fwd a), NFDataX (Bwd b)) =>
  CheckConfiguration ->
  ((HiddenClockResetEnable dom) => Circuit (MemoryMapped a) b) ->
  DecsQ
checkMemoryMap config circuitFn = do
  let
    SimOnly ann = annotation @dom circuitFn

  case check config ann of
    Right _mmValid -> pure []
    Left MemoryMapValidationErrors{..} -> do
      let absErrorMsgs = flip L.map absAddrErrors $ \AbsAddressValidateError{..} ->
            let path' = prettyPrintPath path
                component = case componentName of
                  Just name -> name
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

      pure []
 where
  -- fail "Something wrong with the MemoryMap"

  prettyPrintPath :: ComponentPath -> String
  prettyPrintPath Root = "root"
  prettyPrintPath (InterconnectComponent idx path) = prettyPrintPath path <> "." <> show idx
