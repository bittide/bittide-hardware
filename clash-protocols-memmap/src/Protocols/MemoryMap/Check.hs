-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Validity checks performed on memory maps
module Protocols.MemoryMap.Check where

import Clash.Prelude

import Protocols

import Protocols.MemoryMap
import Protocols.MemoryMap.Check.AbsAddress
import Protocols.MemoryMap.FieldType (TypeName)
import Protocols.MemoryMap.TypeCollect

import GHC.Stack (SrcLoc (..))
import Language.Haskell.TH hiding (location)
import Text.Printf (printf)

import qualified Data.List as L
import qualified Data.Map.Strict as Map

{-

{- | A validated memory map, containing a 'MemoryMap' with absolute addressess
and the set of types used in device registers and their definitions.
-}
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

-- | Validate a 'MemoryMap' based on a configuration.
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

{- | Validate a 'MemoryMap' in TemplateHaskell, reporting any errors at compile
time.
-}
checkMemoryMapTH ::
  CheckConfiguration ->
  MemoryMap ->
  DecsQ
checkMemoryMapTH config mmap =
  case check config mmap of
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

-- fail "Something wrong with the MemoryMap"

prettyPrintPath :: ComponentPath -> String
prettyPrintPath Root = "root"
prettyPrintPath (InterconnectComponent idx path) = prettyPrintPath path <> "." <> show idx

{- | Validate a 'Circuit' in TemplateHaskell, reporting any errors at compile
time.
-}
checkCircuitTH ::
  forall dom a b.
  (KnownDomain dom, NFDataX (Fwd a), NFDataX (Bwd b)) =>
  CheckConfiguration ->
  ((HiddenClockResetEnable dom) => Circuit (MemoryMapped a) b) ->
  DecsQ
checkCircuitTH config circuitFn =
  let
    SimOnly ann = annotation @dom circuitFn
   in
    checkMemoryMapTH config ann

{- | Validate a 'Circuit' which contains memory-map information in its 'snd'
input in TemplateHaskell, reporting any errors at compile time.
-}
checkCircuitSndTH ::
  forall dom a b c.
  (KnownDomain dom, NFDataX (Fwd a), NFDataX (Fwd b), NFDataX (Bwd c)) =>
  CheckConfiguration ->
  ((HiddenClockResetEnable dom) => Circuit (a, MemoryMapped b) c) ->
  DecsQ
checkCircuitSndTH config circuitFn =
  let
    circ = withClockResetEnable clockGen resetGen enableGen circuitFn
    SimOnly ann = annotationSnd' circ
   in
    checkMemoryMapTH config ann


-}
