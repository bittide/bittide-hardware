-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE NamedFieldPuns #-}

{- | Check a memory map for valid absolute addresses and populate the memory map
with correct absolute addresses if they are missing.
-}
module Protocols.MemoryMap.Check.AbsAddress where

import Prelude

import GHC.Stack (SrcLoc)

import qualified Data.Map.Strict as Map

import Protocols.MemoryMap
import qualified Data.Bifunctor

type AddressRange = (Address, Address)

data AddressError
  = SizeExceedsError
      { startAddr :: Address
      , availableSize :: Integer
      , requestedSize :: Integer
      , path :: Path
      , location :: SrcLoc
      }
  | AddressDifferentThanExpected
      { expected :: Address
      , actual :: Address
      , path :: Path
      , location :: SrcLoc }
  deriving (Show)

type MemoryMapTreeAbs = MemoryMapTreeAnn (Path, Address) 'Normalised

makeAbsolute :: DeviceDefinitions -> AddressRange -> MemoryMapTreePathAbsAddr -> (MemoryMapTreeAbs, [AddressError])
makeAbsolute ctx (start, end) (AnnDeviceInstance ((_, path), assertedAddr) srcLoc deviceName) =
  checkAssertedAddr start assertedAddr path $
  case Map.lookup deviceName ctx of
    Nothing -> error $ "DeviceDefinition " <> show deviceName <> " not found (" <> show srcLoc <> ")"
    Just def ->
      let
        availableSize = end - start
        devSize = deviceSize def
        newDevInstance = AnnDeviceInstance (path, start) srcLoc deviceName
      in if devSize > availableSize then
        let err = SizeExceedsError { startAddr = start, availableSize, requestedSize = devSize, path, location = srcLoc }
        in (newDevInstance, [err])
      else
        (newDevInstance, [])
makeAbsolute ctx (start, end) (AnnInterconnect ((_, path), assertedAddr) srcLoc comps) =
  checkAssertedAddr start assertedAddr path $
    let (unzip -> (comps1, concat -> errs)) = flip map (zip ranges comps) $ \((start', end'), (relStart, comp)) -> do
          let (comp1, errs1) =  makeAbsolute ctx (start', end') comp
          ((relStart, comp1), errs1)
    in
      (AnnInterconnect (path, start) srcLoc comps1, errs)
 where
  ranges
    | [] <- comps = []
    | [(relAddr, _comp)] <- comps = [(start + relAddr, end)]
    | (first, _) : rest <- comps =
        let
          relStarts = zip (first : (fst <$> rest)) $ (fst <$> rest) <> [end - start]
          startEnds = map (Data.Bifunctor.bimap (start +) (start +)) relStarts
        in map (Data.Bifunctor.bimap (start+) (end+)) startEnds


checkAssertedAddr :: Address -> Maybe (SrcLoc, Address) -> Path -> (a, [AddressError]) -> (a, [AddressError])
checkAssertedAddr _ Nothing _ action = action
checkAssertedAddr addr (Just (srcLoc, asserted)) path action
  | addr == asserted = action
  | otherwise =
      let
        err = AddressDifferentThanExpected
          { expected = asserted
          , actual = addr
          , path
          , location = srcLoc }
        (res, errors) = action
      in (res, err : errors)
