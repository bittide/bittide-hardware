-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# HLINT ignore "Use list comprehension" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- | Check a memory map for valid absolute addresses and populate the memory map
with correct absolute addresses if they are missing.
-}
module Protocols.MemoryMap.Check.AbsAddress where

import Prelude

import Protocols.MemoryMap

import Control.Monad (forM, when)
import Control.Monad.Writer
import Data.List (sortOn)
import GHC.Stack (SrcLoc)

import qualified Data.Map.Strict as Map

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
      , location :: SrcLoc
      }
  deriving (Show)

data AbsNormData = AbsNormData
  { tags :: [(SrcLoc, String)]
  , path :: Path
  , absoluteAddr :: Address
  }
  deriving (Show)

type MemoryMapTreeAbsNorm =
  MemoryMapTreeAnn AbsNormData 'Normalized

runMakeAbsolute ::
  DeviceDefinitions ->
  AddressRange ->
  MemoryMapTreeRelNorm ->
  (MemoryMapTreeAbsNorm, [AddressError])
runMakeAbsolute ctx range tree = runWriter (makeAbsolute ctx range tree)

makeAbsolute ::
  DeviceDefinitions ->
  AddressRange ->
  MemoryMapTreeRelNorm ->
  Writer [AddressError] MemoryMapTreeAbsNorm
makeAbsolute ctx (start, size) (AnnDeviceInstance relData srcLoc deviceName) =
  checkAssertedAddr start relData.absoluteAddr relData.path $
    case Map.lookup deviceName ctx of
      Nothing -> error $ "DeviceDefinition " <> show deviceName <> " not found (" <> show srcLoc <> ")"
      Just def -> do
        let
          devSize = deviceSize def
          newDevInstance =
            AnnDeviceInstance
              (AbsNormData{tags = relData.tags, path = relData.path, absoluteAddr = start})
              srcLoc
              deviceName

        when (devSize > size) $ do
          let err =
                SizeExceedsError
                  { startAddr = start
                  , availableSize = size
                  , requestedSize = devSize
                  , path = relData.path
                  , location = srcLoc
                  }
          tell [err]

        pure newDevInstance
makeAbsolute ctx (start, size) (AnnInterconnect relData srcLoc comps0) =
  checkAssertedAddr start relData.absoluteAddr relData.path $ do
    comps2 <- forM componentList $ \((start', size'), comp0) -> do
      comp1 <- makeAbsolute ctx (start + start', size') comp0
      pure (start', comp1)

    pure
      ( AnnInterconnect
          (AbsNormData{tags = relData.tags, path = relData.path, absoluteAddr = start})
          srcLoc
          comps2
      )
 where
  comps1 = sortOn fst comps0
  compRelStart = relAddrs `zip` (drop 1 (relAddrs <> [size]))
   where
    relAddrs = fst <$> comps1

  compRelStartSize = map (\(start', nextStart) -> (start', nextStart - start')) compRelStart

  componentList = compRelStartSize `zip` (snd <$> comps1)

checkAssertedAddr ::
  Address ->
  Maybe (SrcLoc, Address) ->
  Path ->
  Writer [AddressError] a ->
  Writer [AddressError] a
checkAssertedAddr _ Nothing _ action = action
checkAssertedAddr addr (Just (srcLoc, asserted)) path action
  | addr == asserted = action
  | otherwise =
      do
        let
          err =
            AddressDifferentThanExpected
              { expected = asserted
              , actual = addr
              , path
              , location = srcLoc
              }
        res <- action
        tell [err]
        pure res
