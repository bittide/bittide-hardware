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

import Data.List (sortBy)
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

type MemoryMapTreeAbsNorm =
  MemoryMapTreeAnn ([(SrcLoc, String)], Path, Address) 'Normalized

makeAbsolute ::
  DeviceDefinitions ->
  AddressRange ->
  MemoryMapTreeRelNorm ->
  (MemoryMapTreeAbsNorm, [AddressError])
makeAbsolute ctx (start, size) (AnnDeviceInstance (tags, path, assertedAddr) srcLoc deviceName) =
  checkAssertedAddr start assertedAddr path $
    case Map.lookup deviceName ctx of
      Nothing -> error $ "DeviceDefinition " <> show deviceName <> " not found (" <> show srcLoc <> ")"
      Just def ->
        let
          devSize = deviceSize def
          newDevInstance = AnnDeviceInstance (tags, path, start) srcLoc deviceName
         in
          if devSize > size
            then
              let err =
                    SizeExceedsError
                      { startAddr = start
                      , availableSize = size
                      , requestedSize = devSize
                      , path
                      , location = srcLoc
                      }
               in (newDevInstance, [err])
            else (newDevInstance, [])
makeAbsolute ctx (start, size) (AnnInterconnect (tags, path, assertedAddr) srcLoc comps0) =
  checkAssertedAddr start assertedAddr path $
    let (unzip -> (comps2, concat -> errs)) = flip map componentList $ \((start', size'), comp) -> do
          let (comp1, errs1) = makeAbsolute ctx (start + start', size') comp
          ((start', comp1), errs1)
     in (AnnInterconnect (tags, path, start) srcLoc comps2, errs)
 where
  comps1 = sortBy (\(a, _) (b, _) -> a `compare` b) comps0
  compRelStart = relAddrs `zip` (drop 1 $ (relAddrs <> [size]))
   where
    relAddrs = fst <$> comps1

  compRelStartSize = map (\(start', nextStart) -> (start', nextStart - start')) compRelStart

  componentList = compRelStartSize `zip` (snd <$> comps1)

checkAssertedAddr ::
  Address -> Maybe (SrcLoc, Address) -> Path -> (a, [AddressError]) -> (a, [AddressError])
checkAssertedAddr _ Nothing _ action = action
checkAssertedAddr addr (Just (srcLoc, asserted)) path action
  | addr == asserted = action
  | otherwise =
      let
        err =
          AddressDifferentThanExpected
            { expected = asserted
            , actual = addr
            , path
            , location = srcLoc
            }
        (res, errors) = action
       in
        (res, err : errors)
