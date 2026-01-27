-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Hitl.Utils.MemoryMap where

import Prelude

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (
  DeviceDefinition (..),
  MemoryMap (..),
  Name (..),
  NamedLoc (..),
  PathComp (..),
  Register (..),
  convert,
  normalizeRelTree,
 )
import Protocols.MemoryMap.Check (
  MemoryMapTreeAnn (..),
  Normalized (Normalized),
 )
import Protocols.MemoryMap.Check.AbsAddress (
  AbsNormData (absoluteAddr, path),
  MemoryMapTreeAbsNorm,
  runMakeAbsolute,
 )
import Text.Show.Pretty (ppShow)

import qualified Data.Map.Strict as M

type CanonicalTreeAbsNorm = MemoryMapTreeAnn (Maybe String, AbsNormData) 'Normalized

{- | Finds the address for a given path.

A "path" is a list of 'String's, where each item in the list is an interconnect
number, device name, or register name. Paths may end in a device name or register
name, but should not end with an interconnect number. For example, @["0", \"SwitchDemoPE\",
"buffer"]@ is a path that ends in a register name, and @["0", \"CaptureUgn0\"]@ is
a path that ends in a device name.

Caveat(s):

Device names are "canonicalized", where this means that if there are multiple
devices with the same name connected to the same interconnect, then each of them
is suffixed with their index. For instance, if your memory map looks like:

> interconnect
>   - DeviceA @ 0b000
>   - DeviceB @ 0b001
>   - DeviceC @ 0b010
>   - DeviceA @ 0b011
>   - DeviceA @ 0b100

Then after canonicalization, it will look like

> interconnect
>   - DeviceA0 @ 0b000
>   - DeviceB  @ 0b001
>   - DeviceC  @ 0b010
>   - DeviceA1 @ 0b011
>   - DeviceA2 @ 0b100

This is so that any path the function attempts to follow can only refer to one
location in memory.
-}
getPathAddress ::
  (HasCallStack, Num a) =>
  -- | Memory map to find the path in
  MemoryMap ->
  -- | The path to search for in the memory map
  [String] ->
  a
getPathAddress mm = traverseTree canonicalTree
 where
  canonicalTree = canonicalizeMemoryMapTree mm

  showPathComponent :: PathComp -> String
  showPathComponent (PathName _ s) = s
  showPathComponent (PathUnnamed n) = show n

  getTreeName :: (HasCallStack) => CanonicalTreeAbsNorm -> String
  getTreeName (AnnInterconnect (_, absData) _ _) = showPathComponent (last absData.path)
  getTreeName (AnnDeviceInstance _ _ name) = name

  traverseTree :: (HasCallStack, Num a) => CanonicalTreeAbsNorm -> [String] -> a
  traverseTree (AnnInterconnect _ _ _) [] = error "Empty path given!"
  traverseTree (AnnInterconnect (_, absData) _ _) [name1] =
    let name0 = showPathComponent (last absData.path)
     in if name0 /= name1
          then error [i|Mismatch on interconnect name! Expected #{name0}, found #{name1}.|]
          else fromIntegral absData.absoluteAddr
  traverseTree
    (AnnInterconnect (_, absData) _ (fmap snd -> components))
    (name1 : next : t) =
      let name0 = showPathComponent (last absData.path)
       in if name0 == name1
            then case find (\tree -> next == getTreeName tree) components of
              Just comp -> traverseTree comp t
              Nothing -> error [i|Failed to find device #{next} in interconnect.|]
            else error [i|Mismatch on interconnect name! Expected #{name0}, found #{name1}.|]
  traverseTree (AnnDeviceInstance (_, absData) _ _) [] = fromIntegral absData.absoluteAddr
  traverseTree (AnnDeviceInstance _ _ name) (a : b : c) =
    error
      [i|Cannot index into #{name} farther than #{a}, but path continues: #{ppShow $ b : c}|]
  traverseTree (AnnDeviceInstance (oldName, absData) _ devName) [regName] =
    case find (\regNL -> regName == regNL.name.name) devDef.registers of
      Just regNL -> fromIntegral (absData.absoluteAddr + regNL.value.address)
      Nothing -> error [i|Failed to find register #{regName} in device #{devName}|]
   where
    devDef :: DeviceDefinition
    devDef =
      case mm.deviceDefs M.!? fromMaybe devName oldName of
        Just d -> d
        Nothing -> error [i|Device definition for #{devName} not found in memory map.|]

canonicalizeMemoryMapTree :: (HasCallStack) => MemoryMap -> CanonicalTreeAbsNorm
canonicalizeMemoryMapTree mm = canonicalizeDevNames annAbsTree0
 where
  annRelTree = convert mm.tree
  annRelNormTree = normalizeRelTree annRelTree
  (annAbsTree0, _) = runMakeAbsolute mm.deviceDefs (0x0000_0000, 0xFFFF_FFFF) annRelNormTree

  canonicalizeDevNames :: (HasCallStack) => MemoryMapTreeAbsNorm -> CanonicalTreeAbsNorm
  canonicalizeDevNames (AnnDeviceInstance a b c) = AnnDeviceInstance (Nothing, a) b c
  canonicalizeDevNames (AnnInterconnect ann srcLoc relsAndMMs) =
    AnnInterconnect (Nothing, ann) srcLoc $
      doCanonicalization (M.fromList $ zip deviceNames (repeat 0)) relsAndMMs
   where
    components :: [MemoryMapTreeAbsNorm]
    components = snd <$> relsAndMMs
    deviceNames :: [String]
    deviceNames = mapMaybe getDeviceName components
    deviceNamesMult :: M.Map String Bool
    deviceNamesMult = M.fromListWith (\_ _ -> True) $ zip deviceNames (repeat False)

    getDeviceName :: (HasCallStack) => MemoryMapTreeAbsNorm -> Maybe String
    getDeviceName (AnnDeviceInstance _ _ name) = Just name
    getDeviceName _ = Nothing

    doCanonicalization ::
      (HasCallStack) =>
      M.Map String Integer ->
      [(Integer, MemoryMapTreeAbsNorm)] ->
      [(Integer, CanonicalTreeAbsNorm)]
    doCanonicalization _ [] = []
    doCanonicalization counts ((addr, AnnInterconnect ann1 srcLoc1 subtree) : t) =
      ( addr
      , AnnInterconnect (Nothing, ann1) srcLoc1 (bimap id canonicalizeDevNames <$> subtree)
      )
        : doCanonicalization counts t
    doCanonicalization counts0 ((addr, AnnDeviceInstance ann1 srcLoc1 devName0) : t) =
      (addr, AnnDeviceInstance (extraAnn, ann1) srcLoc1 devName1) : doCanonicalization counts1 t
     where
      needsSuffix =
        case deviceNamesMult M.!? devName0 of
          Nothing ->
            error [i|Device name #{devName0} not found in device names map during canonicalization.|]
          Just b -> b

      extraAnn = if needsSuffix then Just devName0 else Nothing

      curCount =
        case counts0 M.!? devName0 of
          Nothing -> error [i|Device name #{devName0} not found in counts map during canonicalization.|]
          Just c -> c

      devName1 = if needsSuffix then devName0 <> show curCount else devName0
      counts1 =
        if needsSuffix
          then M.adjust succ devName0 counts0
          else counts0
