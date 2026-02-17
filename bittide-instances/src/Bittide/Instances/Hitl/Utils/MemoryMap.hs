-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Hitl.Utils.MemoryMap where

import Prelude

import Control.Monad (unless, when)
import Data.Either.Extra (maybeToEither)
import Data.List (unsnoc)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (
  Address,
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

import qualified Data.Map.Strict as M

type CanonicalTreeAbsNorm = MemoryMapTreeAnn (Maybe String, AbsNormData) 'Normalized

bindLeft :: Either a b -> (a -> Either c b) -> Either c b
bindLeft a fn = case a of
  Right val -> return val
  Left val -> fn val

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
  Either String a
getPathAddress mm pathWithName = do
  (pfx, name) <- maybeToEither "Empty path given!" (unsnoc pathWithName)
  children <- listChildren mm pfx
  case filter (\(_, childName) -> childName == name) children of
    [] -> Left [i|Prefix #{pfx} has no child #{name}|]
    [(addr, _)] -> Right $ fromInteger addr
    many -> error [i|This is a bug! Prefix #{pfx} has multiple children: #{snd <$> many}|]

{- | Given a memorymap and a path, it returns a list af addresses and names of the children of that path.
For devices it will return the addresses and names of all registers in the device.
For interconnects it will return the addresses and names of all directly connected devices and interconnects.
-}
listChildren ::
  (HasCallStack) =>
  -- | Memory map to find the path in
  MemoryMap ->
  -- | The path to search for in the memory map
  [String] ->
  -- | A list of addresses and names of the children of the path
  Either String [(Integer, String)]
listChildren mm path = case (path, canonicalTree) of
  ([], self@(AnnInterconnect (_, absData) _ _)) -> Right [(absData.absoluteAddr, getName self)]
  ([], AnnDeviceInstance (oldName, absData) _ devName) -> Right [(absData.absoluteAddr, fromMaybe devName oldName)]
  (nonEmptyPath, _) -> getChildren nonEmptyPath canonicalTree
 where
  canonicalTree :: CanonicalTreeAbsNorm
  canonicalTree = canonicalizeMemoryMapTree mm

  showPathComponent :: PathComp -> String
  showPathComponent (PathName _ s) = s
  showPathComponent (PathUnnamed n) = show n

  getName :: CanonicalTreeAbsNorm -> String
  getName (AnnInterconnect (_, absData) _ _) = showPathComponent $ last absData.path
  getName (AnnDeviceInstance _ _ devName) = devName

  getAbsRegs :: Address -> NamedLoc Register -> (Integer, String)
  getAbsRegs baseAddress register =
    (baseAddress + register.value.address, register.name.name)

  returnInterconnectChildren :: CanonicalTreeAbsNorm -> (Integer, String)
  returnInterconnectChildren node = (addr, getName node)
   where
    addr = case node of
      (AnnInterconnect (_, absData) _ _) -> absData.absoluteAddr
      (AnnDeviceInstance (_, absData) _ _) -> absData.absoluteAddr

  getChildren ::
    (HasCallStack) =>
    [String] ->
    CanonicalTreeAbsNorm ->
    Either String [(Integer, String)]
  getChildren [] ann = Left [i|This is a bug! Empty path on component #{ann}|]
  getChildren (lookFor : rest) (AnnInterconnect (_, absData) _ successors) = do
    when (lookFor /= interconnectName) $
      Left [i|Interconnect name mismatch! Found #{interconnectName}, expected #{lookFor}|]
    if null rest
      then Right $ returnInterconnectChildren . snd <$> successors
      else trySuccessors $ snd <$> successors
   where
    interconnectName = showPathComponent $ last absData.path
    trySuccessors :: [CanonicalTreeAbsNorm] -> Either String [(Integer, String)]
    trySuccessors [] = Left [i|Matching section #{rest} on component #{lookFor} in #{path} failed.|]
    trySuccessors (ann : restSucc) = getChildren rest ann `bindLeft` (const $ trySuccessors restSucc)
  getChildren (lookFor : rest) (AnnDeviceInstance (oldName, absData) _ devName) = do
    when (lookFor /= devName) $
      Left [i|Device name mismatch! Found #{devName}, expected #{lookFor}|]
    unless (null rest) $
      Left [i|Path #{path} attempts to find children of a register in device #{devName}|]
    let deviceName = fromMaybe devName oldName
    deviceDef <-
      maybeToEither
        [i|Device definition for #{deviceName} not find in memory map.|]
        $ mm.deviceDefs M.!? deviceName
    return $ getAbsRegs absData.absoluteAddr <$> deviceDef.registers

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
    doCanonicalization counts ((addr, interconnect@(AnnInterconnect _ _ _)) : t) =
      (addr, canonicalizeDevNames interconnect) : doCanonicalization counts t
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
