-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}

module Protocols.MemoryMap.Check.Normalised where

import qualified Data.Bifunctor
import GHC.Stack (SrcLoc)
import Prelude

data PathComp
  = PathName String
  | PathUnnamed Integer

instance Show PathComp where
  show (PathName s) = s
  show (PathUnnamed n) = "#" <> show n

type Path = [PathComp]

type Address = Integer
type RelAddress = Integer
type Prefix = Integer
type DeviceName = String

{- | A tree structure that describes the memory map of a device. Its definitions
are using non-translatable constructs on purpose: Clash is currently pretty
bad at propagating contants properly, so designers should only /produce/
memory maps, not rely on constant folding to be able to extract addresses
from them to use in their designs.
-}
data MemoryMapTree
  = WithName SrcLoc String MemoryMapTree
  | WithAbsAddr SrcLoc Address MemoryMapTree
  | Interconnect SrcLoc [(RelAddress, MemoryMapTree)]
  | DeviceInstance SrcLoc DeviceName
  deriving (Show)

type MemoryMapTreePathAbsAddr =
  MemoryMapTreeAnn ((Maybe SrcLoc, Path), Maybe (SrcLoc, Address)) 'Normalised

type AbsAddressList = [(Path, Address)]

data Normalised = Normalised | NotNormalised

data MemoryMapTreeAnn ann (norm :: Normalised) where
  AnnInterconnect ::
    ann -> SrcLoc -> [(RelAddress, MemoryMapTreeAnn ann norm)] -> MemoryMapTreeAnn ann norm
  AnnDeviceInstance :: ann -> SrcLoc -> DeviceName -> MemoryMapTreeAnn ann 'Normalised
  AnnWithName ::
    ann ->
    SrcLoc ->
    String ->
    MemoryMapTreeAnn ann norm ->
    MemoryMapTreeAnn ann 'NotNormalised
  AnnAbsAddr ::
    ann ->
    SrcLoc ->
    Address ->
    MemoryMapTreeAnn ann norm ->
    MemoryMapTreeAnn ann 'NotNormalised
  AnnNormWrapper :: MemoryMapTreeAnn ann 'Normalised -> MemoryMapTreeAnn ann 'NotNormalised

instance (Show ann) => Show (MemoryMapTreeAnn ann norm) where
  show (AnnInterconnect ann _srcLoc comps) = "Interconnect(" <> show ann <> ", " <> show comps <> ")"
  show (AnnDeviceInstance ann _srcLoc deviceName) = "DeviceInstance(" <> show ann <> ", " <> deviceName <> ")"
  show (AnnWithName ann _srcLoc name tree) = "WithName(" <> show ann <> ", " <> show name <> ", " <> show tree <> ")"
  show (AnnAbsAddr ann _srcLoc addr tree) = "AbsAddr(" <> show ann <> ", " <> show addr <> ", " <> show tree <> ")"
  show (AnnNormWrapper tree) = show tree

convert :: MemoryMapTree -> MemoryMapTreeAnn () 'NotNormalised
convert (WithName srcLoc name tree) = AnnWithName () srcLoc name (convert tree)
convert (WithAbsAddr srcLoc addr tree) = AnnAbsAddr () srcLoc addr (convert tree)
convert (Interconnect srcLoc comps) = AnnInterconnect () srcLoc comps'
 where
  comps' = Data.Bifunctor.second convert <$> comps
convert (DeviceInstance srcLoc deviceName) = AnnNormWrapper (AnnDeviceInstance () srcLoc deviceName)

fillPathsAndAddrs :: MemoryMapTreeAnn () norm -> MemoryMapTreePathAbsAddr
fillPathsAndAddrs = go [] 0 Nothing Nothing
 where
  nextName :: [PathComp] -> Integer -> Maybe (SrcLoc, String) -> (Maybe SrcLoc, Path)
  nextName path n Nothing = (Nothing, PathUnnamed n : path)
  nextName path _ (Just (srcLoc, n)) = (Just srcLoc, PathName n : path)

  go ::
    [PathComp] ->
    Integer ->
    Maybe (SrcLoc, String) ->
    Maybe (SrcLoc, Address) ->
    MemoryMapTreeAnn () norm ->
    MemoryMapTreePathAbsAddr
  go path n prevName prevAddr (AnnDeviceInstance () srcLoc name) = AnnDeviceInstance ((nameLoc, reverse newName), prevAddr) srcLoc name
   where
    (nameLoc, newName) = nextName path n prevName
  go path n prevName prevAddr (AnnInterconnect () srcLoc comps) = AnnInterconnect ((nameLoc, reverse path'), prevAddr) srcLoc comps'
   where
    (nameLoc, path') = nextName path n prevName
    comps' = flip map ([0 ..] `zip` comps) $ \(i, (pre, comp)) ->
      (pre, go path' i Nothing Nothing comp)
  go path _n _prevName prevAddr (AnnWithName () srcLoc name tree') =
    go path 0 (Just (srcLoc, name)) prevAddr tree'
  go path n prevName _prevAddr (AnnAbsAddr () srcLoc addr tree') =
    go path n prevName (Just (srcLoc, addr)) tree'
  go path n prevName prevAddr (AnnNormWrapper tree') = go path n prevName prevAddr tree'

absAddresses :: MemoryMapTreeAnn (Path, Maybe Address) 'Normalised -> AbsAddressList
absAddresses (AnnDeviceInstance (_, Nothing) _ _) = []
absAddresses (AnnDeviceInstance (path, Just addr) _ _) = [(path, addr)]
absAddresses (AnnInterconnect (_, Nothing) _ comps) = comps >>= (absAddresses . snd)
absAddresses (AnnInterconnect (path, Just addr) _ comps) =
  let rest = comps >>= (absAddresses . snd)
   in (path, addr) : rest
