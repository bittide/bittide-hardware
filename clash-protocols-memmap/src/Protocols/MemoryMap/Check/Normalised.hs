-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}

module Protocols.MemoryMap.Check.Normalised where

import qualified Data.Bifunctor
import GHC.Stack (SrcLoc)
import Prelude

data PathComp
  = PathName SrcLoc String
  | PathUnnamed Integer

instance Show PathComp where
  show (PathName _ s) = s
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
  | WithTag SrcLoc String MemoryMapTree
  | Interconnect SrcLoc [(RelAddress, MemoryMapTree)]
  | DeviceInstance SrcLoc DeviceName
  deriving (Show)

type MemoryMapTreeRelNorm =
  MemoryMapTreeAnn ([(SrcLoc, String)], Path, Maybe (SrcLoc, Address)) 'Normalised

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
  AnnWithTag ::
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
  show (AnnWithTag ann _srcLoc tag tree) = "WithTag(" <> show ann <> ", " <> show tag <> ", " <> show tree <> ")"
  show (AnnAbsAddr ann _srcLoc addr tree) = "AbsAddr(" <> show ann <> ", " <> show addr <> ", " <> show tree <> ")"
  show (AnnNormWrapper tree) = show tree

convert :: MemoryMapTree -> MemoryMapTreeAnn () 'NotNormalised
convert (WithName srcLoc name tree) = AnnWithName () srcLoc name (convert tree)
convert (WithAbsAddr srcLoc addr tree) = AnnAbsAddr () srcLoc addr (convert tree)
convert (WithTag srcLoc tag tree) = AnnWithTag () srcLoc tag (convert tree)
convert (Interconnect srcLoc comps) = AnnInterconnect () srcLoc comps'
 where
  comps' = Data.Bifunctor.second convert <$> comps
convert (DeviceInstance srcLoc deviceName) = AnnNormWrapper (AnnDeviceInstance () srcLoc deviceName)

normaliseRelTree :: MemoryMapTreeAnn () norm -> MemoryMapTreeRelNorm
normaliseRelTree = go [] 0 [] Nothing Nothing
 where
  nextName :: [PathComp] -> Integer -> Maybe (SrcLoc, String) -> Path
  nextName path n Nothing = PathUnnamed n : path
  nextName path _ (Just (srcLoc, n)) = PathName srcLoc n : path

  go ::
    [PathComp] ->
    Integer ->
    [(SrcLoc, String)] ->
    Maybe (SrcLoc, String) ->
    Maybe (SrcLoc, Address) ->
    MemoryMapTreeAnn () norm ->
    MemoryMapTreeRelNorm
  go path n tags prevName prevAddr (AnnDeviceInstance () srcLoc name) = AnnDeviceInstance (tags, reverse newName, prevAddr) srcLoc name
   where
    newName = nextName path n prevName
  go path n tags prevName prevAddr (AnnInterconnect () srcLoc comps) = AnnInterconnect (tags, reverse path', prevAddr) srcLoc comps'
   where
    path' = nextName path n prevName
    comps' = flip map ([0 ..] `zip` comps) $ \(i, (pre, comp)) ->
      (pre, go path' i [] Nothing Nothing comp)
  go path _n tags _prevName prevAddr (AnnWithName () srcLoc name tree') =
    go path 0 tags (Just (srcLoc, name)) prevAddr tree'
  go path n tags prevName prevAddr (AnnWithTag () srcLoc tag tree') =
    go path n ((srcLoc, tag) : tags) prevName prevAddr tree'
  go path n tags prevName _prevAddr (AnnAbsAddr () srcLoc addr tree') =
    go path n tags prevName (Just (srcLoc, addr)) tree'
  go path n tags prevName prevAddr (AnnNormWrapper tree') = go path n tags prevName prevAddr tree'

absAddresses :: MemoryMapTreeRelNorm -> AbsAddressList
absAddresses (AnnDeviceInstance (_, _, Nothing) _ _) = []
absAddresses (AnnDeviceInstance (_, path, Just (_, addr)) _ _) = [(path, addr)]
absAddresses (AnnInterconnect (_, _, Nothing) _ comps) = comps >>= (absAddresses . snd)
absAddresses (AnnInterconnect (_, path, Just (_, addr)) _ comps) =
  let rest = comps >>= (absAddresses . snd)
   in (path, addr) : rest
