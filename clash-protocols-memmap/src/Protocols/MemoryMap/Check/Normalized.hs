-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Protocols.MemoryMap.Check.Normalized where

import Prelude

import GHC.Stack (SrcLoc)

import qualified Data.Bifunctor

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
bad at propagating constants properly, so designers should only /produce/
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

data RelNormData = RelNormData
  { tags :: [(SrcLoc, String)]
  , path :: Path
  , absoluteAddr :: Maybe (SrcLoc, Address)
  }
  deriving (Show)

type MemoryMapTreeRelNorm =
  MemoryMapTreeAnn RelNormData 'Normalized

type AbsAddressList = [(Path, Address)]

data Normalized = Normalized | NotNormalized

data MemoryMapTreeAnn ann (norm :: Normalized) where
  AnnInterconnect ::
    ann -> SrcLoc -> [(RelAddress, MemoryMapTreeAnn ann norm)] -> MemoryMapTreeAnn ann norm
  AnnDeviceInstance :: ann -> SrcLoc -> DeviceName -> MemoryMapTreeAnn ann 'Normalized
  AnnWithName ::
    ann ->
    SrcLoc ->
    String ->
    MemoryMapTreeAnn ann norm ->
    MemoryMapTreeAnn ann 'NotNormalized
  AnnWithTag ::
    ann ->
    SrcLoc ->
    String ->
    MemoryMapTreeAnn ann norm ->
    MemoryMapTreeAnn ann 'NotNormalized
  AnnAbsAddr ::
    ann ->
    SrcLoc ->
    Address ->
    MemoryMapTreeAnn ann norm ->
    MemoryMapTreeAnn ann 'NotNormalized
  AnnNormWrapper :: MemoryMapTreeAnn ann 'Normalized -> MemoryMapTreeAnn ann 'NotNormalized

instance (Show ann) => Show (MemoryMapTreeAnn ann norm) where
  show (AnnInterconnect ann _srcLoc comps) = "Interconnect(" <> show ann <> ", " <> show comps <> ")"
  show (AnnDeviceInstance ann _srcLoc deviceName) = "DeviceInstance(" <> show ann <> ", " <> deviceName <> ")"
  show (AnnWithName ann _srcLoc name tree) = "WithName(" <> show ann <> ", " <> show name <> ", " <> show tree <> ")"
  show (AnnWithTag ann _srcLoc tag tree) = "WithTag(" <> show ann <> ", " <> show tag <> ", " <> show tree <> ")"
  show (AnnAbsAddr ann _srcLoc addr tree) = "AbsAddr(" <> show ann <> ", " <> show addr <> ", " <> show tree <> ")"
  show (AnnNormWrapper tree) = show tree

convert :: MemoryMapTree -> MemoryMapTreeAnn () 'NotNormalized
convert (WithName srcLoc name tree) = AnnWithName () srcLoc name (convert tree)
convert (WithAbsAddr srcLoc addr tree) = AnnAbsAddr () srcLoc addr (convert tree)
convert (WithTag srcLoc tag tree) = AnnWithTag () srcLoc tag (convert tree)
convert (Interconnect srcLoc comps) = AnnInterconnect () srcLoc comps'
 where
  comps' = Data.Bifunctor.second convert <$> comps
convert (DeviceInstance srcLoc deviceName) = AnnNormWrapper (AnnDeviceInstance () srcLoc deviceName)

normalizeRelTree :: MemoryMapTreeAnn () norm -> MemoryMapTreeRelNorm
normalizeRelTree = go [] 0 [] Nothing Nothing
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
  go path n tags prevName prevAddr (AnnDeviceInstance () srcLoc name) =
    AnnDeviceInstance
      (RelNormData{tags, path = reverse newName, absoluteAddr = prevAddr})
      srcLoc
      name
   where
    newName = nextName path n prevName
  go path n tags prevName prevAddr (AnnInterconnect () srcLoc comps) =
    AnnInterconnect
      (RelNormData{tags, path = reverse path', absoluteAddr = prevAddr})
      srcLoc
      comps'
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
absAddresses (AnnDeviceInstance (RelNormData _ _ Nothing) _ _) = []
absAddresses (AnnDeviceInstance (RelNormData _ path (Just (_, addr))) _ _) = [(path, addr)]
absAddresses (AnnInterconnect (RelNormData _ _ Nothing) _ comps) = comps >>= (absAddresses . snd)
absAddresses (AnnInterconnect (RelNormData _ path (Just (_, addr))) _ comps) =
  let rest = comps >>= (absAddresses . snd)
   in (path, addr) : rest
