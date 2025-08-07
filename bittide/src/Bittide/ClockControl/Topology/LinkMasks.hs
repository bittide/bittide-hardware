-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Utilities for converting topology data structures to link masks.
module Bittide.ClockControl.Topology.LinkMasks (linkMasks, linkMask) where

import Clash.Prelude

import Bittide.ClockControl.Topology (Topology, hasEdge)
import GHC.Stack (HasCallStack)

import qualified Bittide.ClockControl.Topology as Topology
import qualified Data.String.Interpolate as I

{- | A bunch of trivial constraints that are satisfied by any realistic
node setup. Extracted into a constraint synonym to avoid repetition.
-}
type MaskConstraints nNodes nLinks =
  ( 1 <= nNodes
  , 1 <= nLinks
  , CLog 2 nLinks + 1 <= 64
  , CLog 2 nNodes + 1 <= 64
  , KnownNat nNodes
  , KnownNat nLinks
  , HasCallStack
  )

linkMasks ::
  forall nNodes nLinks.
  (MaskConstraints nNodes nLinks) =>
  Vec nNodes (Vec nLinks (Index nNodes)) ->
  Topology ->
  Vec nNodes (BitVector nLinks)
linkMasks setup topology
  | Topology.size topology > natToNum @nNodes =
      error
        [I.i|
        You requested #{natToInteger @nNodes} link masks, but the topology has more
        nodes than that (#{Topology.size topology}). This is probably not what
        you want.
        |]
  | otherwise = pack . linkMask setup topology . numConvert <$> indicesI

-- | Determines whether a link is active for a given FPGA and topology.
isActiveLink ::
  (MaskConstraints nNodes nLinks) =>
  Vec nNodes (Vec nLinks (Index nNodes)) ->
  Topology ->
  Index nNodes ->
  Index nLinks ->
  Bool
isActiveLink setup topology fpgaNr linkNr =
  hasEdge topology (numConvert sourceFpgaNr) (numConvert fpgaNr)
 where
  links = setup !! fpgaNr
  sourceFpgaNr = links !! linkNr

{- | Determines the link mask of a particular node.

>>> import Data.Graph
>>> import Clash.Prelude
>>> import Bittide.ClockControl.Topology
>>> let graph = complete 3
>>> :set -fplugin=GHC.TypeLits.Extra.Solver
>>> :{
setup :: Vec 8 (Vec 7 (Index 8))
setup =
     (3 :> 2 :> 4 :> 5 :> 6 :> 7 :> 1 :> Nil)
  :> (2 :> 3 :> 5 :> 6 :> 7 :> 4 :> 0 :> Nil)
  :> (1 :> 0 :> 6 :> 7 :> 4 :> 5 :> 3 :> Nil)
  :> (0 :> 1 :> 7 :> 4 :> 5 :> 6 :> 2 :> Nil)
  :> (7 :> 6 :> 0 :> 3 :> 2 :> 1 :> 5 :> Nil)
  :> (6 :> 7 :> 1 :> 0 :> 3 :> 2 :> 4 :> Nil)
  :> (5 :> 4 :> 2 :> 1 :> 0 :> 3 :> 7 :> Nil)
  :> (4 :> 5 :> 3 :> 2 :> 1 :> 0 :> 6 :> Nil)
  :> Nil
:}

>>> pack (linkMask @8 @7 setup graph 0)
0b010_0001
>>> pack (linkMask @8 @7 setup graph 1)
0b100_0001
>>> pack (linkMask @8 @7 setup graph 2)
0b110_0000
-}
linkMask ::
  forall nNodes nLinks.
  (MaskConstraints nNodes nLinks) =>
  Vec nNodes (Vec nLinks (Index nNodes)) ->
  Topology ->
  Index nNodes ->
  Vec nLinks Bool
linkMask setup topology fpgaNr = isActiveLink setup topology fpgaNr <$> indicesI
