-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Setup
  ( FpgaCount
  , TransceiverWires
  , channelNames
  , clockPaths
  , fpgaSetup
  , linkMask
  , linkMasks
  ) where

import Clash.Prelude

import Bittide.Topology
import Data.Constraint ((:-)(..), Dict(..))
import Data.Constraint.Nat (leTrans)

-- | The number of FPGAs in the current setup
type FpgaCount = 2 :: Nat

-- | Data wires from/to transceivers. No logic should be inserted on these
-- wires. Should be considered asynchronous to one another - even though their
-- domain encodes them as related.
type TransceiverWires dom = Signal dom (BitVector (FpgaCount - 1))

channelNames :: Vec (FpgaCount - 1) String
channelNames =
  "X0Y10" :> Nil

clockPaths :: Vec (FpgaCount - 1) String
clockPaths =
  "clk0" :> Nil

-- | Some order of the FPGA ids and a mapping to their connected
-- neighbors (via the index position in the vector) according to the
-- different hardware interfaces on the boards.
fpgaSetup :: Vec FpgaCount (String, Vec (FpgaCount - 1) (Index FpgaCount))
fpgaSetup = ("0", 1 :> Nil ) :> ( "1", 0 :> Nil ) :> Nil

-- | Determines the link mask of a particular node.
linkMask ::
  forall n i.
  (KnownNat n, KnownNat i, n <= FpgaCount, i + 1 <= n) =>
  Topology n -> SNat i -> BitVector (FpgaCount - 1)
linkMask g i = case leTrans @(i + 1) @n @FpgaCount of
  Sub Dict -> pack $ map edge $ snd $ at @i @(FpgaCount - i - 1) i fpgaSetup
 where
  edge j =
    j <= (natToNum @(n - 1))
     && hasEdge g (natToNum @i) (truncateB @_ @n @(FpgaCount - n) j)

linkMasks ::
  forall n.
  (KnownNat n, n <= FpgaCount) =>
  Topology n -> Vec n (BitVector (FpgaCount - 1))
linkMasks g = smap (const . linkMask') indicesI
 where
  -- workaround, which is required to compensate for the missing upper
  -- bound witness of smap, which can be improved as soon as
  -- https://github.com/clash-lang/clash-compiler/pull/2686
  -- is available.
  linkMask' :: forall i. SNat i -> BitVector (FpgaCount - 1)
  linkMask' i@SNat = case compareSNat (SNat @(i + 1)) (SNat @n)  of
    SNatLE -> linkMask g i
    _ -> error "impossible"
