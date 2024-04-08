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
type FpgaCount = 8 :: Nat

-- | Data wires from/to transceivers. No logic should be inserted on these
-- wires. Should be considered asynchronous to one another - even though their
-- domain encodes them as related.
type TransceiverWires dom = Vec (FpgaCount - 1) (Signal dom (BitVector 1))

channelNames :: Vec (FpgaCount - 1) String
channelNames =
  "X0Y10":> "X0Y9":> "X0Y16" :> "X0Y17" :> "X0Y18" :> "X0Y19" :> "X0Y11" :> Nil

clockPaths :: Vec (FpgaCount - 1) String
clockPaths =
  "clk0" :> "clk0":> "clk0-2":> "clk0-2":> "clk0-2":> "clk0-2":> "clk0"  :> Nil

-- | Some order of the FPGA ids and a mapping to their connected
-- neighbors (via the index position in the vector) according to the
-- different hardware interfaces on the boards.
fpgaSetup :: Vec FpgaCount (String, Vec (FpgaCount - 1) (Index FpgaCount))
fpgaSetup =
  --   FPGA Id         SFP0    SFP1    J4    J5    J6    J7    SMA
     ( "210308B3B272", 3    :> 2    :> 4  :> 5  :> 6  :> 7  :> 1   :> Nil )
  :> ( "210308B0992E", 2    :> 3    :> 5  :> 6  :> 7  :> 4  :> 0   :> Nil )
  :> ( "210308B0AE73", 1    :> 0    :> 6  :> 7  :> 4  :> 5  :> 3   :> Nil )
  :> ( "210308B0AE6D", 0    :> 1    :> 7  :> 4  :> 5  :> 6  :> 2   :> Nil )
  :> ( "210308B0AFD4", 7    :> 6    :> 0  :> 3  :> 2  :> 1  :> 5   :> Nil )
  :> ( "210308B0AE65", 6    :> 7    :> 1  :> 0  :> 3  :> 2  :> 4   :> Nil )
  :> ( "210308B3A22D", 5    :> 4    :> 2  :> 1  :> 0  :> 3  :> 7   :> Nil )
  :> ( "210308B0B0C2", 4    :> 5    :> 3  :> 2  :> 1  :> 0  :> 6   :> Nil )
  :> Nil

-- | Determines the link mask of a particular node.
--
-- >>> import Data.Graph
-- >>> import Clash.Prelude
-- >>> import Bittide.Topology
-- >>> let edges = [(0, 1), (0, 2), (1, 2), (1, 0), (2, 0), (2, 1)]
-- >>> let g = fromGraph @3 "test" $ buildG (0, 2) edges
-- >>> linkMask g d0
-- 0b010_0001
-- >>> linkMask g d1
-- 0b100_0001
-- >>> linkMask g d2
-- 0b110_0000
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
