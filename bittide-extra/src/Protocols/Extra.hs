-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Many of these functions should be added to `clash-protocols`, there is a PR
for this: https://github.com/clash-lang/clash-protocols/pull/116
And a bittide-hardware issue:
https://github.com/bittide/bittide-hardware/issues/645
-}
module Protocols.Extra where

import Clash.Prelude

import Protocols

-- | Map a circuit over a vector of circuits
mapCircuit ::
  forall a b n.
  (KnownNat n) =>
  Circuit a b ->
  Circuit (Vec n a) (Vec n b)
mapCircuit circ = Circuit go
 where
  circuitFn :: (Fwd a, Bwd b) -> (Bwd a, Fwd b)
  Circuit circuitFn = circ
  go :: (Vec n (Fwd a), Vec n (Bwd b)) -> (Vec n (Bwd a), Vec n (Fwd b))
  go (aFwd, bBwd) = unzip $ circuitFn <$> zip aFwd bBwd

-- | Applies the mappings @Fwd a -> Fwd b@ and @Bwd b -> Bwd a@ to the circuit's signals.
applyC ::
  forall a b.
  (Fwd a -> Fwd b) ->
  (Bwd b -> Bwd a) ->
  Circuit a b
applyC fwdFn bwdFn = Circuit go
 where
  go :: (Fwd a, Bwd b) -> (Bwd a, Fwd b)
  go (fwdA, bwdB) = (bwdFn bwdB, fwdFn fwdA)

-- | Split a vector of circuits into two vectors of circuits.
splitAtC ::
  SNat left ->
  Circuit (Vec (left + right) a) (Vec left a, Vec right a)
splitAtC snat = applyC (splitAt snat) (uncurry (++))

{- | Split a vector of circuits into two vectors of circuits, where the length of the vectors is
determined by the context.
-}
splitAtCI ::
  (KnownNat left) =>
  Circuit (Vec (left + right) a) (Vec left a, Vec right a)
splitAtCI = applyC splitAtI (uncurry (++))

-- Split a vector of circuits into three vectors of circuits, where the length of the vectors is
-- determined by the context.
split3CI ::
  (KnownNat n0, KnownNat n1) =>
  Circuit (Vec (n0 + n1 + n2) circuit) (Vec n0 circuit, Vec n1 circuit, Vec n2 circuit)
split3CI = applyC splitAtI3 (\(a, b, c) -> a ++ b ++ c)
 where
  splitAtI3 (splitAtI -> (a, splitAtI -> (b, c))) = (a, b, c)

-- | Conceptually the same as '++', but for 'Circuit's.
appendC ::
  forall a m n.
  (KnownNat m, KnownNat n) =>
  Circuit (Vec m a, Vec n a) (Vec (m + n) a)
appendC = applyC (uncurry (++)) splitAtI

-- | Append three separate vectors of the same circuits into one vector of circuits
appendC3 ::
  (KnownNat n0, KnownNat n1) =>
  Circuit (Vec n0 circuit, Vec n1 circuit, Vec n2 circuit) (Vec (n0 + n1 + n2) circuit)
appendC3 = applyC (\(a, b, c) -> a ++ b ++ c) splitAtI3
 where
  splitAtI3 (splitAtI -> (a, splitAtI -> (b, c))) = (a, b, c)

{- | Transforms two vectors of circuits into a vector of tuples of circuits.
Only works if the two vectors have the same length.
-}
zipC ::
  forall a b n.
  (KnownNat n) =>
  Circuit (Vec n a, Vec n b) (Vec n (a, b))
zipC = applyC (uncurry zip) unzip

-- | Unzip a vector of tuples of circuits into a tuple of vectors of circuits.
unzipC ::
  forall a b n.
  (KnownNat n) =>
  Circuit (Vec n (a, b)) (Vec n a, Vec n b)
unzipC = applyC unzip (uncurry zip)

-- | Unzip a vector of 3-tuples of circuits into a 3-tuple of vectors of circuits.
unzipC3 ::
  (KnownNat n) =>
  Circuit (Vec n (a, b, c)) (Vec n a, Vec n b, Vec n c)
unzipC3 = applyC unzip3 (\(a, b, c) -> zip3 a b c)

{- | Transforms three vectors of circuits into a vector of tuples of circuits.
Only works if the three vectors have the same length.
-}
zipC3 ::
  forall a b c n.
  (KnownNat n) =>
  Circuit (Vec n a, Vec n b, Vec n c) (Vec n (a, b, c))
zipC3 = applyC (\(a, b, c) -> zip3 a b c) unzip3

-- | Transforms a 'Circuit' of 'Vector' of @Vector@s into a @Circuit@ of @Vector@.
concatC ::
  (KnownNat n0, KnownNat n1) =>
  Circuit (Vec n0 (Vec n1 circuit)) (Vec (n0 * n1) circuit)
concatC = applyC concat unconcatI

-- | Transforms a 'Circuit' of 'Vector' into a @Circuit@ of @Vector@ of @Vector@s.
unconcatC ::
  (KnownNat n, KnownNat m) =>
  SNat m ->
  Circuit (Vec (n * m) circuit) (Vec n (Vec m circuit))
unconcatC snat = applyC (unconcat snat) concat

{- | Replicates a 'CSignal dom a' into a 'Vec n (CSignal dom a)', where the length of the
vector is determined by the context.
-}
replicateCSignalI ::
  forall dom a n.
  (KnownNat n) =>
  Circuit (CSignal dom a) (Vec n (CSignal dom a))
replicateCSignalI = applyC repeat (const $ pure ())

-- | Map a function over a 'Circuit' of 'CSignal's
cSignalMap ::
  forall dom a b.
  (KnownDomain dom) =>
  (a -> b) ->
  Circuit (CSignal dom a) (CSignal dom b)
cSignalMap fn = applyC (fmap fn) (const $ pure ())
