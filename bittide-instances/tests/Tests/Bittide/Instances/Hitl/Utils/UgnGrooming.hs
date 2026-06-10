-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.Bittide.Instances.Hitl.Utils.UgnGrooming where

import Prelude

import Clash.Prelude (BitVector, Index, Signed)

import Bittide.Graph.Weighted (edges, weight)
import Bittide.Instances.Hitl.Setup (LinkCount)
import Bittide.Instances.Hitl.Utils.Ugn (UgnEdge (..))
import Bittide.Instances.Hitl.Utils.UgnGrooming

import Control.Monad (forM_)
import Data.List (sort)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Hedgehog ((===))

import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- * Helpers

mkEdge :: BitVector 32 -> Index LinkCount -> BitVector 32 -> Index LinkCount -> Signed 64 -> UgnEdge
mkEdge = UgnEdge

-- * Adapter unit tests

case_safeMargin_adds_eps :: Assertion
case_safeMargin_adds_eps = do
  let es = [mkEdge 0 0 1 0 10, mkEdge 1 0 0 0 (-4)]
  map (.ugn) (safeMargin 5 es) @?= [15, 1]

case_ugnGraph_roundtrip :: Assertion
case_ugnGraph_roundtrip = do
  let
    es = [mkEdge 0 0 1 0 7, mkEdge 1 0 0 0 (-3)]
    g = ugnGraph es
  sort (edges g) @?= sort [(0, 1, 7), (1, 0, -3)]
  weight 0 1 g @?= Just 7
  weight 1 0 g @?= Just (-3)

case_groomToSafe_feasible :: Assertion
case_groomToSafe_feasible = do
  -- A 2-cycle with sum 2 (allowed); pad to a uniform +5 margin.
  let
    measured = [mkEdge 0 0 1 0 3, mkEdge 1 0 0 0 (-1)]
    safe = safeMargin 5 measured
  case groomToSafe measured safe of
    UgnsChangedTooMuch ns -> assertFailure ("unexpected infeasible: " <> show ns)
    Groomed{frames} -> do
      assertBool "all frames >= 0" (all ((>= 0) . snd) frames)
      -- No relabeling needed for a uniform positive margin, so every pad == eps.
      map snd frames @?= [5, 5]

case_groomToSafe_infeasible :: Assertion
case_groomToSafe_infeasible = do
  -- measured 2-cycle sums to 0; the stored "safe" demands sum -2: impossible.
  let
    measured = [mkEdge 0 0 1 0 1, mkEdge 1 0 0 0 (-1)]
    safe = [mkEdge 0 0 1 0 0, mkEdge 1 0 0 0 (-2)]
  case groomToSafe measured safe of
    UgnsChangedTooMuch _ -> pure ()
    Groomed{} -> assertFailure "expected UgnsChangedTooMuch"

-- * symmetrizeUgn

case_symmetrize_removes_gradient :: Assertion
case_symmetrize_removes_gradient = do
  -- UGN(i->j) = base(i,j) + g_i - g_j, with symmetric base and node offsets g = [0,4,9].
  -- The imbalance is a pure gradient, so symmetrizeUgn must restore base on both directions.
  let
    es =
      [ mkEdge 0 0 1 0 6
      , mkEdge 1 0 0 0 14 -- base 10
      , mkEdge 1 0 2 0 15
      , mkEdge 2 0 1 0 25 -- base 20
      , mkEdge 0 0 2 0 21
      , mkEdge 2 0 0 0 39 -- base 30
      ]
    m = Map.fromList [((e.srcNode, e.dstNode), e.ugn) | e <- symmetrizeUgn es]
    dirs i j = (Map.findWithDefault 0 (i, j) m, Map.findWithDefault 0 (j, i) m)
  dirs 0 1 @?= (10, 10)
  dirs 1 2 @?= (20, 20)
  dirs 0 2 @?= (30, 30)

{- | On a complete mesh whose only asymmetry is a per-node offset (a pure gradient),
'symmetrizeUgn' removes it entirely: both directions of every link end up equal to the
symmetric base. It also leaves every round-trip unchanged (a relabel is a gauge change).
-}
prop_symmetrize_balances_pure_gradient :: H.Property
prop_symmetrize_balances_pure_gradient = H.property $ do
  k <- H.forAll $ Gen.int (Range.linear 2 6)
  let
    ns = [0 .. fromIntegral (k - 1)] :: [BitVector 32]
    pairs = [(i, j) | i <- ns, j <- ns, i < j]
  gs <-
    H.forAll $
      Gen.list (Range.singleton k) (Gen.integral (Range.linearFrom (0 :: Integer) (-10_000) 10_000))
  bases <-
    H.forAll $
      Gen.list (Range.singleton (length pairs)) (Gen.integral (Range.linear (0 :: Integer) 10_000))
  let
    gOf x = Map.findWithDefault 0 x (Map.fromList (zip ns gs))
    es =
      concat
        [ [ mkEdge i 0 j 0 (fromIntegral (base + gOf i - gOf j))
          , mkEdge j 0 i 0 (fromIntegral (base + gOf j - gOf i))
          ]
        | ((i, j), base) <- zip pairs bases
        ]
    m = Map.fromList [((e.srcNode, e.dstNode), e.ugn) | e <- symmetrizeUgn es]
  forM_ (zip pairs bases) $ \((i, j), base) -> do
    let
      fwd = Map.findWithDefault 0 (i, j) m
      bwd = Map.findWithDefault 0 (j, i) m
    fwd === fromIntegral base -- pure-gradient imbalance fully removed
    bwd === fromIntegral base

{- | 'symmetrizeUgn' is a /relabel/, so it must preserve the latency characteristics: the
round-trip of every link (and indeed every cycle sum) is unchanged. Here we check the
per-link round-trips on an arbitrary mesh.
-}
prop_symmetrize_preserves_roundtrips :: H.Property
prop_symmetrize_preserves_roundtrips = H.property $ do
  k <- H.forAll $ Gen.int (Range.linear 2 6)
  let
    ns = [0 .. fromIntegral (k - 1)] :: [BitVector 32]
    pairs = [(i, j) | i <- ns, j <- ns, i < j]
  fwds <-
    H.forAll $
      Gen.list
        (Range.singleton (length pairs))
        (Gen.integral (Range.linearFrom (0 :: Integer) (-5000) 5000))
  bwds <-
    H.forAll $
      Gen.list
        (Range.singleton (length pairs))
        (Gen.integral (Range.linearFrom (0 :: Integer) (-5000) 5000))
  let
    es =
      concat
        [ [mkEdge i 0 j 0 (fromIntegral f), mkEdge j 0 i 0 (fromIntegral b)]
        | ((i, j), f, b) <- zip3 pairs fwds bwds
        ]
    m = Map.fromList [((e.srcNode, e.dstNode), e.ugn) | e <- symmetrizeUgn es]
  forM_ (zip pairs (zipWith (+) fwds bwds)) $ \((i, j), rt) -> do
    let
      fwd = Map.findWithDefault 0 (i, j) m
      bwd = Map.findWithDefault 0 (j, i) m
    fwd + bwd === fromIntegral rt -- round-trip preserved (it is a relabel)

-- * Property: a relabel plan restores a freshly-booted mesh to a prior state

-- | The four nodes of the modelled mesh, identified by their (DNA-derived) node ids.
nodeIds :: [BitVector 32]
nodeIds = [0, 1, 2, 3]

-- | All directed links of the full mesh (one per ordered node pair).
nodePairs :: [(BitVector 32, BitVector 32)]
nodePairs = [(a, b) | a <- nodeIds, b <- nodeIds, a /= b]

{- | Reference fiber/transceiver latency per directed link, in cycles. Fixed hardware
characteristic of the topology — hard-coded here just as the implementation stores a
fixed reference. Asymmetric on purpose (forward/return links differ) to exercise the
directed-graph handling.
-}
referenceFibers :: Map (BitVector 32, BitVector 32) Integer
referenceFibers =
  Map.fromList $
    zip
      nodePairs
      -- (0,1)(0,2)(0,3) (1,0)(1,2)(1,3) (2,0)(2,1)(2,3) (3,0)(3,1)(3,2)
      [120, 340, 275, 118, 260, 410, 345, 262, 190, 280, 405, 188]

{- | Per-node sender-side pipeline delay, in cycles. A fixed hardware characteristic,
shared by every boot.
-}
sendDelays :: Map (BitVector 32) Integer
sendDelays = Map.fromList (zip nodeIds [7, 11, 5, 9])

{- | The reference boot's per-node boot delay (the cycle each node leaves reset).
Hard-coded: together with 'referenceFibers' it pins down the stored golden UGN set,
exactly as the implementation keeps a fixed reference rather than re-deriving it.
-}
goldenBootDelays :: Map (BitVector 32) Integer
goldenBootDelays = Map.fromList (zip nodeIds [1000, 4200, 350, 2750])

{- | Reference (golden) UGN set: the stored snapshot we groom back to. Derived once from
the hard-coded reference latencies and golden boot delays, with no elastic-buffer
contribution (the elastic buffers sit at their reference position).
-}
goldenUgns :: [UgnEdge]
goldenUgns = meshUgns goldenBootDelays (const 0)

{- | Build the mesh's UGN edges for one boot, exactly as the demo derives them: form the
two captured counters, then take their signed difference (see
'Bittide.Instances.Hitl.Utils.Ugn.timingOracleToUgnEdge', @ugn = localCounter -
remoteCounter@). A node's counter is just wall-clock time shifted by when it left reset:
node @i@'s counter reads @absoluteTime - bootDelay_i@. For a directed link @a -> b@, node
@a@ stamps a frame with its counter at some send time @t@; that frame reaches @b@
@fiber + eb@ cycles later, where @b@ captures its own counter:

  * @remoteCounter = (t - bootDelay_a) + sendDelay_a@ — sender's counter, captured at @b@.
  * @localCounter  = ((t + fiber_{a->b} + eb_{a->b}) - bootDelay_b) + sendDelay_a@ —
    receiver's counter at arrival.

  * @UGN_{a->b} = localCounter - remoteCounter
               = fiber_{a->b} + eb_{a->b} + bootDelay_a - bootDelay_b@.

The send time @t@ and the sender pipeline @sendDelay_a@ cancel in the difference (the
elastic buffer / fiber sit on the path between the two captures). @bootDelay@ is the
(per-boot, nondeterministic) reset-release cycle — a fact of /this/ boot, not a relabeling
we choose. Because @bootDelay_a@ enters with @+@ and @bootDelay_b@ with @-@, every
round-trip @UGN_{a->b} + UGN_{b->a}@ is boot-delay-independent (@= 2·fiber + 2·eb@),
honouring the physical round-trip constraint.
-}
meshUgns ::
  -- | Per-node boot delay
  Map (BitVector 32) Integer ->
  -- | Per-link elastic-buffer latency
  ((BitVector 32, BitVector 32) -> Integer) ->
  [UgnEdge]
meshUgns bootDelays ebOf =
  [ mkEdge a 0 b 0 (fromIntegral (localCounter - remoteCounter))
  | (a, b) <- nodePairs
  , let bootDelayOf n = Map.findWithDefault 0 n bootDelays
        -- An arbitrary fixed send time; it cancels in the difference.
        sendTime = 0
        remoteCounter = (sendTime - bootDelayOf a) + sendDelays Map.! a
        localCounter =
          ((sendTime + referenceFibers Map.! (a, b) + ebOf (a, b)) - bootDelayOf b)
            + sendDelays Map.! a
  ]

{- | Grooming a freshly-booted mesh back onto a fixed stored reference reconstructs the
reference UGNs exactly.

The reference topology ('referenceFibers'), sender pipeline ('sendDelays'), golden boot
('goldenBootDelays') and the resulting golden UGN set ('goldenUgns') are hard-coded — the
implementation likewise grooms onto a stored reference, not a freshly re-measured one. We
generate the /fresh/ boot: a new per-node boot delay and a small per-link elastic-buffer
deviation from the reference position. From those we synthesize the measured UGNs with the
same counter model, then groom them onto @λ^safe = goldenUgns + ε@.

Grooming returns a per-node /relabel offset/ @q@ (the correction we compute and apply,
distinct from the hardware's @bootDelay@) plus small per-link frame insertions. With a
strictly positive margin @ε@ the restore is always feasible: the slack on link @a -> b@ is
@(D_a - D_b) + ε - ebDev_{a->b}@ where @D_i = goldenBootDelay_i - currentBootDelay_i@ and
@ebDev@ is the (bounded) elastic-buffer deviation. Over any directed cycle the @D@ terms
telescope away, leaving @Σ(ε - ebDev) > 0@ (we keep @ebDev < ε@), so there is no negative
cycle. We assert the restore is feasible, every frame insertion is nonnegative, and the
relabel-then-pad reconstructs @λ^safe@ on every link.
-}
prop_relabelRestoresPriorBoot :: H.Property
prop_relabelRestoresPriorBoot = H.property $ do
  let margin = 5 :: Signed 64

  -- The fresh boot: a new per-node boot delay (the nondeterministic reset-release cycle).
  currentBootDelaysList <-
    H.forAll $ Gen.list (Range.singleton 4) (Gen.integral (Range.linear 0 100_000))
  -- Per-link elastic-buffer deviation from the reference position: small, and strictly
  -- below the margin so the residual stays a nonnegative insertion.
  ebDevs <-
    H.forAll $
      Gen.list (Range.singleton (length nodePairs)) (Gen.integral (Range.linear 0 4))

  let
    currentBootDelays = Map.fromList (zip nodeIds currentBootDelaysList)
    ebDevOf = Map.findWithDefault 0 `flip` Map.fromList (zip nodePairs ebDevs)

    currentUgns = meshUgns currentBootDelays ebDevOf
    lambdaSafe = safeMargin margin goldenUgns
    lambdaSafeByKey = Map.fromList [((e.srcNode, e.dstNode), e.ugn) | e <- lambdaSafe]

  case groomToSafe currentUgns lambdaSafe of
    UgnsChangedTooMuch ns -> do
      H.annotateShow ns
      H.failure
    Groomed{correction = q, frames} -> do
      -- 'q' is the per-node relabel offset grooming computed; every frame correction is
      -- a nonnegative insertion.
      H.assert (all ((>= 0) . snd) frames)
      -- Relabel-then-pad reconstructs the stored reference UGNs on every link:
      --   λ_{a->b} + q_b - q_a + frames_{a->b} == λ^safe_{a->b}
      forM_ frames $ \(e, f) -> do
        let
          qOf n = Map.findWithDefault 0 n q
          safeW =
            Map.findWithDefault
              (error "edge missing from λ^safe")
              (e.srcNode, e.dstNode)
              lambdaSafeByKey
        e.ugn + qOf e.dstNode - qOf e.srcNode + f === safeW

tests :: TestTree
tests = $(testGroupGenerator)
