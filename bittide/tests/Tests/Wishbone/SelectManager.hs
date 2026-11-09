-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.Wishbone.SelectManager where

import Clash.Prelude

import Bittide.Wishbone (selectManager)
import Clash.Explicit.Reset (noReset)
import Hedgehog (Gen, Property, forAll, property, (===))
import Protocols (toSignals)
import Protocols.Experimental.Wishbone (
  WishboneM2S (..),
  WishboneS2M (..),
  emptyWishboneM2S,
  emptyWishboneS2M,
 )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import qualified Data.List as L
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: TestTree
tests =
  testGroup
    "Tests.Wishbone.SelectManager"
    [ testPropertyNamed
        "2 managers, immediate acknowledge"
        "prop_twoManagersImmediate"
        (prop_matchesModel d2 ackImmediately)
    , testPropertyNamed
        "2 managers, delayed acknowledge"
        "prop_twoManagersDelayed"
        (prop_matchesModel d2 ackDelayed)
    , testPropertyNamed
        "3 managers, immediate acknowledge"
        "prop_threeManagersImmediate"
        (prop_matchesModel d3 ackImmediately)
    , testPropertyNamed
        "3 managers, delayed acknowledge"
        "prop_threeManagersDelayed"
        (prop_matchesModel d3 ackDelayed)
    ]
 where
  ackImmediately = pure True
  -- Most cycles are not ready, so transfers often take multiple cycles.
  ackDelayed = Gen.frequency [(3, pure False), (1, pure True)]

main :: IO ()
main = defaultMain tests

-- | Inputs of a single cycle.
data Input n = Input
  { select :: Index n
  -- ^ Manager to select
  , starts :: Vec n Bool
  -- ^ Managers that start a transfer, if they are not already doing one
  , ready :: Bool
  -- ^ Whether the subordinate acknowledges a transfer in this cycle
  }
  deriving (Generic, NFDataX, Show)

{- | Observed outputs of a single cycle: whether the subordinate sees a transfer,
the address it sees, and the acknowledge and error of every manager.
-}
type Output n = (Bool, BitVector 8, Vec n (Bool, Bool))

{- | Drive the managers with random transfers and a random select signal, and
check every cycle against a model. A manager holds its transfer until it gets an
acknowledge or an error, and puts its own index on the address bus. The
subordinate acknowledges a transfer on the cycles it is ready.

The model checks that only the connected manager reaches the subordinate, that
the others get an error, and that the connection only changes while the
connected manager is not doing a transfer. On top of that, the test checks
directly that a transfer that has started is never interrupted.
-}
prop_matchesModel ::
  forall n. (KnownNat n, 1 <= n) => SNat n -> Gen Bool -> Property
prop_matchesModel SNat genReady = property $ do
  len <- forAll $ Gen.int (Range.linear 1 100)
  inputs <-
    forAll
      $ Gen.list (Range.singleton len)
      $ Input
      <$> Gen.enumBounded @_ @(Index n)
      <*> sequence (repeat Gen.bool)
      <*> genReady

  let
    actual = L.take len $ sample $ system inputs
    expected = model inputs

  actual === expected
  [] === interruptions (L.zip inputs actual)

-- | 'selectManager', connected to the managers and subordinate.
system :: forall n. (KnownNat n, 1 <= n) => [Input n] -> Signal System (Output n)
system inputs =
  withClockResetEnable clockGen noReset enableGen
    $ let
        input = fromList (inputs <> L.repeat (L.last inputs))
        (s2ms, m2s) =
          toSignals
            (selectManager ((.select) <$> input))
            (izipWith manager (unbundle ((.starts) <$> input)) s2ms, s2m)
        s2m = subordinate <$> ((.ready) <$> input) <*> m2s
       in
        bundle
          ( (.strobe) <$> m2s
          , (.addr) <$> m2s
          , map (\s -> (s.acknowledge, s.err)) <$> bundle s2ms
          )
 where
  manager ::
    (HiddenClockResetEnable System) =>
    Index n ->
    Signal System Bool ->
    Signal System (WishboneS2M 4) ->
    Signal System (WishboneM2S 8 4)
  manager i start s2m = mealy go False (bundle (start, s2m))
   where
    go busy (start1, s) = (stb && not (s.acknowledge || s.err), m2s)
     where
      stb = busy || start1
      m2s =
        (emptyWishboneM2S @8 @4){busCycle = stb, strobe = stb, addr = fromIntegral i}

  subordinate :: Bool -> WishboneM2S 8 4 -> WishboneS2M 4
  subordinate rdy m = (emptyWishboneS2M @4){acknowledge = m.busCycle && m.strobe && rdy}

-- | Model of 'system'.
model :: forall n. (KnownNat n) => [Input n] -> [Output n]
model = go 0 (repeat False)
 where
  -- Connected manager at the start of the cycle, and which managers are doing a
  -- transfer that started in an earlier cycle.
  go :: Index n -> Vec n Bool -> [Input n] -> [Output n]
  go _ _ [] = []
  go connected busy (input : inputs) = output : go current nextBusy inputs
   where
    strobes = zipWith (||) busy input.starts
    current
      | strobes !! connected = connected
      | otherwise = input.select
    ack = strobes !! current && input.ready
    responses = imap (\i s -> if i == current then (ack, False) else (False, s)) strobes
    nextBusy = zipWith (\s (a, e) -> s && not (a || e)) strobes responses
    output = (strobes !! current, fromIntegral current, responses)

{- | Cycles in which the subordinate sees a transfer that it does not
acknowledge, followed by a cycle in which it does not see the same transfer.
-}
interruptions :: [(Input n, Output n)] -> [(Output n, Output n)]
interruptions cycles =
  [ (now, next)
  | ((input, now@(stb, addr, _)), (_, next@(nextStb, nextAddr, _))) <- L.zip cycles (L.drop 1 cycles)
  , stb && not input.ready
  , not (nextStb && nextAddr == addr)
  ]
