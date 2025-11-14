-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.Wishbone.Arbiter where

import Clash.Prelude
import Protocols

import Bittide.SharedTypes (Bytes)
import Bittide.Wishbone (WishboneRequest (..), WishboneResponse (..), arbiter)
import Clash.Explicit.Reset (noReset)
import Clash.Hedgehog.Sized.Vector ()
import Data.Maybe (catMaybes, isJust)
import Data.String ()
import Hedgehog (Property, footnote, forAll, property, success, (===))
import Protocols.Wishbone (
  WishboneM2S,
  WishboneS2M,
  acknowledge,
  busCycle,
  strobe,
 )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)
import Tests.Wishbone.Utils (
  backPressure,
  driver,
  forwardPressure,
  genWishboneRequest,
  monitor,
  raw,
  sink,
 )
import Text.Show.Pretty (ppShow)

import qualified Data.List as L
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: TestTree
tests =
  testGroup
    "Tests.Wishbone.Arbiter"
    [ testPropertyNamed "simple" "simple" simple
    ]

main :: IO ()
main = defaultMain tests

simple :: Property
simple = property $ do
  let
    bigRange = Range.linear 0 50
    smallRange = Range.linear 0 5

  reqs1 <- forAll $ Gen.list bigRange genWishboneRequest
  reqs2 <- forAll $ Gen.list bigRange genWishboneRequest

  forwardDelays1 <- forAll $ Gen.list bigRange (Gen.int smallRange)
  forwardDelays2 <- forAll $ Gen.list bigRange (Gen.int smallRange)

  backDelays <- forAll $ Gen.list bigRange (Gen.int smallRange)

  let
    simLength =
      10
        + 2
        * sum
          [ L.length reqs1
          , L.length forwardDelays1
          , L.length reqs2
          , L.length forwardDelays2
          , L.length backDelays
          ]

    dutC ::
      ( dom ~ System
      , n ~ 4
      , addrW ~ 8
      ) =>
      Circuit
        ()
        ( CSignal dom (Maybe (WishboneRequest addrW n, WishboneResponse n))
        , CSignal dom (WishboneM2S addrW n (Bytes n))
        , CSignal dom (WishboneS2M (Bytes n))
        , CSignal dom (WishboneM2S addrW n (Bytes n))
        , CSignal dom (WishboneS2M (Bytes n))
        , CSignal dom (WishboneM2S addrW n (Bytes n))
        , CSignal dom (WishboneS2M (Bytes n))
        )
    dutC =
      withClockResetEnable clockGen noReset enableGen
        $ circuit
        $ do
          (wb1, m2s1, s2m1) <- raw <| forwardPressure forwardDelays1 <| driver reqs1
          (wb2, m2s2, s2m2) <- raw <| forwardPressure forwardDelays2 <| driver reqs2
          wb3 <- arbiter -< [wb1, wb2]
          (wb4, transactions) <- monitor -< wb3
          wb5 <- backPressure backDelays -< wb4
          (wb6, m2s, s2m) <- raw -< wb5
          sink -< wb6
          idC -< (transactions, m2s1, s2m1, m2s2, s2m2, m2s, s2m)

    ( transactions :: [Maybe (WishboneRequest 8 4, WishboneResponse 4)]
      , m2s1 :: [WishboneM2S 8 4 (BitVector 32)]
      , s2m1 :: [WishboneS2M (BitVector 32)]
      , m2s2 :: [WishboneM2S 8 4 (BitVector 32)]
      , s2m2 :: [WishboneS2M (BitVector 32)]
      , m2s :: [WishboneM2S 8 4 (BitVector 32)]
      , s2m :: [WishboneS2M (BitVector 32)]
      ) = L.unzip7 $ sampleN simLength $ bundle $ snd $ toSignals dutC units

  footnote ("transactions: " <> ppShow transactions)
  footnote ("m2s1: " <> ppShow m2s1)
  footnote ("s2m1: " <> ppShow s2m1)
  footnote ("m2s2: " <> ppShow m2s2)
  footnote ("s2m2: " <> ppShow s2m2)
  footnote ("m2s: " <> ppShow m2s)
  footnote ("s2m: " <> ppShow s2m)

  -- Property 1: No simultaneous acknowledgements to different managers
  -- The arbiter must never acknowledge both managers at the same time
  let bothAcknowledged = L.filter (\(s1, s2) -> s1.acknowledge && s2.acknowledge) (L.zip s2m1 s2m2)
  L.null bothAcknowledged === True

  -- Property 2: Total transactions equals sum of individual manager requests
  -- All transactions that complete should be accounted for
  let
    totalTransactions = L.length $ L.filter isJust transactions
    expectedTotal = L.length reqs1 + L.length reqs2
  totalTransactions === expectedTotal

  -- Property 3: Arbiter cannot switch while busCycle is asserted
  -- If a manager keeps busCycle asserted, the arbiter should not switch to another manager
  -- This tests the key arbiter invariant: a manager that asserts busCycle should keep the bus
  -- until it deasserts busCycle, even if other managers have pending requests.
  -- We use mapAccumL to track the currently active manager and detect illegal switches
  let
    cycles = L.zip3 s2m1 s2m2 (L.zip m2s1 m2s2)

    checkCycle ::
      Maybe Int ->
      ( WishboneS2M (BitVector 32)
      , WishboneS2M (BitVector 32)
      , ( WishboneM2S 8 4 (BitVector 32)
        , WishboneM2S 8 4 (BitVector 32)
        )
      ) ->
      (Maybe Int, Maybe String)
    checkCycle activeManager (ack1, ack2, (req1, req2)) =
      let
        -- Determine who got acknowledged this cycle
        acked1 = ack1.acknowledge && req1.busCycle && req1.strobe
        acked2 = ack2.acknowledge && req2.busCycle && req2.strobe

        -- Determine new active manager based on acknowledgements
        newActive = case (acked1, acked2) of
          (True, False) -> Just 0 -- Manager 0 (m1) is active
          (False, True) -> Just 1 -- Manager 1 (m2) is active
          _ -> Nothing -- No one or both acked (both case handled by property 1)

        -- Check for violation: switched away from a manager that still has busCycle asserted
        violation = case (activeManager, newActive) of
          (Just 0, Just 1)
            | req1.busCycle ->
                Just $ "Switched from manager 0 to 1, but manager 0 still has busCycle asserted"
          (Just 1, Just 0)
            | req2.busCycle ->
                Just $ "Switched from manager 1 to 0, but manager 1 still has busCycle asserted"
          _ -> Nothing

        -- Update active manager: keep it if they still have busCycle, otherwise clear
        finalActive = case activeManager of
          Just 0 | req1.busCycle -> Just 0
          Just 1 | req2.busCycle -> Just 1
          _ -> newActive
       in
        (finalActive, violation)

    (_, violations) = L.mapAccumL checkCycle Nothing cycles
    actualViolations = catMaybes violations

  L.null actualViolations === True

  success
