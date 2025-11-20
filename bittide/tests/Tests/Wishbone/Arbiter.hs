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
import Data.Maybe (isJust)
import Data.String ()
import Hedgehog (Property, footnote, forAll, property, success, (===))
import Protocols.Wishbone (WishboneM2S, WishboneS2M, acknowledge)
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
      1
        + 3
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

  success
