-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Tests.DelayWishboneC where

import Clash.Prelude

import Bittide.SharedTypes (Bytes, withBittideByteOrder)
import Bittide.Wishbone (makeWhoAmIdTh, whoAmIC)

import Protocols
import Protocols.MemoryMap (ConstBwd, MM)
import Protocols.Wishbone
import Protocols.Wishbone.Extra
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Tests.Wishbone (wbRead)

import qualified Data.List as L

whoAmID :: BitVector 32
whoAmID = $(makeWhoAmIdTh "helo")

simSimple :: IO ()
simSimple = mapM_ (putStrLn . show) simResult

-- | Calculate how many cycles a given WishboneM2S transaction will take

--- through the delayWishboneC component.
m2sToCycles :: WishboneM2S aw sw a -> Int
m2sToCycles m2s
  | not (m2s.lock || m2s.busCycle || m2s.strobe || m2s.writeEnable) = 1
  | otherwise = 3

simResult :: [WishboneS2M (Bytes 4)]
simResult = simulateN len (dutSimpleFn @System) input
 where
  input = [wbRead 0x0000_0000, emptyWishboneM2S]
  len = L.foldl (\acc m2s -> acc + m2sToCycles m2s) 0 input

dutSimple ::
  (HiddenClockResetEnable dom, 1 <= DomainPeriod dom) =>
  Circuit (ConstBwd MM, Wishbone dom 'Standard 0 (Bytes 4)) ()
dutSimple = withBittideByteOrder $ circuit $ \(mm, wb) -> do
  wbDelayed <- delayWishboneC -< wb
  whoAmIC whoAmID -< (mm, wbDelayed)

dutSimpleFn ::
  (HiddenClockResetEnable dom, 1 <= DomainPeriod dom) =>
  Signal dom (WishboneM2S 0 4 (Bytes 4)) ->
  Signal dom (WishboneS2M (Bytes 4))
dutSimpleFn m2s = let ((_, s2m), _) = circFn (((), m2s), ()) in s2m
 where
  Circuit circFn = dutSimple

case_test_wishboneDelayC :: Assertion
case_test_wishboneDelayC = assertEqual msg readResult.readData whoAmID
 where
  topEntityInput = [wbRead 0x0000_0000, emptyWishboneM2S]
  simulateLength = L.foldl (\acc m2s -> acc + m2sToCycles m2s) 0 topEntityInput
  simOut = simulateN simulateLength (dutSimpleFn @System) topEntityInput
  readResult = simOut L.!! ((m2sToCycles $ L.head topEntityInput) - 1)
  msg =
    "Simulation result "
      <> show readResult.readData
      <> " not equal to expected data "
      <> show whoAmID

tests :: TestTree
tests = $(testGroupGenerator)
