-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Transceiver.Prbs where

import Clash.Explicit.Prelude
import Clash.Prelude (withClock)

import Clash.Hedgehog.Sized.Index (genIndex)

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import Tests.Shared

import qualified Bittide.Transceiver.Prbs as Prbs
import qualified Bittide.Transceiver.WordAlign as WordAlign
import qualified Data.List as L
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


data SomePrbsConfig where
  SomePrbsConfig ::
    KnownNat nBytes =>
    Prbs.Config polyLength polyTap (8 * nBytes) ->
    SomePrbsConfig

instance Show SomePrbsConfig where
  show (SomePrbsConfig (Prbs.Config :: Prbs.Config polyLength polyTap (8 * nBytes))) =
    "SomePrbsConfig (Prbs.Config"
      <> "{polyLength=" <> show (snatToNatural (SNat :: SNat polyLength))
      <> ", polyTap=" <> show (snatToNatural (SNat :: SNat polyTap))
      <> ", nBytes=" <> show (snatToNatural (SNat :: SNat nBytes))
      <> "})"

-- | Generate a 'SomePrbsConfig' such that:
--
-- * 1 <= nBits <= (n + 1)
-- * 1 <= polyTap <= (n + 1)
-- * (polyTap + 1) <= polyLength <= (polyTap + 1 + n + 1)
--
genSomePrbsConfig :: Natural -> Gen SomePrbsConfig
genSomePrbsConfig n = do
  n0 <- Gen.integral (Range.linear 0 n)
  n1 <- Gen.integral (Range.linear 0 n)
  n2 <- Gen.integral (Range.linear 0 n)

  case (someSNat n0, someSNat n1, someSNat n2) of
    (SomeSNat sn0, SomeSNat sn1, SomeSNat sn2) ->
      let
        nBytes = sn0 `addSNat` d1
        polyTap = sn1 `addSNat` d1
        polyLength = polyTap `addSNat` d1 `addSNat` sn2
      in
        case (nBytes, polyTap, polyLength) of
          (SNat :: SNat nBytes, SNat :: SNat polyTap, SNat :: SNat polyLength) ->
            pure $ SomePrbsConfig (Prbs.Config @polyLength @polyTap @(8 * nBytes))

-- | Number of cycles to check for PRBS errors after the PRBS checker is in sync
checkOk :: Int
checkOk = 8

-- | Connect a PRBS generator to a PRBS checker and check that no errors are
-- detected after the expected time it takes for the checker to align with the
-- generator.
prop_happy :: Property
prop_happy = property $ do
  SomePrbsConfig config@Prbs.Config{} <- forAll (genSomePrbsConfig 100)
  case config of
    (Prbs.Config :: Prbs.Config polyLength polyTap nBits) -> do

      offset <- forAll (genIndex Range.constantBounded)
      let
        resetCycle = 1
        alignCycle = 1 -- if offset == 0 then 0 else 1
        stateCycles = natToNum @(polyLength `DivRU` nBits)
        expectAlignmentAfterNCycles = stateCycles + resetCycle + alignCycle

      nNoiseCycles <- forAll (Gen.integral (Range.linear 0 (10 * expectAlignmentAfterNCycles)))
      noise <- forAll (Gen.list (Range.singleton nNoiseCycles) genDefinedBitVector)

      let
        clk = clockGen @XilinxSystem
        rst = noReset
        ena = enableGen

        prbs = Prbs.generator clk rst ena config
        noiseCounter = register clk rst ena (0 :: Int) (noiseCounter + 1)
        sendNoise = noiseCounter .<. pure nNoiseCycles
        noiseOrPrbs = mux sendNoise (fromList (noise <> L.repeat 0)) prbs
        noiseOrPrbsDealigned = withClock clk $
          WordAlign.aligner WordAlign.dealignLsbFirst (pure False) (pure offset) noiseOrPrbs
        errors = Prbs.checker clk rst ena config noiseOrPrbsDealigned

        okAfter = fromIntegral (nNoiseCycles + expectAlignmentAfterNCycles)
        (notOk, ok) = L.splitAt okAfter (sample errors)

        -- Note the first three cycles are always reported as errornous due to
        -- start up behavior
        anyNotOk = L.any (/= 0) (L.drop 3 notOk)
        allOk = not anyNotOk

      L.take checkOk ok === L.replicate checkOk 0

      -- Statistics checks
      cover 20
        "nNoiseCycles > expectAlignmentAfterNCycles"
        (nNoiseCycles > expectAlignmentAfterNCycles)

      cover 20
        "nNoiseCycles <= expectAlignmentAfterNCycles"
        (nNoiseCycles <= expectAlignmentAfterNCycles)

      cover 70 "any error detected after 3 cycles" anyNotOk
      cover 2 "no errors detected after 3 cycles" allOk -- detect "always pass"

tests :: TestTree
tests = testGroup "Prbs"
  [ testPropertyNamed "prop_happy" "prop_happy" prop_happy
  ]

main :: IO ()
main = defaultMain tests
