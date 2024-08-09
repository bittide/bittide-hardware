-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.OverflowResistantDiff where

import Clash.Explicit.Prelude
import qualified Prelude as P

import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Clash.Hedgehog.Sized.Unsigned

import qualified Data.List as List
import qualified GHC.TypeNats as TN

import Bittide.Instances.Hitl.IlaPlot

tests :: TestTree
tests =
  testGroup
    "OverflowResistantDiff"
    [ testPropertyNamed
        "test with step-wise incrementing counter"
        "testStepwise"
        $ ordTest True
    , testPropertyNamed
        "test with randomly incrementing counter"
        "testStepwise"
        $ ordTest False
    ]

ordTest :: Bool -> Property
ordTest stepwise = property $ do
  dN <- forAll $ Gen.enum 0 (maxBitSize - 1)
  dM <- forAll $ Gen.enum 0 (2 P.^ maxBitSize - 1)
  case (TN.someNatVal dN, TN.someNatVal dM) of
    ( SomeNat (snatProxy -> (SNat :: SNat n))
      , SomeNat (snatProxy -> (SNat :: SNat m))
      ) ->
        do
          inputCounterValues <-
            fmap (0 :)
              $ if stepwise
                then return
                  $ flip List.unfoldr (0 :: Int, 0 :: Unsigned (n + 1))
                  $ \(n, o) ->
                    if n == depth
                      then Nothing
                      else Just (o, (n + 1, satSucc SatWrap o))
                else
                  forAll
                    $ Gen.list (Range.singleton depth)
                    $ genUnsigned Range.linearBounded

          newRefPositions <-
            forAll
              $ Gen.list (Range.linear 10 100)
              $ Gen.integral
              $ Range.linear 0 (depth - 1)

          let
            inputTriggerValues :: [Bool]
            inputTriggerValues = (False :)
              $ flip List.unfoldr (0, List.nub $ List.sort $ newRefPositions)
              $ \(n, xs) ->
                if n == depth
                  then Nothing
                  else Just $ case xs of
                    [] -> (False, (n + 1, []))
                    x : xr -> (x == n, ((n + 1), if x == n then xr else xs))

          -- the generated input lists should be of equal length
          List.length inputCounterValues === List.length inputTriggerValues

          let
            outputValues :: [DiffResult Integer]
            outputValues =
              fmap toInteger
                <$> sampleWithResetN @System @(DiffResult (Index (m + 1)))
                  d1
                  depth
                  ( \clk rst _ ->
                      overflowResistantDiff
                        clk
                        rst
                        (fromList inputTriggerValues)
                        (fromList inputCounterValues)
                  )

            expectedOutputs =
              List.tail
                $ List.reverse
                $ snd
                $ List.foldl golden (Nothing, [])
                $ List.zip inputCounterValues inputTriggerValues

            -- don't use fixed size numbers for the golden reference, so
            -- there are basically no overflows and we just can compute
            -- distance directly, where we discard the if it exceeds the
            -- capacity the output type
            golden ::
              ( Maybe (Integer, Unsigned (n + 1))
              , [DiffResult Integer]
              ) ->
              (Unsigned (n + 1), Bool) ->
              ( Maybe (Integer, Unsigned (n + 1))
              , [DiffResult Integer]
              )

            golden (_, xs) (c, True) =
              (Just (0, c), Difference 0 : xs)
            golden (Nothing, xs) _ =
              (Nothing, NoReference : xs)
            golden (Just (x, o), xs) (c, _) =
              let y = x + toInteger (c - o)
                  out =
                    if y > toInteger (maxBound :: Index (m + 1))
                      then TooLarge
                      else Difference y
               in (Just (y, c), out : xs)

          -- the generated output lists should be of equal length
          List.length outputValues === List.length expectedOutputs

          outputValues === expectedOutputs
 where
  maxBitSize = 8
  depth = 1000 :: Int
