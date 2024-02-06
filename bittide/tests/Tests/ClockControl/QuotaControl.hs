-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ImplicitParams #-}

module Tests.ClockControl.QuotaControl where

import Prelude

import Bittide.ClockControl (SpeedChange(..))
import Bittide.ClockControl.QuotaControl (QuotaControlState(..), quotaControlTF)

import Control.Monad (forM_)
import Control.Arrow (second)
import Data.Maybe (catMaybes)

import Test.Tasty
import Test.Tasty.HUnit

quotaControlGroup :: TestTree
quotaControlGroup = testGroup "Clock Quota Controller"
 [ testCase "Cost Function (SpeedUp)"
     $ let ?speedChange = SpeedUp  in costFunctionTest
 , testCase "Cost Function (SlowDown)"
     $ let ?speedChange = SlowDown in costFunctionTest
 , testCase "Reverse is for free (SpeedUp)"
     $ let ?speedChange = SpeedUp  in towardsCenterIsFreeTest
 , testCase "Reverse is for free (SpeedDown)"
     $ let ?speedChange = SlowDown in towardsCenterIsFreeTest
 ]

costFunctionTest :: (?speedChange :: SpeedChange) => Assertion
costFunctionTest =
  forM_ (zip3 golden outcome [0,1..n-1]) $ \(g,o,i) -> do
    assertEqual ("Mismatch at position " <> show i) g o
 where
  n = length golden

  outcome :: [Maybe SpeedChange]
  outcome = reverse $ snd $ foldl
    (\(s, ys) x -> second (:ys) $ quotaControlTF s x)
    (QuotaControlState NoChange 0 0, [])
    (replicate n ?speedChange)

towardsCenterIsFreeTest :: (?speedChange :: SpeedChange) => Assertion
towardsCenterIsFreeTest =
  assertEqual "Re-centering has costs" 0
    $ incDecCounter
    $ foldl
        (\s -> fst . quotaControlTF s)
        (QuotaControlState NoChange 0 0)
        (replicate n ?speedChange <> replicate ops oppositeDir)

 where
  n = length golden
  ops = length $ catMaybes golden

  oppositeDir = case ?speedChange of
    SpeedUp  -> SlowDown
    NoChange -> NoChange
    SlowDown -> SpeedUp

golden :: (?speedChange :: SpeedChange) => [Maybe SpeedChange]
golden = concatMap
  (\(n, d) ->
      concat $ replicate n $ reverse $ Just ?speedChange : replicate d Nothing
  )
  --  #INC/DECs      cost
  [ ( p2 12,         0     )
  , ( p2 14 - p2 12, 1     )
  , ( p2 13,         2     )
  , ( p2 12,         3     )
  , ( p2 11,         4     )
  , ( p2 10,         5     )
  , ( p2 9,          6     )
  , ( p2 8,          7     )
  , ( p2 7,          8     )
  , ( p2 6,          9     )
  , ( p2 5,          10    )
  , ( p2 4,          11    )
  , ( p2 3,          12    )
  , ( p2 2,          13    )
  , ( p2 1,          14    )
  , ( p2 0,          15    )
  , ( 1,             p2 4  )
  , ( 1,             p2 5  )
  , ( 1,             p2 6  )
  , ( 1,             p2 7  )
  , ( 1,             p2 8  )
  , ( 1,             p2 9  )
  , ( 1,             p2 10 )
  ]
 where
  p2 :: Int -> Int
  p2 = (2^)