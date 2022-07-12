-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.ElasticBuffer ( ebGroup, res ) where

import Clash.Explicit.Prelude

import Control.Monad (join)
import Data.Maybe (catMaybes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import qualified Prelude as P

import Bittide.ElasticBuffer
import Tests.ElasticBuffer.ClockSync

type Dom = XilinxSystem

ebGroup :: TestTree
ebGroup = testGroup "Elastic buffer"
    [ testCase "Contiguous once stable" $ assertBool "doesn't lose data when stable" (contiguous res) ]

contiguous :: (Num a, Eq a) => [a] -> Bool
contiguous xs = P.and (P.zipWith (\x y -> y == x + 1) xs (P.tail xs))

feedState :: [BitVector 16] -> a -> ([BitVector 16], BitVector 16)
feedState (x:xs) _ = (xs, x)

inpSignal ::
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  [BitVector 16] ->
  Signal dom a ->
  Signal dom (BitVector 16)
inpSignal clk rst ena = mealy clk rst ena feedState

res :: [BitVector 16]
res =
  catMaybes
    $ sampleN 1000
    $ fmap (\(p0,p1,x,_) -> join (p0 `orNothing` (p1 `orNothing` x)))
    $ (\(b0, b1, ~(x,st,_,cnt)) -> bundle (b0 .&&. b1, st, x, cnt))
    $ watchEb clk rst ena (inpSignal clk rst ena [1..] (pure ())) -- TODO inpSignal doesn't respect wobble
 where
  clk = clockGen
  rst = resetGen
  ena = enableGen

watchEb ::
  Clock Dom ->
  Reset Dom ->
  Enable Dom ->
  Signal Dom (BitVector 16) ->
  ( Signal Dom Bool -- TODO: same Dom for two varying enables is sketch
  , Signal Dom Bool
  ,
    ( Signal Dom (BitVector 16)
    , Signal Dom Bool
    , Signal Dom Bool
    , Signal Dom (Signed 7)
    )
  )
  -- TODO: resetGen delay maybe causing this? idk
  --
  -- OR: enables one cycle off so
watchEb clk rst ena dat = (fromEnable ena0, fromEnable ena1,) $ elasticBuffer (SNat @7) clk rst ena0 clk rst ena1 dat
 where
  (ena0, ena1) = onBoth toEnable $ entangledEna clk rst ena (83 :: Int) 89

orNothing :: Bool -> a -> Maybe a
orNothing False _ = Nothing
orNothing True a = Just a

onBoth :: (a -> b) -> (a, a) -> (b, b)
onBoth f (x, y) = (f x, f y)
