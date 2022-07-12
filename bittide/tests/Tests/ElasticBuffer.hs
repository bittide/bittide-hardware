-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.ElasticBuffer ( ebGroup, res ) where

import Clash.Explicit.Prelude

import Control.Monad (join)
import Data.Maybe (catMaybes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Prelude as P

import Bittide.ElasticBuffer
import Tests.ElasticBuffer.ClockSync

type Dom = XilinxSystem

ebGroup :: TestTree
ebGroup = testGroup "Elastic buffer"
    [ testCase "no exceptions" $ fmap fst res @?= P.replicate 10 0xee ]
    -- TODO: drop beginning or filter based on underflow

feedState :: [BitVector 16] -> Bool -> ([BitVector 16], BitVector 16)
feedState (x:xs) True = (xs, x)
feedState xs False = (xs, deepErrorX "Feed off")

inpSignal ::
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  [BitVector 16] ->
  -- | For our stall mechanism
  Signal dom Bool ->
  Signal dom (BitVector 16)
inpSignal clk rst ena = mealy clk rst ena feedState

res :: [(BitVector 16, Signed 7)]
res =
  catMaybes
    $ sampleN 1000
    $ fmap (\(p0,p1,x,d) -> join (p0 `orNothing` (p1 `orNothing` (x,d))))
    $ (\(b0, b1, ~(x,_,ou,cnt)) -> bundle (b0, b1, x, cnt))
    $ watchEb clk rst ena (inpSignal clk rst ena [1..] (pure True)) -- TODO inpSignal doesn't respect wobble
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
  (ena0, ena1) = onBoth toEnable $ entangledEna clk rst ena (3 :: Int) 5
  -- FIXME: even same ratio loses data??
  --
  -- TODO: tbClockGen
  -- biTbClockGen

orNothing :: Bool -> a -> Maybe a
orNothing False _ = Nothing
orNothing True a = Just a

onBoth :: (a -> b) -> (a, a) -> (b, b)
onBoth f (x, y) = (f x, f y)
