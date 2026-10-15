-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Tests.Bittide.Instances.Hitl.Utils.ElasticBufferOccupancy where

import Prelude

import Bittide.Instances.Hitl.Utils.ElasticBufferOccupancy
import Data.String.Interpolate (i)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Text.Parsec (parse)

import qualified Data.List as L

case_parse_eb_window :: Assertion
case_parse_eb_window =
  parse parseEbWindow "" "[MU] EBW 12 12000123 min=-3,-2,0,1,0,0,-16 max=4,2,1,1,0,0,15 uf=0 of=64"
    @?= Right
      EbWindow
        { window = 12
        , timeUs = 12000123
        , mins = [-3, -2, 0, 1, 0, 0, -16]
        , maxs = [4, 2, 1, 1, 0, 0, 15]
        , underflows = 0
        , overflows = 64
        }

case_parse_eb_window_too_few_occupancies :: Assertion
case_parse_eb_window_too_few_occupancies =
  case parse parseEbWindow "" "[MU] EBW 0 0 min=0,0 max=0,0 uf=0 of=0" of
    Left _ -> pure ()
    Right w -> assertFailure [i|Should have failed on too few occupancies, got: #{show w}|]

case_parse_eb_window_trailing_garbage :: Assertion
case_parse_eb_window_trailing_garbage =
  case parse parseEbWindow "" "[MU] EBW 0 0 min=0,0,0,0,0,0,0 max=0,0,0,0,0,0,0 uf=0 of=0x" of
    Left _ -> pure ()
    Right w -> assertFailure [i|Should have failed on trailing garbage, got: #{show w}|]

case_csv_row_matches_header :: Assertion
case_csv_row_matches_header =
  columns (toCsvRow emptyWindow) @?= columns csvHeader
 where
  columns = L.length . L.filter (== ',')
  emptyWindow =
    EbWindow
      { window = 0
      , timeUs = 0
      , mins = L.replicate 7 0
      , maxs = L.replicate 7 0
      , underflows = 0
      , overflows = 0
      }

tests :: TestTree
tests = $(testGroupGenerator)
