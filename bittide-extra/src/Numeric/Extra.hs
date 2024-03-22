-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Numeric.Extra where

import Prelude

import Control.Monad (foldM)
import Clash.Prelude (BitPack(..), BitSize, natToNum, unpack)
import Data.Bits (shift, (.|.))
import Data.Char (digitToInt, isHexDigit)

parseHex :: forall a. BitPack a => String -> Either String a
parseHex s =
  fmap unpack . foldM pHex 0 . reverse . zip [0, 1..] . reverse $ s
 where
  pHex !a (i, c)
    | not (isHexDigit c) =
        Left $ "Non-hexadecimal digit: " <> [c]
    | i * 4 + log2 (digitToNum c) > natToNum @(BitSize a) =
        Left $ "Value is out of range: " <> s
    | otherwise =
        return $ shift a 4 .|. digitToNum c

  -- 'digitToInt' produces only 16 different values. Hence, it can be
  --  extended to work for any 'Num' instance.
  digitToNum x = case digitToInt x of
    { 0 -> 0; 1 -> 1; 2 -> 2; 3 -> 3; 4 -> 4; 5 -> 5; 6 -> 6; 7 -> 7; 8 -> 8
    ; 9 -> 9; 10 -> 10; 11 -> 11; 12 -> 12; 13 -> 13; 14 -> 14; 15 -> 15
    ; y | y < 0     -> error "digitToInt returned some negative value"
        | otherwise -> error "digitToInt returned some value greater than 15"
    }

  log2 :: Int -> Int
  log2 =
    let log2' !a 0 = a
        log2' !a n = log2' (a + 1) $ n `div` 2
    in  log2' 0
