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
    | i * 4 + log2 (digitToInt c) > natToNum @(BitSize a) =
        Left $ "Value is out of range: " <> s
    | otherwise =
        return $ shift a 4 .|. fromInteger (toInteger (digitToInt c))

  log2 :: Int -> Int
  log2 =
    let log2' !a 0 = a
        log2' !a n = log2' (a + 1) $ n `div` 2
    in  log2' 0
