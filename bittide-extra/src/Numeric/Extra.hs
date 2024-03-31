-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Numeric.Extra where

import Prelude

import Control.Monad (foldM)
import Clash.Prelude (BitPack(..), BitSize, natToNum, unpack)
import Data.Bits (shift, (.|.))
import Data.Char (digitToInt, isHexDigit)

-- | Parse a hexadecimal string into a 'BitPack'able type.
--
-- Note that this function does not handle types that do not use the full range
-- of their bit size. For example, 'Index' will return an 'XException' if the
-- parsed value is out of range of the @'Index' n@, but in range of
-- @'BitVector' ('BitSize' ('Index' n))@. To fix this properly, we need a version
-- of 'unpack' that returns a 'Maybe' value.
parseHex :: forall a. BitPack a => String -> Either String a
parseHex s = fmap unpack . foldM pHex 0 . reverse . zip [0, 1..] . reverse $ s
 where
  pHex !a (i, c)
    | not (isHexDigit c) =
        Left $ "Non-hexadecimal digit: " <> [c]
    | bitsNeeded i c > natToNum @(BitSize a) =
        Left $ "Value is out of range: " <> s
    | otherwise =
        return $ shift a 4 .|. digitToNum c

  bitsNeeded i c = i*4 + specialLog2 (digitToInt c)
  digitToNum = fromInteger . toInteger . digitToInt

  -- Special version of log2:
  --
  --   * 0 -> 0
  --   * 1 -> 0
  --   * n -> 1 + actualLog2 n
  --
  specialLog2 :: Int -> Int
  specialLog2 =
    let log2' !a 0 = a
        log2' !a n = log2' (a + 1) $ n `div` 2
    in  log2' 0
