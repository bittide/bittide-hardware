-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Numeric.Extra where

import Clash.Prelude

import Control.Monad (foldM)
import Data.Char (digitToInt, isHexDigit)

-- | Parse a hexadecimal string into a 'BitPack'able type.
--
-- Note that this function does not handle types that do not use the full range
-- of their bit size. For example, 'Index' will return an 'XException' if the
-- parsed value is out of range of the @'Index' n@, but in range of
-- @'BitVector' ('BitSize' ('Index' n))@. To fix this properly, we need a version
-- of 'unpack' that returns a 'Maybe' value.
parseHex :: forall a. BitPack a => String -> Either String a
parseHex "" = Left "Empty string"
parseHex s = do
  result <- foldM parseDigit (0 :: Integer) s
  if result > natToNum @(2^BitSize a - 1)
  then Left $ "Value is out of range: " <> show result
  else Right $ unpack (fromInteger result)
 where
  parseDigit !a c
    | not (isHexDigit c) = Left $ "Non-hexadecimal digit: " <> [c] <> " in " <> s
    | otherwise = Right $ shift a 4 .|. toInteger (digitToInt c)
