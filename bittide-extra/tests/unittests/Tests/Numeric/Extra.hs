-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Tests.Numeric.Extra where

import Clash.Prelude hiding (someNatVal)

import Clash.Hedgehog.Sized.Unsigned (genUnsigned)
import Data.Typeable (Typeable, typeRep)
import GHC.TypeNats (someNatVal)
import Hedgehog
import Numeric (showHex)
import Numeric.Extra (parseHex)
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Gen as Gen
import Data.Proxy

-- | Generate an in range value, convert it to hex, and parse it back. The result
-- should be the same as the original value.
parseHexInRange ::
  forall a m.
  (Monad m, BitPack a, Eq a, Show a, Bounded a, Integral a) =>
  Gen a ->
  PropertyT m ()
parseHexInRange genA = do
  a0 <- forAll genA
  let a0hex = showHex (toInteger a0) ""
  a1 <- evalEither (parseHex a0hex)
  a0 === a1

-- | 'parseHexInRange', but specialized to 'Unsigned'.
parseHexInRangeUnsigned :: Property
parseHexInRangeUnsigned = property $ do
  n <- forAll $ Gen.integral (Range.linear 0 128)
  case someNatVal n of
    SomeNat (Proxy :: Proxy n) ->
      parseHexInRange (genUnsigned @_ @n Range.constantBounded)

-- | Generate an in range value, add @maxBound + 1@ to it, convert it to hex, and
-- parse it back. The result should yield a parse error.
parseHexOutOfRange ::
  forall a m.
  (Monad m, BitPack a, Eq a, Show a, Bounded a, Integral a, Typeable a) =>
  Gen a ->
  PropertyT m ()
parseHexOutOfRange genA = do
  a0 <- forAll genA
  let
    a1 = toInteger a0 + toInteger (maxBound @a) + 1
    a1hex = showHex a1 ""
  case parseHex @a a1hex of
    Left _ -> success
    Right res -> do
      footnote ("type: " <> show (typeRep (Proxy @a)))
      footnote ("a0: " <> show a0)
      footnote ("a1: " <> show a1)
      footnote ("a1hex: " <> a1hex)
      footnote ("res: " <> show res)
      failure

-- | 'parseHexOutOfRange', but specialized to 'Unsigned'.
parseHexOutOfRangeUnsigned :: Property
parseHexOutOfRangeUnsigned = property $ do
  n <- forAll $ Gen.integral (Range.linear 0 128)
  case someNatVal n of
    SomeNat (Proxy :: Proxy n) ->
      parseHexOutOfRange (genUnsigned @_ @n Range.constantBounded)

tests :: TestTree
tests = testGroup "Tests.Numeric.Extra"
  [ testGroup "parseHexRountTrip"
    [ testPropertyNamed
        "Unsigned in range" "parseHexInRangeUnsigned" parseHexInRangeUnsigned
    , testPropertyNamed
        "Unsigned out of range" "parseHexOutOfRangeUnsigned" parseHexOutOfRangeUnsigned
    ]
  ]
