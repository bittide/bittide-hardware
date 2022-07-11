-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}
module Tests.ContranomySim.Print (tests) where

import           Clash.Prelude

import qualified Data.ByteString     as BS
import qualified Data.List           as L
import           Data.Word           (Word8)
import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Test.Tasty          (TestTree, testGroup)

import           ContranomySim.Print (getDataBytes)
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

genWriteStream
  :: BS.ByteString
  -> Unsigned 32 -- ^ Character device address
  -> Unsigned 32 -- ^ Range around character device address to generate irrelevant writes
  -> Int -- ^ 0 .. <this> writes to interleave
  -> Gen [(Unsigned 32, Signed 32)]
genWriteStream str addr addrRange interleave' = go (BS.unpack str)
 where
  go :: [Word8] -> Gen [(Unsigned 32, Signed 32)]
  go [] = Gen.list genLength ((,) <$> genAddr <*> genS32)
  go (x:xs) = do
    prefix <- Gen.list genLength ((,) <$> genAddr <*> genS32)
    rest <- go xs
    pure $ prefix <> [(addr, extend $ bitCoerce x)] <> rest

  genAddr = Gen.choice [Gen.integral_ $ Range.linear (addr + 1) (addr + addrRange), Gen.integral_ $ Range.linear (addr - addrRange) (addr - 1)]
  genLength = Range.linear 0 interleave'
  genS32 = Gen.integral_ @Gen $ Range.linear (minBound :: Signed 32) (maxBound :: Signed 32)

tests :: TestTree
tests = testGroup "Print Tests"
  [ testGroup "\"hello world\""
      [ testCase "\"hello world\" no interleaved writes" $ do
          let str = "hello world"
          stream <- Gen.sample (genWriteStream str 0x90000000 0 0)
          str @?= getDataBytes (L.length stream) 0x90000000 (Just <$> stream)

      , testPropertyNamed "\"hello world\", interleaved" "hello_world_interleaved" $ property $ do
          let str = "hello world"
          stream <- forAll $ genWriteStream str 0x90000000 100 10
          str === getDataBytes (L.length stream) 0x90000000 (Just <$> stream)
      ]

  , testGroup "non-empty String"
      [ testPropertyNamed "non-empty-string no interleaved writes" "non_empty_string_no_interleaved_writes" $ property $ do
          bytes <- forAll $ Gen.list (Range.linear 10 100) (Gen.word8 Range.constantBounded)
          let str = BS.pack bytes
          stream <- forAll $ genWriteStream str 0x90000000 0 0
          str === getDataBytes (L.length stream) 0x90000000 (Just <$> stream)

      , testPropertyNamed "non-empty-string interleaved writes" "non_empty_string_interleaved_writes" $ property $ do
          bytes <- forAll $ Gen.list (Range.linear 10 100) (Gen.word8 Range.constantBounded)
          let str = BS.pack bytes
          stream <- forAll $ genWriteStream str 0x90000000 100 10
          str === getDataBytes (L.length stream) 0x90000000 (Just <$> stream)
      ]

  , testPropertyNamed "no print-writes cause no output" "no_writes_no_output" $ property $ do
      let bytes = []
      let str = BS.pack bytes
      stream <- forAll $ genWriteStream str 0x90000000 100 1000
      str === getDataBytes (L.length stream) 0x90000000 (Just <$> stream)
  ]
