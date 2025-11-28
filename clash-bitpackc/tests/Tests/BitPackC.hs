-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Tests.BitPackC where

import Clash.Explicit.Prelude

import Clash.Class.BitPackC

import Control.Monad (forM_)
import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import qualified Prelude as P

-- Helper for testing the pack/unpack identity
packUnpackIdentity :: forall a. (Eq a, Show a, BitPackC a) => a -> Assertion
packUnpackIdentity val = do
  maybeUnpackC LittleEndian (packC LittleEndian val) @?= Just val
  maybeUnpackC BigEndian (packC BigEndian val) @?= Just val

idx :: (KnownNat n) => Vec n a -> Index n -> a
idx v i = v !! i

newtype MyInt = MkMyInt (Signed 10)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (BitPackC)
  deriving newtype (NFDataX)

data MyUnit = MyUnitPayload ()
  deriving (Show, Eq, Generic, BitPackC, NFDataX)

data MySumWithPayload
  = MySumPayload (BitVector 16)
  | MySumSecondCase
  deriving (Show, Eq, Generic, BitPackC, NFDataX)

data MyCustomBool = MyFalse | MyTrue
  deriving (Show, Eq, Generic, BitPackC, NFDataX, BitPack, Enum, Bounded)

data SmallSum = S1 | S2 | S3
  deriving (Show, Eq, Generic, BitPackC, NFDataX, Enum, Bounded)

data Point2D = Point2D {pX :: Unsigned 4, pY :: Unsigned 4}
  deriving (Show, Eq, Generic, BitPackC, NFDataX)

data Pixel = Pixel {rCh :: Unsigned 4, gCh :: Unsigned 5, bCh :: Unsigned 6}
  deriving (Show, Eq, Generic, BitPackC, NFDataX)

data Choice
  = OptA (Signed 5)
  | OptB Bool
  deriving (Show, Eq, Generic, BitPackC, NFDataX)

data LargeSum = L0 | L1 | L2 | L3 | L4 | L5 | L6 | L7
  deriving (Show, Eq, Generic, BitPackC, NFDataX, Enum, Bounded)

data NestedProd = NestedProd {npPoint :: Point2D, npActive :: Bool}
  deriving (Show, Eq, Generic, BitPackC, NFDataX)

data SoPComplex
  = ConSMixed (Unsigned 3) Bool
  | ConSPixel Pixel
  | ConSLargeSum LargeSum
  | ConSEmpty
  deriving (Show, Eq, Generic, BitPackC, NFDataX)

data WithVec = WithVec {wvItems :: Vec 3 (Unsigned 2)}
  deriving (Show, Eq, Generic, BitPackC, NFDataX)

data WithBV = WithBV {bvData :: BitVector 11}
  deriving (Show, Eq, Generic, BitPackC, NFDataX)

data MultiFieldProduct = MFP
  { fieldBool :: Bool
  , fieldU3 :: Unsigned 3
  , fieldS5 :: Signed 5
  , fieldIdx8 :: Index 8
  , fieldBV1 :: BitVector 1
  , fieldUnit :: ()
  , fieldCustomB :: MyCustomBool
  }
  deriving (Show, Eq, Generic, BitPackC, NFDataX)

case_myInt :: Assertion
case_myInt = mapM_ packUnpackIdentity [MkMyInt s | s <- [minBound ..]]

case_myUnit :: Assertion
case_myUnit = do
  let allValues = [MyUnitPayload ()]
  mapM_ packUnpackIdentity allValues

case_myCustomBool :: Assertion
case_myCustomBool = mapM_ packUnpackIdentity [minBound :: MyCustomBool ..]

case_smallSum :: Assertion
case_smallSum = mapM_ packUnpackIdentity [minBound :: SmallSum ..]

case_point2D :: Assertion
case_point2D =
  mapM_ packUnpackIdentity [Point2D x y | x <- [minBound ..], y <- [minBound ..]]

case_pixel :: Assertion
case_pixel =
  mapM_
    packUnpackIdentity
    [ Pixel r g b
    | r <- [minBound ..]
    , g <- [minBound ..]
    , b <- [minBound ..]
    ]

case_choice :: Assertion
case_choice = do
  let optA_values = [OptA s | s <- [minBound ..]]
  let optB_values = [OptB b | b <- [False, True]]
  -- let optC_values = [OptC ()]
  let allValues = optA_values <> optB_values -- <> optC_values
  mapM_ packUnpackIdentity allValues

case_largeSum :: Assertion
case_largeSum = do
  let allValues = [minBound :: LargeSum ..]
  mapM_ packUnpackIdentity allValues

case_nestedProd :: Assertion
case_nestedProd = do
  let allPoint2DValues =
        [ Point2D x y
        | x <- [minBound ..]
        , y <- [minBound ..]
        ]
  let allValues = [NestedProd p b | p <- allPoint2DValues, b <- [False, True]]
  mapM_ packUnpackIdentity allValues

case_soPComplex :: Assertion
case_soPComplex = do
  let
    conSMixed_values =
      [ ConSMixed u b
      | u <- [minBound ..]
      , b <- [False, True]
      ]
    pixel_values =
      [ Pixel r g b
      | r <- [minBound ..]
      , g <- [minBound ..]
      , b <- [minBound ..]
      ]

    conSPixel_values = P.map ConSPixel pixel_values

    conSLargeSum_values = P.map ConSLargeSum [minBound ..]
    conSEmpty_values = [ConSEmpty]
    allValues =
      conSMixed_values
        <> conSPixel_values
        <> conSLargeSum_values
        <> conSEmpty_values
  mapM_ packUnpackIdentity allValues

case_soPComplexTags :: Assertion
case_soPComplexTags = do
  let
    conSMixed_values =
      [ ConSMixed u b
      | u <- [minBound ..]
      , b <- [False, True]
      ]
    pixel_values =
      [ Pixel r g b
      | r <- [minBound ..]
      , g <- [minBound ..]
      , b <- [minBound ..]
      ]

    conSPixel_values = P.map ConSPixel pixel_values

    conSLargeSum_values = P.map ConSLargeSum [minBound ..]
    conSEmpty_values = [ConSEmpty]

  let check_tag expected v = expected @?= (v !! (0 :: Int))

  mapM_ (check_tag 0) (packC BigEndian <$> conSMixed_values)
  mapM_ (check_tag 1) (packC BigEndian <$> conSPixel_values)
  mapM_ (check_tag 2) (packC BigEndian <$> conSLargeSum_values)
  mapM_ (check_tag 3) (packC BigEndian <$> conSEmpty_values)

case_soPComplexPixelOffsets :: Assertion
case_soPComplexPixelOffsets = do
  let
    pixel_values =
      [ Pixel r g b
      | r <- [minBound ..]
      , g <- [minBound ..]
      , b <- [minBound ..]
      ]
    conSPixel_values = P.map ConSPixel pixel_values

  forM_ conSPixel_values $ \sPixel@(ConSPixel pixel) -> do
    packUnpackIdentity pixel
    forM_ [BigEndian, LittleEndian] $ \endian -> do
      let bytes = packC endian sPixel
      -- tag
      bytes `idx` 0 @?= 1

      bytes `idx` 1 @?= resize (pack pixel.rCh)
      bytes `idx` 2 @?= resize (pack pixel.gCh)
      bytes `idx` 3 @?= resize (pack pixel.bCh)

case_withVec :: Assertion
case_withVec = do
  let
    u2Values = [minBound ..]
    allVecValues =
      [ WithVec (x :> y :> z :> Nil)
      | x <- u2Values
      , y <- u2Values
      , z <- u2Values
      ]
  mapM_ packUnpackIdentity allVecValues

  forM_ allVecValues $ \wv@(WithVec vec) -> do
    forM_ [LittleEndian, BigEndian] $ \endian -> do
      let bytes = packC endian wv
      bytes `idx` 0 @?= (resize . pack $ vec `idx` 0)
      bytes `idx` 1 @?= (resize . pack $ vec `idx` 1)
      bytes `idx` 2 @?= (resize . pack $ vec `idx` 2)

case_withBV :: Assertion
case_withBV = do
  let allValues = [WithBV bv | bv <- [minBound ..]]
  mapM_ packUnpackIdentity allValues

case_multiFieldProduct :: Assertion
case_multiFieldProduct = do
  let allValues =
        [ MFP b u3 s5 idx8 bv1 unit cb
        | b <- [False, True]
        , u3 <- [minBound ..]
        , s5 <- [minBound ..]
        , idx8 <- [minBound ..]
        , bv1 <- [minBound ..]
        , unit <- [()]
        , cb <- [minBound ..]
        ]
  mapM_ packUnpackIdentity allValues

  forM_ allValues $ \val -> do
    forM_ [LittleEndian, BigEndian] $ \endian -> do
      let bytes = packC endian val
      bytes `idx` 0 @?= (resize . pack $ val.fieldBool)
      bytes `idx` 1 @?= (resize . pack $ val.fieldU3)
      bytes `idx` 2 @?= pack (signExtend val.fieldS5)
      bytes `idx` 3 @?= (resize . pack $ val.fieldIdx8)
      bytes `idx` 4 @?= (resize . pack $ val.fieldBV1)
      -- no test for unit! This one isn't packed
      -- and the offset of fieldCustomB below shows that!
      bytes `idx` 5 @?= (resize . pack $ val.fieldCustomB)

case_myProductByteOrder :: Assertion
case_myProductByteOrder = do
  let val = MySumPayload 0xABCD
  let bytesLe = packC LittleEndian val

  bytesLe `idx` 0 @?= 0 -- tag
  bytesLe `idx` 1 @?= 0xCD
  bytesLe `idx` 2 @?= 0xAB

  let bytesBe = packC BigEndian val

  bytesBe `idx` 0 @?= 0 -- tag
  bytesBe `idx` 1 @?= 0xAB
  bytesBe `idx` 2 @?= 0xCD

  let bytesNoPayload = packC LittleEndian MySumSecondCase
  bytesNoPayload `idx` 0 @?= 1 -- tag

case_eitherB8B16 :: Assertion
case_eitherB8B16 = do
  mapM_
    (packUnpackIdentity @(Either (BitVector 8) (BitVector 16)))
    ([Left x | x <- [minBound ..]] <> [Right x | x <- [minBound ..]])

case_eitherU8U16 :: Assertion
case_eitherU8U16 = do
  mapM_
    (packUnpackIdentity @(Either (Unsigned 8) (Unsigned 16)))
    ([Left x | x <- [minBound ..]] <> [Right x | x <- [minBound ..]])

case_eitherS8S16 :: Assertion
case_eitherS8S16 = do
  mapM_
    (packUnpackIdentity @(Either (Signed 8) (Signed 16)))
    ([Left x | x <- [minBound ..]] <> [Right x | x <- [minBound ..]])

case_eitherI256I65536 :: Assertion
case_eitherI256I65536 = do
  mapM_
    (packUnpackIdentity @(Either (Index 256) (Index 65536)))
    ([Left x | x <- [minBound ..]] <> [Right x | x <- [minBound ..]])

case_eitherB7B15 :: Assertion
case_eitherB7B15 = do
  mapM_
    (packUnpackIdentity @(Either (BitVector 7) (BitVector 15)))
    ([Left x | x <- [minBound ..]] <> [Right x | x <- [minBound ..]])

case_eitherU7U15 :: Assertion
case_eitherU7U15 = do
  mapM_
    (packUnpackIdentity @(Either (Unsigned 7) (Unsigned 15)))
    ([Left x | x <- [minBound ..]] <> [Right x | x <- [minBound ..]])

case_eitherS7S15 :: Assertion
case_eitherS7S15 = do
  mapM_
    (packUnpackIdentity @(Either (Signed 7) (Signed 15)))
    ([Left x | x <- [minBound ..]] <> [Right x | x <- [minBound ..]])

case_eitherI255I65535 :: Assertion
case_eitherI255I65535 = do
  mapM_
    (packUnpackIdentity @(Either (Index 255) (Index 65535)))
    ([Left x | x <- [minBound ..]] <> [Right x | x <- [minBound ..]])

case_indexMaxBoundExeededError :: Assertion
case_indexMaxBoundExeededError = do
  -- we can't test maxBounds of byte-multiple sizes because
  -- the test would overflow when trying to construct a value larger than
  -- what can fit in the amount of bytes.
  testNat (Proxy @1)
  testNat (Proxy @20)
  testNat (Proxy @254)
  testNat (Proxy @203898924)
 where
  testNat :: forall n. (KnownNat n, 1 <= n) => Proxy n -> Assertion
  testNat Proxy = do
    let maxVal = maxBound :: Index n
    let maxBv = pack $ packC LittleEndian maxVal
    let oneLargerBv = maxBv + 1
    let oneLarger = unpack oneLargerBv
    maybeUnpackC @(Index n) LittleEndian oneLarger @?= Nothing

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
