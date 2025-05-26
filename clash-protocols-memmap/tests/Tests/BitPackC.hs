-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}

module Tests.BitPackC where

import Clash.Explicit.Prelude

import BitPackC

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import qualified Prelude as P

-- Helper for testing the pack/unpack identity
packUnpackIdentity :: forall a. (Eq a, Show a, BitPackC a) => a -> Assertion
packUnpackIdentity val = unpackC (packC val) @?= val

newtype MyInt = MkMyInt (Signed 10)
  deriving (Show, Eq, Generic, NFDataX, BitPackC)

-- data MyUnit = MyUnitPayload ()
--   deriving (Show, Eq, Generic, NFDataX, BitPackC)

data MyCustomBool = MyFalse | MyTrue
  deriving (Show, Eq, Generic, NFDataX, BitPackC, Enum, Bounded)

data SmallSum = S1 | S2 | S3
  deriving (Show, Eq, Generic, NFDataX, BitPackC, Enum, Bounded)

data Point2D = Point2D {pX :: Unsigned 4, pY :: Unsigned 4}
  deriving (Show, Eq, Generic, NFDataX, BitPackC)

data Pixel = Pixel {rCh :: Unsigned 4, gCh :: Unsigned 5, bCh :: Unsigned 6}
  deriving (Show, Eq, Generic, NFDataX, BitPackC)

data Choice
  = OptA (Signed 5)
  | OptB Bool
  deriving (Show, Eq, Generic, NFDataX, BitPackC)

data LargeSum = L0 | L1 | L2 | L3 | L4 | L5 | L6 | L7
  deriving (Show, Eq, Generic, NFDataX, BitPackC, Enum, Bounded)

data NestedProd = NestedProd {npPoint :: Point2D, npActive :: Bool}
  deriving (Show, Eq, Generic, NFDataX, BitPackC)

data SoPComplex
  = ConSMixed (Unsigned 3) Bool
  | ConSPixel Pixel
  | ConSLargeSum LargeSum
  | ConSEmpty
  deriving (Show, Eq, Generic, NFDataX, BitPackC)

data WithVec = WithVec {wvItems :: Vec 3 (Unsigned 2)}
  deriving (Show, Eq, Generic, NFDataX, BitPackC)

data WithBV = WithBV {bvData :: BitVector 11}
  deriving (Show, Eq, Generic, NFDataX, BitPackC)

data MultiFieldProduct = MFP
  { fieldBool :: Bool
  , fieldU3 :: Unsigned 3
  , fieldS5 :: Signed 5
  , fieldIdx8 :: Index 8
  , fieldBV1 :: BitVector 1
  , -- , fieldUnit :: ()
    fieldCustomB :: MyCustomBool
  }
  deriving (Show, Eq, Generic, NFDataX, BitPackC)

case_myInt :: Assertion
case_myInt = mapM_ packUnpackIdentity [MkMyInt s | s <- [minBound ..]]

-- case_myUnit :: Assertion
-- case_myUnit = do
--   let allValues = [MyUnitPayload ()]
--   mapM_ packUnpackIdentity allValues

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

case_withBV :: Assertion
case_withBV = do
  let allValues = [WithBV bv | bv <- [minBound ..]]
  mapM_ packUnpackIdentity allValues

case_multiFieldProduct :: Assertion
case_multiFieldProduct = do
  let allValues =
        [ MFP b u3 s5 idx bv1 cb
        | b <- [False, True]
        , u3 <- [minBound ..]
        , s5 <- [minBound ..]
        , idx <- [minBound ..]
        , bv1 <- [minBound ..]
        , -- , unit <- [()]
        cb <- [minBound ..]
        ]
  mapM_ packUnpackIdentity allValues

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
