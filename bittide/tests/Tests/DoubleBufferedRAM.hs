{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Tests.DoubleBufferedRAM(ramGroup) where

import Clash.Prelude
import Clash.Hedgehog.Sized.Vector

import Bittide.DoubleBufferedRAM
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Prelude as P

deriving instance (Show a) => Show (SomeVec 1 a)

ramGroup :: TestTree
ramGroup = testGroup "DoubleBufferedRAM group"
  [ testPropertyNamed "Reading the buffer." "readDoubleBufferedRAM" readDoubleBufferedRAM
  , testPropertyNamed "Wriing and reading back buffers." "readWriteDoubleBufferedRAM" readWriteDoubleBufferedRAM]

genRamContents :: (MonadGen m, Integral i) => i -> m a -> m (SomeVec 1 a)
genRamContents depth = genSomeVec (Range.singleton $ fromIntegral (depth - 1))

-- | This test checks if we can read the inital values of the double buffered RAM.
readDoubleBufferedRAM :: Property
readDoubleBufferedRAM = property $ do
  ramDepth <- forAll $ Gen.int (Range.constant 1 31)
  ramContents <- forAll $ genRamContents ramDepth $ Gen.int Range.constantBounded
  case ramContents of
    SomeVec SNat contents -> do
      simLength <- forAll $ Gen.int (Range.constant 1 100)
      let simRange = Range.singleton simLength
      switchSignal <- forAll $ Gen.list simRange Gen.bool
      readAddresses <- forAll $ Gen.list simRange $ Gen.enum 0 (fromIntegral $ ramDepth - 1)
      let
        topEntity (unbundle -> (switch, readAddr)) = withClockResetEnable @System clockGen
          resetGen enableGen doubleBufferedRAM contents switch readAddr (pure Nothing)
        topEntityInput = P.zip switchSignal readAddresses
        simOut = P.tail $ simulateN simLength topEntity topEntityInput
        expectedOut = fmap (contents !!) readAddresses
      simOut === P.init expectedOut

-- | This test checks if we can write new values to the double buffered RAM and read them.
readWriteDoubleBufferedRAM :: Property
readWriteDoubleBufferedRAM = property $ do
  ramDepth <- forAll $ Gen.int (Range.constant 2 31)
  ramContents <- forAll $ genRamContents ramDepth $ Gen.int Range.constantBounded
  let minSimLength = 2 * ramDepth
  simLength <- forAll $ Gen.int (Range.constant minSimLength 100)
  case ramContents of
    SomeVec SNat contents -> do
      let
        topEntity (unbundle -> (switch, readAddr, writePort)) = withClockResetEnable
          @System clockGen resetGen enableGen doubleBufferedRAM contents switch readAddr
          writePort
      let
        addresses = cycle $ fmap fromIntegral [0..ramDepth-1]
        switchSignal = (==0) <$> addresses
      writeEntries <- forAll (Gen.list (Range.singleton simLength) $ Gen.int Range.constantBounded)
      let
        topEntityInput = L.zip3 switchSignal addresses $ fmap Just (P.zip addresses writeEntries)
        simOut = simulateN @System simLength topEntity topEntityInput
      Set.fromList simOut === Set.fromList (toList contents <> L.take (simLength - ramDepth - 1) writeEntries)
