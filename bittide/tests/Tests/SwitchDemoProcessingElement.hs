-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.SwitchDemoProcessingElement where

import Clash.Prelude hiding (someNatVal, withSomeSNat)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Control.Monad (forM_)
import Data.Proxy (Proxy (..))
import GHC.TypeNats (someNatVal)

import Bittide.SwitchDemoProcessingElement

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Clash.Explicit.Prelude as E
import qualified Data.List as L

import Clash.Explicit.Reset (noReset)
import Clash.Hedgehog.Sized.BitVector (genDefinedBitVector)
import Clash.Hedgehog.Sized.Index (genIndex)
import Clash.Hedgehog.Sized.Unsigned (genUnsigned)
import Hedgehog ((===))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "SwitchDemoProcessingElement"
    [ testPropertyNamed "prop_readThenWrite" "prop_readThenWrite" prop_readThenWrite
    , testCase "case_zeroExtendTimesThree" case_zeroExtendTimesThree
    ]

withSomeSNat :: Natural -> (forall (n :: Nat). SNat n -> r) -> r
withSomeSNat n f = case someNatVal n of
  SomeNat (_ :: Proxy n) -> f (SNat @n)

linearLength :: (Integral a) => a -> a -> Range.Range a
linearLength start len = Range.linear start (start + len)

singletonInt :: (Integral a) => a -> Range.Range Int
singletonInt = Range.singleton . fromIntegral

-- | Exhaustive test for 'zeroExtendTimesThree' for n ~ [1..64]
case_zeroExtendTimesThree :: Assertion
case_zeroExtendTimesThree =
  forM_ [0 .. 63] $ \nMinusOne ->
    withSomeSNat nMinusOne $ \(succSNat -> (SNat :: SNat n)) -> do
      forM_ [(0 :: Index n) ..] $ \i -> do
        let
          actual = fromIntegral (zeroExtendTimesThree @n i)
          expected = fromIntegral @_ @Integer i * 3
        actual @?= expected

prop_readThenWrite :: H.Property
prop_readThenWrite = H.property $ do
  bufferSizeMinusOne <- H.forAll $ Gen.integral (Range.linear 0 10)
  withSomeSNat bufferSizeMinusOne $ \(succSNat -> bufferSizeSNat@(SNat :: SNat bufferSize)) -> do
    nReadTriCycles <-
      H.forAll
        $ Gen.frequency
          [ (30, Gen.constant 0)
          , (70, genIndex Range.linearBounded)
          ]
    nWriteTriCycles <-
      if nReadTriCycles == 0
        then H.forAll $ genIndex (Range.linear 1 maxBound)
        else H.forAll $ genIndex Range.linearBounded

    let
      cyclesPerReadWrite = 3 :: Unsigned 64
      nReadCycles = cyclesPerReadWrite * fromIntegral nReadTriCycles
      nWriteCycles = cyclesPerReadWrite * fromIntegral nWriteTriCycles
      maxIdle1 = 100
      maxIdle2 = 100

    -- Notice that the PE needs a single clock cycle in its idle state to function
    -- correctly. Hence, we always start reading a minimum at clockStart+1.
    readData <- H.forAll $ Gen.list (singletonInt nReadCycles) genDefinedBitVector
    clockStart <- H.forAll $ genUnsigned @64 (Range.linear 0 100)
    readStart <-
      H.forAll
        $ Gen.frequency
          [ (30, Gen.constant clockStart)
          , (70, genUnsigned @64 (linearLength clockStart maxIdle1))
          ]
    let readEnd = readStart + fromIntegral nReadCycles
    writeStart <-
      H.forAll
        $ Gen.frequency
          [ (30, Gen.constant readEnd)
          , (70, genUnsigned @64 (linearLength readEnd maxIdle2))
          ]
    deviceDna <- H.forAll genDefinedBitVector

    let immediateRead = nReadCycles > 0 && readStart == clockStart
    H.cover 5 "Read in the very first cycle we're allowed to" immediateRead

    let immediateWrite = nWriteCycles > 0 && writeStart == clockStart
    H.cover 3 "Write in the very first cycle we're allowed to" immediateWrite

    let isBackToBack = nReadCycles > 0 && nWriteCycles > 0 && readEnd == writeStart
    H.cover 5 "Back-to-back read/write" isBackToBack

    let
      idle1length = readStart - clockStart
      idle2length = writeStart - readEnd
      idle1in = L.replicate (fromIntegral idle1length) 0
      crossBarIn = fromList (idle1in <> readData <> L.repeat 0)

      out =
        E.sample
          $ bundle
          $ withClockResetEnable @System clockGen noReset enableGen
          $ (\(a, b, _) -> (a, b))
          $ switchDemoPe
            bufferSizeSNat
            (fromList [clockStart ..])
            crossBarIn
            (pure deviceDna)
            (pure readStart)
            (pure nReadTriCycles)
            (pure writeStart)
            (pure nWriteTriCycles)

      (idle1out, rest0) = L.splitAt (fromIntegral idle1length) out
      (readOut, rest1) = L.splitAt (fromIntegral nReadCycles) rest0
      (idle2out, rest2) = L.splitAt (fromIntegral idle2length) rest1
      (writeOut, rest3) = L.splitAt (fromIntegral nWriteCycles) rest2

      (idle1outs, _idle1buffers) = L.unzip idle1out
      (readOuts, _readBuffers) = L.unzip readOut
      (idle2outs, _idle2buffers) = L.unzip idle2out
      (writeOuts, _writeBuffers) = L.unzip writeOut
      (_restOuts, restBuffers) = L.unzip rest3

    H.footnote $ "idle1in: " <> show idle1in
    H.footnote $ "idle1length: " <> show idle1length
    H.footnote $ "idle2length: " <> show idle2length
    H.footnote $ "nReadCycles: " <> show nReadCycles
    H.footnote $ "nWriteCycles: " <> show nWriteCycles
    H.footnote $ "readData: " <> show readData
    H.footnote $ "readEnd: " <> show readEnd
    H.footnote $ "readStart: " <> show readStart
    H.footnote $ "writeStart: " <> show writeStart
    H.footnote $ "clockStart: " <> show clockStart
    H.footnote $ "idle2outs: " <> showX idle2outs
    H.footnote $ "bufferSizeSNat: " <> show bufferSizeSNat

    -- Check that at the end of the simulation the buffer is what we expect it
    -- to be. The buffer should be equal to the data we send to the PE. We don't
    -- care about data we don't write, hence we truncate (L.take) the buffer to
    -- match the number of read cycles.
    case restBuffers of
      [] -> error "Unexpected end of output"
      (buffer : _) -> do
        H.footnote $ "buffer: " <> show buffer
        L.take (fromIntegral nReadCycles) (toList buffer) === readData

    -- Check that idle value is written at correct times
    let idleValue = resize $ complement deviceDna
    idle1outs === L.replicate (L.length idle1outs) idleValue
    readOuts === L.replicate (L.length readOuts) idleValue
    idle2outs === L.replicate (L.length idle2outs) idleValue

    -- Check that the right data is written the crossbar link at the time we
    -- expect it to.
    let
      -- Note we can always write one tri-cycle more than we read, since internal
      -- data comes first.
      relevantOutCycles = fromIntegral (min nWriteCycles (nReadCycles + cyclesPerReadWrite))
      deviceDnaVec = reverse $ bitCoerce @_ @(Vec 2 (BitVector 64)) (zeroExtend deviceDna)
      expectedOutData = toList (pack writeStart :> deviceDnaVec) <> readData
    L.take relevantOutCycles writeOuts === L.take relevantOutCycles expectedOutData
