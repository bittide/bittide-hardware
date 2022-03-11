{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# LANGUAGE GADTs #-}
module Tests.DoubleBufferedRAM(ramGroup) where
import Clash.Prelude
import Bittide.DoubleBufferedRAM
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog.Gen as Gen
import Clash.Sized.Vector ( unsafeFromList )
import Hedgehog.Range as Range
import qualified GHC.TypeNats as TN
import qualified Prelude as P
import qualified Data.List as L
import qualified Data.Set as Set

ramGroup :: TestTree
ramGroup = testGroup "DoubleBufferedRAM group"
  [ testProperty "Reading the buffer." readDoubleBufferedRAM
  , testProperty "Wriing and reading back buffers." readWriteDoubleBufferedRAM]

-- | RamContents is a data type containing a Vec (extra + n) Int, this can be used to
-- safisfy the 1 <= size constraints imposed by the topEntities.
data RamContents extra where
  RamContents :: (1 <= (extra + n)) => SNat n -> Vec (n + extra) Int -> RamContents extra

instance Show (RamContents extra) where
  show (RamContents SNat c) = show c

genRamContents :: Int -> Gen (RamContents 1)
genRamContents depth = do
  case TN.someNatVal . fromIntegral $ depth - 1 of
    (SomeNat extraDepth) -> do
      contents <- Gen.list (Range.singleton $ fromIntegral depth) $ Gen.int Range.constantBounded
      return $ RamContents (snatProxy extraDepth) $ unsafeFromList contents

-- | This test checks if we can read the inital values of the double buffered RAM.
readDoubleBufferedRAM :: Property
readDoubleBufferedRAM = property $ do
  ramDepth <- forAll $ Gen.enum 1 31
  ramContents <- forAll $ genRamContents ramDepth
  case ramContents of
    RamContents SNat contents -> do
      simLength <- forAll $ Gen.enum 1 100
      let simRange = Range.singleton $ fromIntegral simLength
      switchSignal <- forAll $ Gen.list simRange Gen.bool
      readAddresses <- forAll $ Gen.list simRange $ Gen.enum 0 (fromIntegral $ ramDepth - 1)
      let
        topEntity (unbundle -> (switch, readAddr)) = withClockResetEnable @System clockGen
          resetGen enableGen doubleBufferedRAM contents switch readAddr (pure Nothing)
        topEntityInput = P.zip switchSignal readAddresses
        simOut = P.tail $ simulateN simLength topEntity topEntityInput
        expectedOut = [contents !! i | i <- readAddresses]
      simOut === P.init expectedOut

-- | This test checks if we can write new values to the double buffered RAM and read them.
readWriteDoubleBufferedRAM :: Property
readWriteDoubleBufferedRAM = property $ do
  ramDepth <- forAll $ Gen.enum 2 31
  ramContents <- forAll $ genRamContents ramDepth
  let minSimLength = 2 * ramDepth
  simLength <- forAll $ Gen.enum minSimLength 100
  case ramContents of
    RamContents SNat contents -> do
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
