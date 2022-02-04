{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# LANGUAGE GADTs #-}
module Tests.Switch(switchGroup) where
import Bittide.Switch
import Clash.Prelude
import Clash.Sized.Vector ( unsafeFromList)
import Data.Maybe
import Data.String
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Data.Set as Set
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude as P
switchGroup :: TestTree
switchGroup = testGroup "Switch group"
  [testProperty "Switch - Routing works" switchFrameRoutingWorks]

data SwitchCalendar extra where
  SwitchCalendar :: (1 <= (extra + links), 1 <= (extra + depth)) =>
   SNat links -> SNat depth ->
   Vec (extra + depth) (Vec (extra + links) (Index (extra + depth), Index (links + extra + 1))) ->
   SwitchCalendar extra

instance Show (SwitchCalendar extra) where
  show (SwitchCalendar l@SNat d@SNat list) = "links: " P.++ show l P.++ "\n\rdepth: " P.++ show d P.++ "\n\r" P.++ show list

genSwitchCalendar :: Gen (SwitchCalendar 1)
genSwitchCalendar = do
  links <- Gen.enum 0 7
  depth <- Gen.enum 0 255
  case (TN.someNatVal links, TN.someNatVal depth) of
    (SomeNat l, SomeNat d) -> do
      let
       realDepth = depth + 1
       realLinks = links + 1
       scatterPerLink = unsafeFromList <$> Gen.shuffle [0..fromIntegral depth]
       crossbarPerEntry = unsafeFromList <$> Gen.shuffle [1.. fromIntegral realLinks]

      scatterCal <- transpose . unsafeFromList <$> Gen.list (Range.singleton $ fromIntegral realLinks) scatterPerLink
      crossbarCal <- unsafeFromList <$> Gen.list (Range.singleton $ fromIntegral realDepth) crossbarPerEntry

      let cal = zip <$> scatterCal <*> crossbarCal
      return (SwitchCalendar (snatProxy l) (snatProxy d) cal)

switchFrameRoutingWorks :: Property
switchFrameRoutingWorks = property $ do
  switchCal <- forAll genSwitchCalendar
  case switchCal of
    SwitchCalendar l@SNat d@SNat cal -> do
      let
        depth = 1 + snatToNum d
        links = 1 + snatToNum l
      simLength <- forAll $ (\l' -> l'*depth + 2) <$> Gen.enum 0 10

      let
        outputLength = simLength - depth - 2
        genFrame = Just <$> Gen.integral Range.linearBounded
        allLinks = Gen.list (Range.singleton links) genFrame
      allInputCycles <- forAll $ Gen.list (Range.singleton outputLength) allLinks

      let
        topEntityInput = (P.replicate links Nothing) : allInputCycles P.++ P.repeat (P.replicate links Nothing)
        topEntity = exposeClockResetEnable (switch @_ @_ @_ @_ @64) clockGen resetGen enableGen
        testBench inp = topEntity cal (pure Nothing) inp
        simOut = simulateN @System simLength testBench $ fmap unsafeFromList topEntityInput
        simOut' = P.drop (depth + 2) $ fmap toList simOut

      let expectedOutput = P.concat $ P.take outputLength allInputCycles
      footnote . fromString $ "expected:" <> showX expectedOutput
      footnote . fromString $ "simOut': " <> showX simOut'
      footnote . fromString $ "simOut: " <> showX simOut
      footnote . fromString $ "input: " <> (showX $ P.take outputLength topEntityInput)
      Set.fromList (P.concat simOut') === Set.fromList [ f | f <- expectedOutput, isJust f]
