-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Axi4.Generators where

import Clash.Prelude
import Protocols.Axi4.Stream
import Hedgehog
import Clash.Hedgehog.Sized.Unsigned
import Clash.Hedgehog.Sized.Vector (genVec)
import Data.Maybe
import Data.Proxy
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.List as L
import qualified Hedgehog.Internal.Property as H

tests :: TestTree
tests = testGroup "Axi4Stream Generators"
  [ testProperty "genAxisM2S" prop_genAxisM2S
  ]

-- | Byte types for Axi4StreamM2S transactions
data AxisByteType
  = Null
  | Data
  | Position
  | Reserved
  deriving (Show, Eq)

type Keep = Bool
type Strobe = Bool

-- | Get a list of byte types for a given Axi4StreamM2S transaction
getByteTypes :: Axi4StreamM2S conf a -> Vec (DataWidth conf) AxisByteType
getByteTypes Axi4StreamM2S{..} = zipWith getByteType _tkeep _tstrb

-- | Get the byte type based on a keep and strobe value
getByteType :: Keep -> Strobe -> AxisByteType
getByteType True True = Data
getByteType True False = Position
getByteType False False = Null
getByteType False True = Reserved

getKeepStrobe :: AxisByteType -> (Keep, Strobe)
getKeepStrobe Null = (False, False)
getKeepStrobe Data = (True, True)
getKeepStrobe Position = (True, False)
getKeepStrobe Reserved = (False, True)

-- | Generates a directed Axi4StreamM2S transaction.
genAxisM2S ::
  -- | Data width of the Axi4StreamM2S transaction
  SNat dataWidth ->
  -- | ID width of the Axi4StreamM2S transaction
  SNat idWidth ->
  -- | Destination width of the Axi4StreamM2S transaction
  SNat destWidth ->
  -- | Allowed byte types to generate (can be used to skew the distribution of byte types)
  [AxisByteType] ->
  -- | Allowed _tlast values to generate (can be used to skew the distribution of _tlast values)
  [Bool] ->
  -- | Generator for user data
  Gen userType ->
  Gen (Axi4StreamM2S ('Axi4StreamConfig dataWidth idWidth destWidth) userType)
genAxisM2S SNat SNat SNat byteTypes lastValues genUser = do
  bytes  <- genVec $ Gen.choice $ fmap pure byteTypes
  let (_tkeep, _tstrb) = unzip $ map getKeepStrobe bytes
  _tdata <- genVec $ genUnsigned Range.constantBounded
  _tlast <- Gen.choice $ fmap pure lastValues
  _tid   <- genUnsigned Range.constantBounded
  _tdest <- genUnsigned Range.constantBounded
  _tuser <- genUser
  pure $ Axi4StreamM2S{..}

prop_genAxisM2S :: Property
prop_genAxisM2S = property $ do
  dataWidth <- forAll $ Gen.int (Range.constant 1 4)
  case TN.someNatVal $ fromIntegral dataWidth of
    SomeNat (Proxy :: Proxy dataWidth) -> do
      let
        allowedByteTypes = L.filter (not . null) $ L.subsequences [Null, Data, Position, Reserved]
        allowedLasts = [[False], [True], [False, True]]
      byteTypes <- forAll $ Gen.choice $ pure <$> allowedByteTypes
      lastValues <- forAll $ Gen.choice $ fmap pure allowedLasts
      axi <- forAll $ genAxisM2S (SNat @dataWidth) d8 d8 byteTypes lastValues (pure ())
      let axiBytes = getByteTypes axi
      cover 40 "tlast" (_tlast axi)
      cover 40 "not tlast" (not $ _tlast axi)
      mapM_ (\ byte -> cover 25 (H.LabelName $ "One or more " <> show byte) (isJust $ elemIndex byte axiBytes)) byteTypes
      mapM_ (\ byte -> cover 1 (H.LabelName $ "All " <> show byte) (all (== byte) axiBytes)) byteTypes
      mapM_ (\ byte -> cover 1 (H.LabelName $ "No " <> show byte) (byte `notElem` axiBytes)) byteTypes
      assert (all (`L.elem` byteTypes) $ toList axiBytes)
      assert (_tlast axi `L.elem` lastValues)

data PacketDensity
  = Sparse
  | Dense
  deriving (Show, Eq)

-- | Generate a list of Axi4StreamM2S transactions that form a single packet, only the last transaction
-- will have _tlast set to True. When `allowSparse` is set to False, only the last transaction can hold
-- null bytes. These null bytes will only appear at the end of the packet.
genAxiPacket ::
  -- | Data width of the Axi4StreamM2S transaction
  SNat dataWidth ->
  -- | ID width of the Axi4StreamM2S transaction
  SNat idWidth ->
  -- | Destination width of the Axi4StreamM2S transaction
  SNat destWidth ->
  -- | Allow sparse packet
  PacketDensity ->
  -- | Allowed byte types to generate (can be used to skew the distribution of byte types)
  -- | A
  [AxisByteType] ->
  -- | Range for the length of the packet, excluding the last transaction
  Range Int ->
  -- | Generator for user data
  Gen userType ->
  -- | Generator for a list of transactions representing a packet
  Gen [Maybe (Axi4StreamM2S ('Axi4StreamConfig dataWidth idWidth destWidth) userType)]
genAxiPacket SNat SNat SNat density byteTypes range genUser = do
  -- Generate init transactions
  let initByteTypes = if density == Sparse then byteTypes else L.filter (/= Null) byteTypes
  packetInit <- Gen.list range (Gen.maybe $ genAxisM2S SNat SNat SNat initByteTypes [False] genUser)

  -- Generate last transaction
  let packetLastGen = genAxisM2S SNat SNat SNat byteTypes [True] genUser
  let hasRising axi = or $ snd $ mapAccumL (\prev curr -> (not prev && curr, curr)) True (_tkeep axi)

  -- Filter out transactions that have gaps in the data
  packetLast <- if density == Sparse then packetLastGen else Gen.filter (not . hasRising) packetLastGen
  pure (L.tail $ packetInit <> [Just packetLast])
