{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}

module Tests.Axi4.Generators where

import Clash.Prelude

import Clash.Hedgehog.Sized.Unsigned
import Clash.Hedgehog.Sized.Vector (genVec)
import Data.Maybe
import Data.Proxy
import Hedgehog
import Protocols.Axi4.Stream
import Test.Tasty
import Test.Tasty.Hedgehog

import Tests.Axi4.Properties
import Tests.Axi4.Types

import qualified Data.List as L
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Internal.Property as H
import qualified Hedgehog.Range as Range

tests :: TestTree
tests =
  testGroup
    "Axi4Stream Generators"
    [ testProperty "genAxisM2S" prop_genAxisM2S
    , testProperty "genRandomAxiPacket" prop_genRandomAxiPacket
    ]

-- | Generates a directed Axi4StreamM2S transaction.
genAxisM2S ::
  -- | Data width of the Axi4StreamM2S transaction
  SNat dataWidth ->
  -- | ID width of the Axi4StreamM2S transaction
  SNat idWidth ->
  -- | Destination width of the Axi4StreamM2S transaction
  SNat destWidth ->
  -- | Allowed byte types to generate (can be used to skew the distribution of byte types)
  [AxiByteType] ->
  -- | Allowed _tlast values to generate (can be used to skew the distribution of _tlast values)
  [Bool] ->
  -- | Generator for user data
  Gen userType ->
  Gen (Axi4StreamM2S ('Axi4StreamConfig dataWidth idWidth destWidth) userType)
genAxisM2S SNat SNat SNat byteTypes lastValues genUser = do
  bytes <- genVec $ Gen.choice $ fmap pure byteTypes
  let (_tkeep, _tstrb) = unzip $ map getKeepStrobe bytes
  _tdata <- genVec $ genUnsigned Range.constantBounded
  _tlast <- Gen.choice $ fmap pure lastValues
  _tid <- genUnsigned Range.constantBounded
  _tdest <- genUnsigned Range.constantBounded
  _tuser <- genUser
  pure $ Axi4StreamM2S{..}

prop_genAxisM2S :: Property
prop_genAxisM2S = property $ do
  dataWidth <- forAll (TN.someNatVal . fromIntegral <$> Gen.int (Range.constant 1 4))
  case dataWidth of
    SomeNat (Proxy :: Proxy dataWidth) -> do
      let
        allowedByteTypes = L.filter (not . null) $ L.subsequences [NullByte, DataByte, PositionByte]
        allowedLasts = [[False], [True], [False, True]]
      byteTypes <- forAll $ Gen.choice $ pure <$> allowedByteTypes
      lastValues <- forAll $ Gen.choice $ fmap pure allowedLasts
      axi <- forAll $ genAxisM2S (SNat @dataWidth) d8 d8 byteTypes lastValues (pure ())
      let axiBytes = getByteType <$> getTransferBytes axi
      cover 40 "tlast" (_tlast axi)
      cover 40 "not tlast" (not $ _tlast axi)
      mapM_
        ( \byte ->
            cover 25 (H.LabelName $ "One or more " <> show byte) (isJust $ elemIndex byte axiBytes)
        )
        byteTypes
      mapM_
        (\byte -> cover 1 (H.LabelName $ "All " <> show byte) (all (== byte) axiBytes))
        byteTypes
      mapM_
        (\byte -> cover 1 (H.LabelName $ "No " <> show byte) (byte `notElem` axiBytes))
        byteTypes
      assert (all (`L.elem` byteTypes) $ toList axiBytes)
      assert (_tlast axi `L.elem` lastValues)

data PacketDensity
  = Sparse
  | Dense
  deriving (Show, Eq)

{- | Generate a list of Axi4StreamM2S transactions that form a single packet, only the last transaction
will have _tlast set to True. The packet
-}
genRandomAxiPacket ::
  -- | Data width of the Axi4StreamM2S transaction
  SNat dataWidth ->
  -- | ID width of the Axi4StreamM2S transaction
  SNat idWidth ->
  -- | Destination width of the Axi4StreamM2S transaction
  SNat destWidth ->
  -- | Allowed byte types to generate (can be used to skew the distribution of byte types)
  [AxiByteType] ->
  -- | Range for the length of the packet, excluding the last transaction
  Range Int ->
  -- | Generator for user data
  Gen userType ->
  -- | Generator for a list of transactions representing a packet
  Gen [Maybe (Axi4StreamM2S ('Axi4StreamConfig dataWidth idWidth destWidth) userType)]
genRandomAxiPacket SNat SNat SNat byteTypes range genUser = do
  packetInit <-
    Gen.list range (Gen.maybe $ genAxisM2S SNat SNat SNat byteTypes [False] genUser)
  packetLast <- genAxisM2S SNat SNat SNat byteTypes [True] genUser
  pure (L.tail $ packetInit <> [Just packetLast])

prop_genRandomAxiPacket :: Property
prop_genRandomAxiPacket = property $ do
  dataWidth <- forAll (TN.someNatVal . fromIntegral <$> Gen.int (Range.constant 1 8))
  case dataWidth of
    (SomeNat (Proxy :: Proxy dataWidth)) -> do
      let
        byteTypes = [NullByte, DataByte, PositionByte]
      transfers <-
        forAll
          $ genRandomAxiPacket (SNat @dataWidth) d0 d0 byteTypes (Range.constant 1 16) (pure ())
      let
        packet = catMaybes transfers
        axiBytes = getPacketByteTypes packet
      cover 1 "hasLeadingNullBytes" $ hasLeadingNullBytes axiBytes
      cover 1 "hasTrailingNullBytes" $ hasTrailingNullBytes axiBytes
      cover 1 "isPackedAxi4StreamPacket" $ isPackedAxi4StreamPacket axiBytes
      cover 1 "isStrictlySparseAxi4StreamPacket" $ isStrictlySparseAxi4StreamPacket axiBytes
      cover 1 "isContinuousAxi4StreamPacket" $ isContinuousAxi4StreamPacket axiBytes
      cover 1 "isAlignedAxi4StreamPacket" $ isAlignedAxi4StreamPacket axiBytes
      cover 1 "isUnalignedAxi4StreamPacket" $ isUnalignedAxi4StreamPacket axiBytes
      cover 1 "unInterruptedAxi4Packets" $ unInterruptedAxi4Packets transfers
      assert (isSinglePacket packet)
