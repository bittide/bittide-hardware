-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}

module Tests.Axi4.Properties where

import Clash.Prelude
import Protocols.Axi4.Stream

import Data.Maybe
import Test.Tasty
import Tests.Axi4.Types
import "bittide-extra" Data.List.Extra (maybeHead, maybeLast)

import qualified Data.List as L
import qualified Test.Tasty.HUnit as HU

tests :: TestTree
tests =
  testGroup
    "Tests.Axi4.Properties"
    [ HU.testCase "case_isPackedAxi4StreamPacket" case_isPackedAxi4StreamPacket
    , HU.testCase "case_hasLeadingNullBytes" case_hasLeadingNullBytes
    , HU.testCase "case_hasTrailingNullBytes" case_hasTrailingNullBytes
    , HU.testCase "case_isSinglePacket" case_isSinglePacket
    , HU.testCase "case_isStrictlySparseAxi4StreamPacket" case_isStrictlySparseAxi4StreamPacket
    , HU.testCase "case_isUnalignedAxi4StreamPacket" case_isUnalignedAxi4StreamPacket
    , HU.testCase "case_unInterruptedAxi4Packets" case_unInterruptedAxi4Packets
    ]

-- | Apply filters to an Axi4StreamM2S packet
byteTypeFilter :: [[AxiByteType] -> Bool] -> [Axi4StreamM2S conf a] -> Bool
byteTypeFilter filters packet = all ($ getPacketByteTypes packet) filters

-- | A packet does not contain null bytes between the first and last data or position byte
isPackedAxi4StreamPacket :: [AxiByteType] -> Bool
isPackedAxi4StreamPacket =
  all (== NullByte)
    . dropWhile (/= NullByte) -- There should not be data or position bytes left
    . dropWhile (== NullByte) -- Find first null byte -- Find first null byte
    -- Find first data or position byte

{- | A strictly sparse packet contains at least one position byte between the first
and last data byte
-}
isStrictlySparseAxi4StreamPacket :: [AxiByteType] -> Bool
isStrictlySparseAxi4StreamPacket =
  elem DataByte
    . dropWhile (/= PositionByte) -- There should still be data bytes left
    . dropWhile (/= DataByte) -- Find first position byte -- Find first position byte
    -- Find first data byte

-- | Continuous packets do not contain null bytes between the first and last data byte
isContinuousAxi4StreamPacket :: [AxiByteType] -> Bool
isContinuousAxi4StreamPacket = notElem NullByte

-- | Aligned packets do not contain position bytes
isAlignedAxi4StreamPacket :: [AxiByteType] -> Bool
isAlignedAxi4StreamPacket = notElem PositionByte

{- | Unaligned packets contain position bytes at the beginning and/or end of the packet,
but not between the first and last data byte.
-}
isUnalignedAxi4StreamPacket :: [AxiByteType] -> Bool
isUnalignedAxi4StreamPacket bytes0
  | Just firstByte <- maybeHead bytes1
  , Just lastByte <- maybeLast bytes1 =
      ((== PositionByte) firstByte || (== PositionByte) lastByte)
        && not (isStrictlySparseAxi4StreamPacket bytes1)
  | otherwise = False
 where
  bytes1 = filter (/= NullByte) bytes0

{- | Leading null bytes are null bytes that appear before the first data or position byte.
If a packet contains only null bytes, this returns @False@.
-}
hasLeadingNullBytes :: [AxiByteType] -> Bool
hasLeadingNullBytes [] = False
hasLeadingNullBytes (x : xs) = x == NullByte && any (/= NullByte) xs

{- | Trailing null bytes are null bytes at the end of a packet
If a packet contains only null bytes, this returns @False@.
-}
hasTrailingNullBytes :: [AxiByteType] -> Bool
hasTrailingNullBytes [] = False
hasTrailingNullBytes xs = (L.last xs == NullByte) && any (/= NullByte) xs

{- | Check if a list of Axi4StreamM2S transfers form an uninterrupted stream.
When a packet transmission is started, all elements should be Just until the
last transfer of the packet is reached.
-}
unInterruptedAxi4Packets :: [Maybe (Axi4StreamM2S conf userType)] -> Bool
unInterruptedAxi4Packets xs = case break (maybe False _tlast) (dropWhile isNothing xs) of
  (payload, l : rest) ->
    all isJust (payload <> [l]) && unInterruptedAxi4Packets rest
  (ys, []) -> all isJust ys

{- | Check if a list of Axi4StreamM2S transfers form a single packet.
A list of `Axi4StreamM2S` transfers form a single packet if only the last transfer
has `_tlast` set.
-}
isSinglePacket :: [Axi4StreamM2S conf userType] -> Bool
isSinglePacket axis = case break _tlast axis of
  (_, [_]) -> True
  _ -> False

case_isPackedAxi4StreamPacket :: HU.Assertion
case_isPackedAxi4StreamPacket = do
  HU.assertBool "expected packed" $ isPackedAxi4StreamPacket []
  HU.assertBool "expected packed" $ isPackedAxi4StreamPacket [DataByte]
  HU.assertBool "expected packed" $ isPackedAxi4StreamPacket [DataByte, NullByte]
  HU.assertBool "expected packed" $ isPackedAxi4StreamPacket [PositionByte, NullByte]
  HU.assertBool "expected packed" $ isPackedAxi4StreamPacket [NullByte, DataByte]
  HU.assertBool "expected packed" $ isPackedAxi4StreamPacket [DataByte, NullByte]
  HU.assertBool "expected packed" $ isPackedAxi4StreamPacket [NullByte, NullByte, NullByte]
  HU.assertBool "expected unpacked"
    $ (not . isPackedAxi4StreamPacket) [DataByte, NullByte, DataByte]
  HU.assertBool "expected unpacked"
    $ (not . isPackedAxi4StreamPacket) [NullByte, PositionByte, NullByte, PositionByte]
  HU.assertBool "expected unpacked"
    $ (not . isPackedAxi4StreamPacket) [NullByte, DataByte, NullByte, PositionByte]
  HU.assertBool "expected unpacked"
    $ (not . isPackedAxi4StreamPacket) [DataByte, NullByte, PositionByte, NullByte]

case_isStrictlySparseAxi4StreamPacket :: HU.Assertion
case_isStrictlySparseAxi4StreamPacket = do
  HU.assertBool "expected not sparse" $ (not . isStrictlySparseAxi4StreamPacket) []
  HU.assertBool "expected not sparse" $ (not . isStrictlySparseAxi4StreamPacket) [DataByte]
  HU.assertBool "expected not sparse"
    $ (not . isStrictlySparseAxi4StreamPacket) [PositionByte]
  HU.assertBool "expected not sparse"
    $ (not . isStrictlySparseAxi4StreamPacket) [DataByte, NullByte]
  HU.assertBool "expected not sparse"
    $ (not . isStrictlySparseAxi4StreamPacket) [PositionByte, NullByte]
  HU.assertBool "expected not sparse"
    $ (not . isStrictlySparseAxi4StreamPacket) [PositionByte, DataByte]
  HU.assertBool "expected not sparse"
    $ (not . isStrictlySparseAxi4StreamPacket) [PositionByte, DataByte, PositionByte]
  HU.assertBool "expected sparse"
    $ isStrictlySparseAxi4StreamPacket [DataByte, PositionByte, DataByte, PositionByte]
  HU.assertBool "expected sparse"
    $ isStrictlySparseAxi4StreamPacket [DataByte, PositionByte, DataByte]
  HU.assertBool "expected sparse"
    $ isStrictlySparseAxi4StreamPacket [DataByte, PositionByte, DataByte, NullByte]
  HU.assertBool "expected sparse"
    $ isStrictlySparseAxi4StreamPacket [PositionByte, DataByte, PositionByte, DataByte]

case_isUnalignedAxi4StreamPacket :: HU.Assertion
case_isUnalignedAxi4StreamPacket = do
  HU.assertBool "expected unaligned" $ isUnalignedAxi4StreamPacket [PositionByte]
  HU.assertBool "expected unaligned" $ isUnalignedAxi4StreamPacket [PositionByte, DataByte]
  HU.assertBool "expected unaligned" $ isUnalignedAxi4StreamPacket [DataByte, PositionByte]
  HU.assertBool "expected unaligned"
    $ isUnalignedAxi4StreamPacket [PositionByte, DataByte, PositionByte]
  HU.assertBool "expected unaligned"
    $ isUnalignedAxi4StreamPacket [PositionByte, NullByte, DataByte, PositionByte]
  HU.assertBool "expected unaligned"
    $ isUnalignedAxi4StreamPacket [NullByte, DataByte, PositionByte]
  HU.assertBool "expected unaligned"
    $ isUnalignedAxi4StreamPacket [DataByte, PositionByte, NullByte]
  HU.assertBool "expected not unaligned" $ not $ isUnalignedAxi4StreamPacket []
  HU.assertBool "expected not unaligned" $ not $ isUnalignedAxi4StreamPacket [DataByte]
  HU.assertBool "expected not unaligned"
    $ not
    $ isUnalignedAxi4StreamPacket [DataByte, PositionByte, DataByte]
  HU.assertBool "expected not unaligned"
    $ not
    $ isUnalignedAxi4StreamPacket [PositionByte, DataByte, PositionByte, DataByte]
  HU.assertBool "expected not unaligned"
    $ not
    $ isUnalignedAxi4StreamPacket [DataByte, PositionByte, DataByte, PositionByte]

case_unInterruptedAxi4Packets :: HU.Assertion
case_unInterruptedAxi4Packets = do
  HU.assertBool "expected uninterrupted" $ unInterruptedAxi4Packets [Nothing, lastTransfer]
  HU.assertBool "expected uninterrupted"
    $ unInterruptedAxi4Packets [Nothing, lastTransfer, Nothing]
  HU.assertBool "expected uninterrupted"
    $ unInterruptedAxi4Packets [Nothing, payloadTransfer, lastTransfer]
  HU.assertBool "expected uninterrupted"
    $ unInterruptedAxi4Packets [payloadTransfer, payloadTransfer, lastTransfer]
  HU.assertBool "expected uninterrupted"
    $ unInterruptedAxi4Packets [lastTransfer, payloadTransfer, lastTransfer]
  HU.assertBool "expected not uninterrupted"
    $ unInterruptedAxi4Packets [lastTransfer, Nothing, payloadTransfer, lastTransfer]
  HU.assertBool "expected not uninterrupted"
    $ not
    $ unInterruptedAxi4Packets [Nothing, payloadTransfer, Nothing, lastTransfer]
 where
  payloadTransfer = Just $ mkDummyM2S (repeat True) False
  lastTransfer = Just $ mkDummyM2S (repeat True) True

case_isSinglePacket :: HU.Assertion
case_isSinglePacket = do
  HU.assertBool "expected single packet" $ isSinglePacket [lastTransfer]
  HU.assertBool "expected single packet" $ isSinglePacket [payloadTransfer, lastTransfer]
  HU.assertBool "expected not single packet" $ not $ isSinglePacket []
  HU.assertBool "expected not single packet" $ not $ isSinglePacket [payloadTransfer]
  HU.assertBool "expected not single packet"
    $ not
    $ isSinglePacket [lastTransfer, payloadTransfer]
  HU.assertBool "expected not single packet"
    $ not
    $ isSinglePacket [lastTransfer, payloadTransfer, lastTransfer]
 where
  payloadTransfer = mkDummyM2S (repeat True) False
  lastTransfer = mkDummyM2S (repeat True) True

case_hasLeadingNullBytes :: HU.Assertion
case_hasLeadingNullBytes = do
  HU.assertBool "expected leading null bytes" $ hasLeadingNullBytes [NullByte, DataByte]
  HU.assertBool "expected not leading null bytes" $ not $ hasLeadingNullBytes []
  HU.assertBool "expected not leading null bytes" $ not $ hasLeadingNullBytes [DataByte]
  HU.assertBool "expected not leading null bytes"
    $ not
    $ hasLeadingNullBytes [DataByte, NullByte]
  HU.assertBool "expected not leading null bytes" $ not $ hasLeadingNullBytes [NullByte]

case_hasTrailingNullBytes :: HU.Assertion
case_hasTrailingNullBytes = do
  HU.assertBool "expected trailing null bytes" $ hasTrailingNullBytes [DataByte, NullByte]
  HU.assertBool "expected not trailing null bytes" $ not $ hasTrailingNullBytes []
  HU.assertBool "expected not trailing null bytes" $ not $ hasTrailingNullBytes [DataByte]
  HU.assertBool "expected not trailing null bytes" $ not $ hasTrailingNullBytes [NullByte]
  HU.assertBool "expected not trailing null bytes"
    $ not
    $ hasTrailingNullBytes [NullByte, DataByte]

mkDummyM2S :: Vec 4 Bool -> Bool -> Axi4StreamM2S ('Axi4StreamConfig 4 0 0) ()
mkDummyM2S keep last0 =
  Axi4StreamM2S
    { _tdata = repeat 0
    , _tkeep = keep
    , _tstrb = repeat True
    , _tlast = last0
    , _tid = 0
    , _tdest = 0
    , _tuser = ()
    }
