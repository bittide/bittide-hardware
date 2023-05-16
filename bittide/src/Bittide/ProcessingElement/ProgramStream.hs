-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ProcessingElement.ProgramStream (
    elfStreamStructure,
    elfStream,
    ZeroPaddingLength,
    IsExecutable,
    Segment,
    Address
  ) where

import Clash.Prelude

import Data.Elf

import qualified Data.ByteString as BS
import qualified Data.List       as L

type ZeroPaddingLength = BitVector 32
type IsExecutable = Bool
type Segment = (IsExecutable, Address, [BitVector 8], ZeroPaddingLength)

type Address = BitVector 32

elfStreamStructure :: BS.ByteString -> (Address, [Segment])
elfStreamStructure contents =
  let elf = parseElf contents
  in readElf elf

elfStream :: BS.ByteString -> [BitVector 8]
elfStream contents =
  let (addr, segs) = elfStreamStructure contents
  in
       bvToLE addr
    <> bvToLE (fromIntegral $ L.length segs)
    <> L.concatMap segmentStream segs

  where
    segmentStream :: Segment -> [BitVector 8]
    segmentStream (isExec, addr, dat, padding) =
          (if isExec then 1 else 0)
        : bvToLE addr
       <> bvToLE (fromIntegral $ L.length dat)
       <> bvToLE padding
       <> dat

    bvToLE :: BitVector 32 -> [BitVector 8]
    bvToLE (bitCoerce -> (a, b, c, d)) = [d, c, b, a]


readElf :: Elf -> (Address, [Segment])
readElf elf =
  let segs = L.foldr go [] (elfSegments elf)
   in (fromIntegral (elfEntry elf), segs)
 where
  go seg acc
    -- skip segments that don't need loading
    | elfSegmentType seg /= PT_LOAD
    = acc

    | PF_X `elem` elfSegmentFlags seg
    = let
        segData = elfSegmentData seg
        fileSz = fromIntegral $ BS.length segData
        memSz = fromIntegral $ elfSegmentMemSize seg
        zeroPadding = memSz - fileSz
        addr = fromIntegral $ elfSegmentPhysAddr seg
        segEntry = (True, addr, bytes segData, zeroPadding)
      in segEntry : acc

    | PF_R `elem` elfSegmentFlags seg
    = let
        segData = elfSegmentData seg
        fileSz = fromIntegral $ BS.length segData
        memSz = fromIntegral $ elfSegmentMemSize seg
        zeroPadding = memSz - fileSz
        addr = fromIntegral $ elfSegmentPhysAddr seg
        segEntry   = (False, addr, bytes segData, zeroPadding)
      in segEntry : acc

    | otherwise = acc

  bytes str = pack <$> BS.unpack str
