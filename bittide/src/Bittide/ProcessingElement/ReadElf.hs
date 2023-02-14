-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PatternGuards #-}

module Bittide.ProcessingElement.ReadElf (readElf, readElfFromMemory, Address, BinaryData) where

import           Clash.Prelude

import qualified Data.ByteString    as BS
import           Data.Elf
import qualified Data.IntMap.Strict as I
import qualified Data.List          as L

type BinaryData = I.IntMap (BitVector 8)
type Address = BitVector 32

readElfFromMemory :: BS.ByteString -> (Address, BinaryData, BinaryData)
readElfFromMemory contents =
  let elf = parseElf contents
  in readElf elf

-- | readElf :: elf file -> (initial PC, instructions, data)
--
-- TODO Check the ELF header is valid: is this RISCV? Is it RV32IMC?
-- TODO Binaries output now are SYS V ABI, are others compatible?
readElf :: Elf -> (Address, BinaryData, BinaryData)
readElf elf =
  let (iMem, dMem) = L.foldr go (mempty, mempty) (elfSegments elf)
   in (fromIntegral (elfEntry elf), iMem, dMem)
 where
  go seg acc@(is, ds)
    -- skip segments that don't need loading
    | elfSegmentType seg /= PT_LOAD
    = acc

    | PF_X `elem` elfSegmentFlags seg
    = (addData (elfSegmentPhysAddr seg) (bytes $ elfSegmentData seg `BS.append` BS.pack [0,0]) is, ds)

    | otherwise
    = let segData = elfSegmentData seg
          fileSz  = fromIntegral $ BS.length segData
          memSz   = fromIntegral $ elfSegmentMemSize seg
          data'   = bytes segData <> L.replicate (memSz - fileSz) 0
      in
      (is, addData (elfSegmentPhysAddr seg) data' ds)

  bytes str = pack <$> BS.unpack str

  addData (fromIntegral -> startAddr) dat mem =
     I.fromList (L.zip [startAddr..] dat) <> mem
