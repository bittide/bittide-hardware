-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PatternGuards #-}

module ContranomySim.ReadElf (readElf, readElfFromMemory, Address, BinaryData) where

import           Clash.Prelude

import qualified Data.ByteString as BS
import Data.Elf
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
    = (addData (elfSegmentPhysAddr seg) (elfSegmentData seg `BS.append` BS.pack [0,0]) is, ds)

    | otherwise
    =
      -- Data gets mapped at 0x5000_0000 and higher, but gets loaded into the data memory
      -- from 0x0000_0000, so the base address needs to be subtracted.
      (is, addData (elfSegmentPhysAddr seg - 0x5000_0000) (elfSegmentData seg) ds)

  addData (fromIntegral -> startAddr) str mem =
    let bytes = pack <$> BS.unpack str
     in I.fromList (L.zip [startAddr..] bytes) <> mem
