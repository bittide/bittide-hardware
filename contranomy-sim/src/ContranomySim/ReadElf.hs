-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module ContranomySim.ReadElf (readElf, readElfFromMemory, Address, BinaryData) where

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
-- Loads the ELF file into memory by extracting section data.
--
-- TODO Check the ELF header is valid: is this RISCV? Is it RV32IMC?
-- TODO Binaries output now are SYS V ABI, are others compatible?
readElf :: Elf -> (Address, BinaryData, BinaryData)
readElf elf =
  let (iMem, dMem) = L.foldr go (mempty, mempty) (elfSections elf)
   in (fromIntegral (elfEntry elf), iMem, dMem)
 where
  go sec acc@(is, ds)
    -- Address is 0: Not mapped to virtual memory
    | elfSectionAddr sec == 0
    = acc

    -- Section contains instruction memory
    | SHF_EXECINSTR `elem` elfSectionFlags sec
    , SHF_WRITE `notElem` elfSectionFlags sec
    = (addData (elfSectionAddr sec) (elfSectionData sec `BS.append` (BS.pack [0,0])) is, ds)
    -- The line above pads the instruction memory with 2 bytes to enable ending on a compressed instruction.

    -- Section contains data memory
    | (SHF_WRITE `elem` elfSectionFlags sec
        || SHF_ALLOC `elem` elfSectionFlags sec)
    , SHF_EXECINSTR `notElem` elfSectionFlags sec
    = (is, addData (elfSectionAddr sec) (elfSectionData sec) ds)

    | otherwise
    = error ("Section is not executable XOR data:\n" <> show sec)

  addData (fromIntegral -> startAddr) str mem =
    let bytes = pack <$> BS.unpack str
     in I.fromList (L.zip [startAddr..] bytes) <> mem
