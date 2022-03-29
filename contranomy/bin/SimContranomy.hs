{-# LANGUAGE ViewPatterns #-}

import Clash.Prelude

import Contranomy
import Contranomy.Println
import qualified Data.ByteString as BS
import Data.Elf
import qualified Data.IntMap.Strict as I
import qualified Data.List as L

type BinaryData = I.IntMap (BitVector 8)
type Address = BitVector 32

-- | readElf :: elf file -> (initial PC, instructions, data)
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

main :: IO ()
main = do
  elfBytes <- BS.readFile "main.elf"
  let elf = parseElf elfBytes
  let (entry, iMem, dMem) = readElf elf

  -- TODO Use 'elfEntry' as an optional(?) argument to the core to start
  -- execution from a particular PC value.

  -- Hook up to println-debugging at special address 0x90000000
  hookPrint 0x90000000 $ sample $ fmap snd $
    contranomy' hasClock hasReset entry iMem dMem $ pure (False, False, 0b0)
