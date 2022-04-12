module ReadElf ( Address, BinaryData, parseElf, readElf ) where

import           Clash.Prelude

import qualified Data.ByteString    as BS
import           Data.Elf
import qualified Data.IntMap.Strict as I
import qualified Data.List          as L

type BinaryData = I.IntMap (BitVector 8)
type Address = BitVector 32

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
      (is, addData (elfSegmentPhysAddr seg) (elfSegmentData seg) ds)

  addData (fromIntegral -> startAddr) str mem =
    let bytes = pack <$> BS.unpack str
     in I.fromList (L.zip [startAddr..] bytes) <> mem
