{-# LANGUAGE ViewPatterns #-}

import Clash.Prelude

import Contranomy
import Contranomy.Println
import qualified Data.ByteString as BS
import ReadElf
import qualified Data.IntMap.Strict as I
import qualified Data.List as L
import System.Environment (getArgs)

main :: IO ()
main = do
  elfFile <- L.head <$> getArgs
  elfBytes <- BS.readFile elfFile
  let elf = parseElf elfBytes
  let (entry, iMem, dMem) = readElf elf

  -- TODO Use 'elfEntry' as an optional(?) argument to the core to start
  -- execution from a particular PC value.

  -- Hook up to println-debugging at special address 0x90000000
  hookPrint 0x90000000 $ sample $ fmap snd $
    contranomy' hasClock hasReset entry iMem dMem $ pure (False, False, 0b0)
