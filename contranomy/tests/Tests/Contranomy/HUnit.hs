module Tests.Contranomy.HUnit ( elfExpect ) where

import Clash.Prelude

import qualified Data.ByteString as BS
import Test.HUnit.Base ((@?=), Assertion)
import System.IO.Temp (withSystemTempFile)

import ReadElf
import Contranomy
import Contranomy.Println

-- | Load an elf binary, inspect the debug output
elfExpect :: (FilePath -> IO ()) -- ^ Action to place the @.elf@ file in the given 'FilePath'
          -> BS.ByteString -- ^ First bytes of expected output
          -> Assertion
elfExpect act expected = do
  withSystemTempFile "ELF" $ \fp _ -> do
    act fp
    elfBytes <- BS.readFile fp
    let elf = parseElf elfBytes
    let (entry, iMem, dMem) = readElf elf

    -- TODO Use 'elfEntry' as an optional(?) argument to the core to start
    -- execution from a particular PC value.

    -- Hook up to println-debugging at special address 0x90000000
    let res = getDataBytes (BS.length expected) 0x90000000 $ sample $ fmap snd $
              contranomy' hasClock hasReset entry iMem dMem $ pure (False, False, 0b0)

    res @?= expected
