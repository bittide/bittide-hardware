import           Clash.Prelude

import           Contranomy
import           Contranomy.Println
import qualified Data.ByteString    as BS
import qualified Data.List          as L
import           ReadElf
import           System.Environment (getArgs)

main :: IO ()
main = do
  elfFile <- L.head <$> getArgs
  elfBytes <- BS.readFile elfFile
  let elf = parseElf elfBytes
  let (entry, iMem, dMem) = readElf elf

  -- Hook up to println-debugging at special address 0x90000000
  hookPrint 0x90000000 $ sample $ fmap snd $
    contranomy' hasClock hasReset entry iMem dMem $ pure (False, False, 0b0)
