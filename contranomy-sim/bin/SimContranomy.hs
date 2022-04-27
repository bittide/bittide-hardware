{-|
Copyright  :  (C) 2022, Google LLC
License    :  Apache-2.0
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

import           Clash.Prelude

import           Contranomy
import           ContranomySim.Print
import           ContranomySim.ReadElf

import qualified Data.ByteString       as BS
import qualified Data.List             as L
import           System.Environment    (getArgs)


main :: IO ()
main = do
  elfFile <- L.head <$> getArgs
  elfBytes <- BS.readFile elfFile
  let (entry, iMem, dMem) = readElfFromMemory elfBytes

  -- Hook up to print-debugging at special address 0x90000000
  hookPrint 0x90000000 $ sample $ fmap snd $
    contranomy' hasClock hasReset entry iMem dMem $ pure (False, False, 0b0)
