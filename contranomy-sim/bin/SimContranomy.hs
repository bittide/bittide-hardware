-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import           Clash.Prelude

import           Contranomy
import           ContranomySim.Print
import           ContranomySim.ReadElf

import qualified Data.ByteString       as BS
import qualified Data.IntMap           as I
import qualified Data.List             as L


main :: IO ()
main = do
  elfBytes <- BS.readFile "main.elf"
  let (entry, iMem, dMem) = readElfFromMemory elfBytes

  -- add device tree as a memory mapped component in 0x1000_0000

  -- TODO read the device tree file from command line args?
  deviceTree <- BS.readFile "../devicetree/blobs/contranomy-sim.dtb"
  -- add padding to prevent uninitialised accesses
  let padding = L.replicate (4 - (BS.length deviceTree `mod` 4)) 0
  let dMem' = dMem
               `I.union` I.fromList (L.zip [0x10000000..] $ pack <$> BS.unpack deviceTree)
               `I.union` I.fromList (L.zip [(0x10000000 + fromIntegral (BS.length deviceTree))..] padding)

  -- Hook up to print-debugging at special address 0x70000000
  hookPrint 0x70000000 $ sample $ fmap snd $
    contranomy' hasClock hasReset entry iMem dMem' $ pure (False, False, 0b0)
