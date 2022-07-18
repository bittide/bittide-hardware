-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import           Clash.Prelude

import           Contranomy
import           ContranomySim.Print
import           ContranomySim.ReadElf
import           ContranomySim.MemoryMapConsts

import qualified Data.ByteString       as BS
import qualified Data.IntMap           as I
import qualified Data.List             as L


main :: IO ()
main = do
  elfBytes <- BS.readFile "main.elf"
  let (entry, iMem, dMem) = readElfFromMemory elfBytes

  -- add device tree as a memory mapped component

  -- TODO read the device tree file from command line args?
  deviceTreeRaw <- BS.readFile "../devicetree/blobs/contranomy-sim.dtb"
  -- add padding to prevent uninitialised accesses
  let padding = L.replicate (4 - (BS.length deviceTreeRaw `mod` 4)) 0
      deviceTree = fmap pack . BS.unpack $ deviceTreeRaw <> BS.pack padding

  let dMem' = dMem `I.union` I.fromAscList (L.zip [fdtAddr..] deviceTree)

  -- Hook up to print-debugging at uts designated address
  hookPrint characterDeviceAddr $ sample $ fmap snd $
    contranomy' hasClock hasReset entry iMem dMem' $ pure (False, False, 0b0)
