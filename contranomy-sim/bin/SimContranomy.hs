-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Clash.Prelude

import Contranomy
import ContranomySim.Print
import ContranomySim.ReadElf

import qualified Data.ByteString as BS


main :: IO ()
main = do
  elfBytes <- BS.readFile "main.elf"
  let (entry, iMem, dMem) = readElfFromMemory elfBytes

  -- Hook up to print-debugging at special address 0x90000000
  hookPrint 0x90000000 $ sample $ fmap snd $
    contranomy' hasClock hasReset entry iMem dMem $ pure (False, False, 0b0)
