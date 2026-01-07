-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

{-

This file is an example of how the API in clash-protocols-memmap
can be used.

Here, a circuit defined in the Internal.HdlTest.UartMock is being used
as an example.

The code then prints out the calculated memory map, processes the
memory map and then prints out the JSON representation of it.

-}

import Clash.Prelude

import Internal.HdlTest.UartMock (someOtherCircuit)

import Protocols.MemoryMap
import Protocols.MemoryMap.Check.AbsAddress (runMakeAbsolute)

import qualified Data.ByteString.Lazy as BS
import Protocols.MemoryMap.Json (LocationStorage (LocationSeparate))
import qualified Protocols.MemoryMap.Json as Json

main :: IO ()
main = do
  let SimOnly memoryMap =
        getConstBwdAny (withClockResetEnable @System clockGen resetGen enableGen someOtherCircuit)
  print memoryMap
  let tree0 = convert memoryMap.tree
  let tree1 = normalizeRelTree tree0
  print tree1
  let (absTree, _errs) = runMakeAbsolute memoryMap.deviceDefs (0x0000_0000, 0xFFFF_FFFF) tree1
  print absTree

  putStrLn "\n\n\n"

  let json = Json.memoryMapJson LocationSeparate memoryMap.deviceDefs absTree
  BS.putStr (Json.encode json)
  pure ()
