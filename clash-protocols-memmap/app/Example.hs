-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

import Clash.Prelude

import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BS
import Internal.HdlTest.UartMock (someOtherCircuit)
import Protocols.MemoryMap
import Protocols.MemoryMap.Check.AbsAddress (makeAbsolute)
import Protocols.MemoryMap.Json (memoryMapJson)

main :: IO ()
main = do
  putStrLn "hello"
  let SimOnly memoryMap =
        getConstBAny (withClockResetEnable @System clockGen resetGen enableGen someOtherCircuit)
  print memoryMap
  let tree0 = convert memoryMap.tree
  let tree1 = normaliseRelTree tree0
  print tree1
  -- let abs = absAddresses tree'
  let (absTree, _errs) = makeAbsolute memoryMap.deviceDefs (0x0000_0000, 0xFFFF_FFFF) tree1
  print absTree

  putStrLn "\n\n\n"

  let json = memoryMapJson memoryMap.deviceDefs absTree
  BS.putStr (Ae.encode json)
  pure ()
