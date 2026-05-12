-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- TODO: Rename module
module Bittide.Instances.Hitl.Utils.Utils where

import Clash.Prelude

import Bittide.ClockControl.Config (CcConf, saveCcConfig)
import Bittide.ClockControl.Topology (Topology)
import Bittide.Instances.Hitl.Utils.MemoryMap (getPathAddress)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)
import Gdb (Gdb)
import Project.Handle (expectRight)
import Protocols.MemoryMap (MemoryMap)

import qualified Gdb

dumpCcSamples :: (HasCallStack) => MemoryMap -> FilePath -> CcConf Topology -> [Gdb] -> IO ()
dumpCcSamples mm hitlDir ccConf ccGdbs = do
  mapConcurrently_ Gdb.interrupt ccGdbs
  sampleMemoryAddr <- expectRight $ getPathAddress @Integer mm ["0", "SampleMemory", "data"]

  nSamples <- liftIO $ zipWithConcurrently (go sampleMemoryAddr) ccGdbs ccSamplesPaths
  putStrLn [i|Dumped /n/ clock control samples: #{nSamples}|]
  saveCcConfig hitlDir ccConf
  putStrLn [i|Wrote configs and samples to: #{hitlDir}|]
 where
  go :: (HasCallStack) => Integer -> Gdb -> FilePath -> IO Word
  go addr gdb dumpPath = do
    nSamplesWritten <- Gdb.readLe @(Unsigned 32) gdb addr

    let
      bytesPerSample = 13
      bytesPerWord = 4

      dumpStart = addr + bytesPerWord
      dumpEnd = dumpStart + fromIntegral nSamplesWritten * bytesPerWord * bytesPerSample

    Gdb.dumpMemoryRegion gdb dumpPath dumpStart dumpEnd >> pure (numConvert nSamplesWritten)
  ccSamplesPaths = [[i|#{hitlDir}/cc-samples-#{n}.bin|] | n <- [(0 :: Int) .. 7]]
