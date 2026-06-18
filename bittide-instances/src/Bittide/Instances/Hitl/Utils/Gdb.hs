-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Utils.Gdb where

import Prelude

import Clash.Prelude (Signed, Unsigned, Vec, checkedFromIntegral, toList)

import Bittide.Hitl (DeviceInfo (..))
import Bittide.Instances.Hitl.Setup (LinkCount)
import Bittide.Instances.Hitl.Utils.Driver (getTargetIndex)
import Bittide.Instances.Hitl.Utils.MemoryMap (getPathAddress)
import Bittide.Instances.Hitl.Utils.OpenOcd (TapInfo (..))
import Bittide.Wishbone (TimeCmd (Capture))
import Control.Monad (forM_)
import GHC.Stack (HasCallStack)
import Gdb (Gdb)
import Project.FilePath
import Project.Handle (expectRight)
import Protocols.MemoryMap (MemoryMap)
import System.FilePath ((</>))
import Vivado.Tcl (HwTarget)

import qualified Gdb

initGdb :: FilePath -> String -> Gdb -> TapInfo -> (HwTarget, DeviceInfo) -> IO ()
initGdb hitlDir binName gdb tapInfo (hwT, _d) = do
  Gdb.setLogging gdb $
    hitlDir
      </> "gdb-" <> binName <> "-" <> show (getTargetIndex hwT) <> ".log"
  Gdb.setFile gdb $ firmwareBinariesDir "riscv32imc" Release </> binName
  Gdb.setTarget gdb tapInfo.gdbPort
  Gdb.setTimeout gdb Nothing
  Gdb.runCommand gdb "echo connected to target device"
  pure ()

-- * GDB-based component HAL accessors

--
-- Read/write a component's registers over GDB, addressed through a 'MemoryMap'. Generic
-- across demos (the memory map is a parameter). All require the CPU to be halted.

-- | Read the current local-counter value from a device's @Timer@ peripheral.
readCurrentTime :: (HasCallStack) => MemoryMap -> (HwTarget, DeviceInfo) -> Gdb -> IO (Unsigned 64)
readCurrentTime mm (_, d) gdb = do
  putStrLn $ "Getting current time from device " <> d.deviceId
  commandAddress <- expectRight $ getPathAddress mm ["0", "Timer", "command"]
  scratchAddress <- expectRight $ getPathAddress mm ["0", "Timer", "scratchpad"]
  Gdb.writeLe gdb commandAddress Capture
  Gdb.readLe gdb scratchAddress

{- | Write a node's reset-release cycle to its @TimedReset@ peripheral. Once the local
counter exceeds this value the node's application (and its application counter) leave
reset.
-}
writeReleaseCycle :: (HasCallStack) => MemoryMap -> Gdb -> Unsigned 64 -> IO ()
writeReleaseCycle mm gdb releaseCycle = do
  addr <- expectRight $ getPathAddress mm ["0", "TimedReset", "release_cycle"]
  Gdb.writeLe gdb addr releaseCycle

{- | Write the per-link frame corrections and set the @valid@ flag on the
@UgnCorrections@ peripheral. Resuming the CPU makes the management unit apply the
corrections to its elastic buffers.
-}
writeCorrections :: (HasCallStack) => MemoryMap -> Gdb -> Vec LinkCount (Signed 64) -> IO ()
writeCorrections mm gdb corrections = do
  correctionsAddr <- expectRight $ getPathAddress mm ["0", "UgnCorrections", "corrections"]
  validAddr <- expectRight $ getPathAddress mm ["0", "UgnCorrections", "valid"]
  Gdb.writeLe gdb correctionsAddr corrections
  Gdb.writeLe gdb validAddr True

{- | Read the per-link hardware UGN counter captures from the @CaptureUgns@ peripheral:
the @(localCounter, remoteCounter)@ pair per link, with @localCounter@ already adjusted
for the elastic-buffer frames added/removed since capture (@elastic_buffer_delta@).
-}
readHardwareUgns ::
  (HasCallStack) => MemoryMap -> (HwTarget, DeviceInfo) -> Gdb -> IO [(Unsigned 64, Unsigned 64)]
readHardwareUgns mm (_, d) gdb = do
  let
    getUgnRegister reg = expectRight $ getPathAddress mm ["0", "CaptureUgns", reg]

    -- Adjust the local counter for the frames added/removed from the elastic
    -- buffer after capturing the UGN. Leaves the remote counter untouched.
    adjustLocalCounter :: (Unsigned 64, Unsigned 64, Signed 32) -> (Unsigned 64, Unsigned 64)
    adjustLocalCounter (localCounter, remoteCounter, delta) =
      (addSigned localCounter delta, remoteCounter)
     where
      addSigned :: Unsigned 64 -> Signed 32 -> Unsigned 64
      addSigned u s = checkedFromIntegral (toInteger u + toInteger s)

  putStrLn $ "Getting UGNs for device " <> d.deviceId
  localCounters <- Gdb.readLe @(Vec LinkCount (Unsigned 64)) gdb =<< getUgnRegister "local_counter"
  remoteCounters <- Gdb.readLe @(Vec LinkCount (Unsigned 64)) gdb =<< getUgnRegister "remote_counter"
  deltas <- Gdb.readLe @(Vec LinkCount (Signed 32)) gdb =<< getUgnRegister "elastic_buffer_delta"
  let ugnTriples = zip3 (toList localCounters) (toList remoteCounters) (toList deltas)
  forM_ ugnTriples $ \triple -> putStrLn $ "Raw UGN triple: " <> show triple
  pure $ adjustLocalCounter <$> ugnTriples

{- | Read the relabeled application counter exposed by the user core's
@AppCounterReadback@ peripheral. With the application running, @localCounter -
app_counter@ equals the node's reset-release cycle, letting the host verify the relabel
landed on hardware.
-}
readAppCounter :: (HasCallStack) => MemoryMap -> (HwTarget, DeviceInfo) -> Gdb -> IO (Unsigned 64)
readAppCounter mm (_, _) gdb = do
  addr <- expectRight $ getPathAddress mm ["0", "AppCounterReadback", "app_counter"]
  Gdb.readLe gdb addr
