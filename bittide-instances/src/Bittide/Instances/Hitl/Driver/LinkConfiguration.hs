-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

module Bittide.Instances.Hitl.Driver.LinkConfiguration (driver) where

import Clash.Prelude

import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently)
import Control.Concurrent.Extra (Lock, newLock, withLock)
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.String.Interpolate (i)
import Gdb (Gdb)
import Numeric (showHex)
import Project.Chan
import Project.Handle (expectRight)
import Protocols.MemoryMap (MemoryMap)
import System.Exit
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM

import Bittide.Hitl
import Bittide.Instances.Hitl.Setup (FpgaCount, LinkCount, fpgaSetup)
import Bittide.Instances.Hitl.Utils.BootPe (withBootPes)
import Bittide.Instances.Hitl.Utils.Driver (assertProbe)
import Bittide.Instances.Hitl.Utils.MemoryMap (getPathAddress)

import qualified Data.List as L
import qualified Gdb
import qualified System.Timeout.Extra as T

{- | Wait for all targets to report stable links, then check that every link
received the index of the neighbor defined in 'fpgaSetup'.
-}
driver ::
  -- | Memory map of the CPU, see 'Bittide.Instances.Hitl.LinkConfiguration.memoryMap'
  MemoryMap ->
  String ->
  [(HwTarget, DeviceInfo)] ->
  VivadoM ExitCode
driver mm _name targets = do
  -- The FPGA index is only valid once the test has started, see 'hitlVio'
  forM_ targets (assertProbe "probe_test_start")

  withBootPes "link-configuration-test" targets $ \gdbs serials -> liftIO $ do
    T.tryWithTimeout T.PrintActionTime "Waiting for stable links" 60_000_000
      $ forConcurrently_ serials
      $ \serial ->
        waitForLine serial "Links stable"

    mapConcurrently_ Gdb.interrupt gdbs
    printLock <- newLock
    checks <- zipWithConcurrently (checkTarget mm printLock) targets gdbs

    pure $ if L.and checks then ExitSuccess else ExitFailure 1

{- | Check that the data received on each link of the given target matches the
index of the neighbor as defined in 'fpgaSetup'. Requires the CPU to be halted.
-}
checkTarget :: MemoryMap -> Lock -> (HwTarget, DeviceInfo) -> Gdb -> IO Bool
checkTarget mm printLock (_, d) gdb = do
  let getRegister reg = expectRight $ getPathAddress mm ["0", "LinkConfigurationCheck", reg]
  myIndex <- Gdb.readLe @(Index FpgaCount) gdb =<< getRegister "my_index"
  received <-
    Gdb.readLe @(Vec LinkCount (Maybe (BitVector 64))) gdb =<< getRegister "received_indices"

  let
    expected :: Vec LinkCount (Maybe (BitVector 64))
    expected = Just . resize . pack <$> snd (fpgaSetup !! myIndex)

    showLink link e r =
      [i|  Link #{link}: expected #{showMaybeHex e}, received #{showMaybeHex r}|]
        <> if e == r then "" else " (MISMATCH)"

    showMaybeHex = maybe "nothing" (\x -> "0x" <> showHex x "")

  let deviceId = d.deviceId
  withLock printLock $ do
    putStrLn [i|Received indices for target #{deviceId} (FPGA index #{myIndex}):|]
    mapM_ putStrLn $ toList $ izipWith showLink expected received

  pure $ expected == received
