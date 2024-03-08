-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Tests for the JTAG debug interface
module Tests.Jtag where

import Prelude

import Control.Monad.Extra (ifM)
import Data.List.Extra (trim)
import Data.Maybe (fromJust)
import System.Exit
import System.IO
import System.Process

import Test.Tasty
import Test.Tasty.HUnit

import Paths_clash_vexriscv_sim (getDataFileName)

cabalListBin :: String -> IO FilePath
cabalListBin name = do
  trim <$> readProcess "cabal" ["-v0", "list-bin", name] ""

getProjectRoot :: IO FilePath
getProjectRoot = trim <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""

getSimulateExecPath :: IO FilePath
getSimulateExecPath = cabalListBin "clash-vexriscv-sim:clash-vexriscv-bin"

getPrintElfPath :: IO FilePath
getPrintElfPath = pure "target/riscv32imc-unknown-none-elf/debug/print_a"

getOpenOcdCfgPath :: IO FilePath
getOpenOcdCfgPath = getDataFileName "data/vexriscv_sim.cfg"

getGdbCmdPath :: IO FilePath
getGdbCmdPath = getDataFileName "data/vexriscv_gdb.cfg"

expectLine :: Handle -> String -> Assertion
expectLine h expected = do
  line <- hGetLine h
  ifM
    (pure $ null line)
    (expectLine h expected)
    (expected @?= line)

waitForLine :: Handle -> String -> IO ()
waitForLine h expected = do
  line <- hGetLine h
  if line == expected
    then pure ()
    else waitForLine h expected

-- | Run three processes in parallel:
--
-- 1. The VexRiscv simulation. It opens a TCP socket for OpenOCD to connect to.
-- 2. OpenOCD. It connects to the VexRiscv simulation and exposes a GDB server.
-- 3. GDB. It connects to the OpenOCD GDB server and bunch of commands. See the
--    file produced by 'getGdbCmdPath' for the commands.
--
test :: Assertion
test = do
  simulateExecPath <- getSimulateExecPath
  printElfPath <- getPrintElfPath
  projectRoot <- getProjectRoot
  openocdCfgPath <- getOpenOcdCfgPath
  gdbCmdPath <- getGdbCmdPath

  let
    vexRiscvProc = (proc simulateExecPath [printElfPath]){
        std_out = CreatePipe
      , cwd = Just projectRoot
    }

    openOcdProc = (proc "openocd-vexriscv" ["-f", openocdCfgPath]){
        std_err = CreatePipe
      , cwd = Just projectRoot
    }

    gdbProc = (proc "gdb" ["--command", gdbCmdPath]){
      std_out = CreatePipe, -- Comment this line to see GDB output
      cwd = Just projectRoot
    }

  withCreateProcess vexRiscvProc $ \_ (fromJust -> vexRiscvStdOut) _ _ -> do
    hSetBuffering vexRiscvStdOut LineBuffering
    expectLine vexRiscvStdOut "[CPU] a"

    -- CPU has started, so we can start OpenOCD
    withCreateProcess openOcdProc $ \_ _ (fromJust -> openOcdStdErr) _ -> do
      hSetBuffering openOcdStdErr LineBuffering
      waitForLine openOcdStdErr "Halting processor"

      -- OpenOCD has started, so we can start GDB
      withCreateProcess gdbProc $ \_ _ _ gdbProcHandle -> do
        expectLine vexRiscvStdOut "[CPU] a"
        expectLine vexRiscvStdOut "[CPU] b"

        gdbExitCode <- waitForProcess gdbProcHandle
        ExitSuccess @?= gdbExitCode

tests :: TestTree
tests = testGroup "JTAG"
  [ testCase "Basic GDB commands, breakpoints, and program loading" test
  ]

main :: IO ()
main = defaultMain tests
