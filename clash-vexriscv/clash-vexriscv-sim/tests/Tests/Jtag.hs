-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Tests for the JTAG debug interface
module Tests.Jtag where

import Prelude

import Control.Applicative ((<|>))
import Control.Monad.Extra (ifM, when)
import Data.List.Extra (trim)
import Data.Maybe (fromJust)
import Data.Proxy
import System.Directory (findExecutable)
import System.Exit
import System.IO
import System.Process

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options

import Paths_clash_vexriscv_sim (getDataFileName)

newtype JtagDebug = JtagDebug Bool

instance IsOption JtagDebug where
    defaultValue = JtagDebug False
    parseValue = fmap JtagDebug . safeReadBool
    optionName = return "jtag-debug"
    optionHelp = return "While waiting for outputs of subprocesses, print them to stderr"
    optionCLParser = flagCLParser Nothing (JtagDebug True)

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

getGdb :: HasCallStack => IO String
getGdb = do
  gdbMultiArch <- findExecutable "gdb-multiarch"
  gdb <- findExecutable "gdb"
  case gdbMultiArch <|> gdb of
    Nothing -> fail "Neither gdb-multiarch nor gdb found in PATH"
    Just x -> pure x

expectLine :: Bool -> Handle -> String -> Assertion
expectLine debug h expected = do
  line <- hGetLine h
  when debug $ do
    hPutStr stderr "> "
    hPutStrLn stderr line
  ifM
    (pure $ null line)
    (expectLine debug h expected)
    (expected @?= line)

waitForLine :: Bool -> Handle -> String -> IO ()
waitForLine debug h expected = do
  line <- hGetLine h
  when debug $ do
    hPutStr stderr "> "
    hPutStrLn stderr line
  if line == expected
    then pure ()
    else waitForLine debug h expected

-- | Run three processes in parallel:
--
-- 1. The VexRiscv simulation. It opens a TCP socket for OpenOCD to connect to.
-- 2. OpenOCD. It connects to the VexRiscv simulation and exposes a GDB server.
-- 3. GDB. It connects to the OpenOCD GDB server and bunch of commands. See the
--    file produced by 'getGdbCmdPath' for the commands.
--
test ::
  -- | Print debug output of subprocesses
  Bool ->
  Assertion
test debug = do
  simulateExecPath <- getSimulateExecPath
  printElfPath <- getPrintElfPath
  projectRoot <- getProjectRoot
  openocdCfgPath <- getOpenOcdCfgPath
  gdbCmdPath <- getGdbCmdPath
  gdb <- getGdb

  let
    vexRiscvProc = (proc simulateExecPath [printElfPath]){
        std_out = CreatePipe
      , cwd = Just projectRoot
    }

    openOcdProc = (proc "openocd-vexriscv" ["-f", openocdCfgPath]){
        std_err = CreatePipe
      , cwd = Just projectRoot
    }

    gdbProc = (proc gdb ["--command", gdbCmdPath]){
      std_out = CreatePipe, -- Comment this line to see GDB output
      cwd = Just projectRoot
    }

  withCreateProcess vexRiscvProc $ \_ (fromJust -> vexRiscvStdOut) _ _ -> do
    hSetBuffering vexRiscvStdOut LineBuffering
    expectLine debug vexRiscvStdOut "[CPU] a"

    -- CPU has started, so we can start OpenOCD
    withCreateProcess openOcdProc $ \_ _ (fromJust -> openOcdStdErr) _ -> do
      hSetBuffering openOcdStdErr LineBuffering
      waitForLine debug openOcdStdErr "Halting processor"

      -- OpenOCD has started, so we can start GDB
      withCreateProcess gdbProc $ \_ _ _ gdbProcHandle -> do
        expectLine debug vexRiscvStdOut "[CPU] a"
        expectLine debug vexRiscvStdOut "[CPU] b"

        gdbExitCode <- waitForProcess gdbProcHandle
        ExitSuccess @?= gdbExitCode

tests :: TestTree
tests = askOption $ \(JtagDebug debug) ->
  testGroup "JTAG"
    [ testCase "Basic GDB commands, breakpoints, and program loading" (test debug)
    ]

main :: IO ()
main =
  defaultMainWithIngredients
    (includingOptions [Option (Proxy :: Proxy JtagDebug)] : defaultIngredients)
    tests
