-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}

-- | Tests for the JTAG debug interface
module Tests.Jtag where

import Control.Applicative ((<|>))
import Control.Monad.Extra (ifM, when)
import Data.List.Extra (trim)
import Data.Maybe (fromJust)
import Data.Proxy
import GHC.Stack (CallStack, callStack, prettyCallStack)
import System.Directory (findExecutable)
import System.Exit
import System.IO
import System.Process
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options
import Prelude

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

getGdb :: (HasCallStack) => IO String
getGdb = do
  gdbMultiArch <- findExecutable "gdb-multiarch"
  gdb <- findExecutable "gdb"
  case gdbMultiArch <|> gdb of
    Nothing -> fail "Neither gdb-multiarch nor gdb found in PATH"
    Just x -> pure x

expectLineOrTimeout ::
  (HasCallStack) =>
  -- | Number of microseconds to wait. I.e., 1_000_000 is 1 second.
  Int ->
  -- | Debug mode. Print output to stderr.
  Bool ->
  -- | Handle to read from.
  Handle ->
  -- | Expected line. Skips empty lines.
  String ->
  Assertion
expectLineOrTimeout us debug h expected = do
  result <- timeout us go
  case result of
    Just () -> pure ()
    Nothing -> expectError callStack expected
 where
  go = do
    line <- hGetLine h

    when debug $ do
      hPutStr stderr "> "
      hPutStrLn stderr line

    ifM
      (pure $ null line)
      go
      (assertEqual (prettyCallStack callStack) expected line)

waitForLineOrTimeout ::
  (HasCallStack) =>
  -- | Number of microseconds to wait. I.e., 1_000_000 is 1 second.
  Int ->
  -- | Debug mode. Print output to stderr.
  Bool ->
  -- | Handle to read from.
  Handle ->
  -- | Expected line. Skips any lines that do not match.
  String ->
  Assertion
waitForLineOrTimeout us debug h expected = do
  result <- timeout us go
  case result of
    Just () -> pure ()
    Nothing -> expectError callStack expected
 where
  go = do
    line <- hGetLine h
    when debug $ do
      hPutStr stderr "> "
      hPutStrLn stderr line
    if line == expected
      then pure ()
      else go

expectError :: CallStack -> String -> Assertion
expectError cs expected = do
  assertFailure ("Timed out waiting for " <> expected <> "\n\n" <> prettyCallStack cs)

{- | Run three processes in parallel:

1. The VexRiscv simulation. It opens a TCP socket for OpenOCD to connect to.
2. OpenOCD. It connects to the VexRiscv simulation and exposes a GDB server.
3. GDB. It connects to the OpenOCD GDB server and bunch of commands. See the
   file produced by 'getGdbCmdPath' for the commands.
-}
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
    -- Timeout after 60 seconds
    expectLine = expectLineOrTimeout 60_000_000
    waitForLine = waitForLineOrTimeout 60_000_000

    vexRiscvProc =
      (proc simulateExecPath [printElfPath])
        { std_out = CreatePipe
        , cwd = Just projectRoot
        }

    openOcdProc =
      (proc "openocd-riscv" ["-f", openocdCfgPath])
        { std_err = CreatePipe
        , cwd = Just projectRoot
        }

    gdbProc =
      (proc gdb ["--command", gdbCmdPath])
        { std_out = CreatePipe -- Comment this line to see GDB output
        , cwd = Just projectRoot
        }

  withCreateProcess vexRiscvProc $ \_ (fromJust -> vexRiscvStdOut) _ _ -> do
    expectLine debug vexRiscvStdOut "JTAG bridge ready at port 7894"

    hSetBuffering vexRiscvStdOut LineBuffering
    putStrLn "Expecting \"[CPU] a\" on vexRiscvStdOut"
    expectLine debug vexRiscvStdOut "[CPU] a"

    -- CPU has started, so we can start OpenOCD
    withCreateProcess openOcdProc $ \_ _ (fromJust -> openOcdStdErr) _ -> do
      hSetBuffering openOcdStdErr LineBuffering
      putStrLn "Waiting for \"Halting processor\" on openOcdStdErr"
      waitForLine debug openOcdStdErr "Halting processor"

      -- OpenOCD has started, so we can start GDB
      withCreateProcess gdbProc $ \_ _ _ gdbProcHandle -> do
        putStrLn "Expecting \"[CPU] a\" on vexRiscvStdOut"
        expectLine debug vexRiscvStdOut "[CPU] a"
        putStrLn "Expecting \"[CPU] b\" on vexRiscvStdOut"
        expectLine debug vexRiscvStdOut "[CPU] b"

        putStrLn "Waiting for gdb to exit"
        gdbExitCode <- waitForProcess gdbProcHandle
        ExitSuccess @?= gdbExitCode

tests :: TestTree
tests = askOption $ \(JtagDebug debug) ->
  testGroup
    "JTAG"
    [ testCase "Basic GDB commands, breakpoints, and program loading" (test debug)
    ]

main :: IO ()
main =
  defaultMainWithIngredients
    (includingOptions [Option (Proxy :: Proxy JtagDebug)] : defaultIngredients)
    tests
