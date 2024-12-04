-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.JtagChain where

import Prelude

import Test.Tasty.HUnit (Assertion, testCase, (@=?))
import Tests.Jtag (cabalListBin, getGdb, JtagDebug (JtagDebug))
import Utils.FilePath (findParentContaining, cabalProject, rustBinsDir, BuildType (Debug))
import System.FilePath ((</>))
import System.Process
import Test.Tasty (TestTree, askOption, testGroup, defaultMainWithIngredients, includingOptions, defaultIngredients)
import Data.Data (Proxy (Proxy))
import Test.Tasty.Options (OptionDescription(Option))
import Control.Exception (ErrorCall (ErrorCallWithLocation))

import qualified Streaming.Prelude as SP
import System.IO (openFile, IOMode (ReadMode, WriteMode), hClose)
import GHC.Exception (throw, Exception (toException))
import Data.Maybe (fromJust)
import System.Directory (doesPathExist, doesDirectoryExist)
import System.Exit (ExitCode(ExitSuccess))
import Control.Monad (when)

getSimulateExecPath :: IO FilePath
getSimulateExecPath = cabalListBin "clash-vexriscv-sim:clash-vexriscv-chain-bin"

getProjectRoot :: IO FilePath
getProjectRoot = findParentContaining cabalProject

test ::
  -- | Print debug output of subprocesses
  Bool ->
  Assertion
test debug = do
  simulateExecPath <- getSimulateExecPath
  projectRoot <- getProjectRoot
  let
    rBD = rustBinsDir projectRoot "riscv32imc-unknown-none-elf" Debug
    printAElfPath = rBD </> "print_a"
    logAPath = projectRoot </> "cpu_a.log"
    printBElfPath = rBD </> "print_b"
    logBPath = projectRoot </> "cpu_b.log"
    simDataDir = projectRoot </> "clash-vexriscv-sim" </> "data"
    openocdCfgPath = simDataDir </> "vexriscv_chain_sim.cfg"
    gdbCmdPathA = simDataDir </> "vexriscv_chain_gdba.cfg"
    gdbCmdPathB = simDataDir </> "vexriscv_chain_gdbb.cfg"
  gdb <- getGdb

  ensureExists logAPath 46
  ensureExists logBPath 47

  let
    vexRiscvProc =
      ( proc
          simulateExecPath
          ["-a", printAElfPath, "-b", printBElfPath, "-A", logAPath, "-B", logBPath]
      )
      { std_out = CreatePipe
      , cwd = Just projectRoot
      }

    openOcdProc = (proc "openocd-vexriscv" ["-f", openocdCfgPath])
      { std_err = CreatePipe
      , cwd = Just projectRoot
      }

    gdbProcA = (proc gdb ["--command", gdbCmdPathA])
      { std_out = CreatePipe
      , cwd = Just projectRoot
      }

    gdbProcB = (proc gdb ["--command", gdbCmdPathB])
      { std_out = CreatePipe
      , cwd = Just projectRoot
      }

  withCreateProcess vexRiscvProc $ \_ _ _ _ -> do
    logAHandle <- openFile logAPath ReadMode
    logBHandle <- openFile logBPath ReadMode
    let
      logA0 = SP.fromHandle logAHandle
      logB0 = SP.fromHandle logBHandle

    logA1 <- expectLineFromStream debug logA0 "[CPU] a" 83
    logB1 <- expectLineFromStream debug logB0 "[CPU] b" 84

    withCreateProcess openOcdProc $ \_ _ (fromJust -> openOcdStdErr) _ -> do
      let openOcdStream = SP.fromHandle openOcdStdErr
      _ <- waitForLineInStream debug openOcdStream "Halting processor" 88

      withCreateProcess gdbProcA $ \_ _ _ gdbProcHandleA -> do
        withCreateProcess gdbProcB $ \_ _ _ gdbProcHandleB -> do
          _ <- expectLineFromStream debug logA1 "[CPU] a" 92
          _ <- expectLineFromStream debug logB1 "[CPU] b" 93
          _ <- expectLineFromStream debug logA1 "[CPU] b" 94
          _ <- expectLineFromStream debug logB1 "[CPU] a" 95

          gdbAExitCode <- waitForProcess gdbProcHandleA
          gdbBExitCode <- waitForProcess gdbProcHandleB
          ExitSuccess @=? gdbAExitCode
          ExitSuccess @=? gdbBExitCode

ensureExists :: FilePath -> Int -> IO ()
ensureExists file line = do
  pathExists <- doesPathExist file
  if not pathExists
    then touchFile
    else do
      pathIsDir <- doesDirectoryExist file
      if pathIsDir
        then throw . toException
          $ ErrorCallWithLocation
            ("log file path `" <> file <> "` points to existing directory")
            ("at line " <> show line)
        else touchFile
 where
  touchFile = do
    file' <- openFile file WriteMode
    hClose file'

expectLineFromStream
  :: Bool
  -> SP.Stream (SP.Of String) IO ()
  -> String
  -> Int
  -> IO (SP.Stream (SP.Of String) IO ())
expectLineFromStream debug stream lookFor line = do
  result <- SP.next stream
  case result of
    Right (out, next) -> do
      when debug $ putStrLn $ "DBG(E): " <> out
      if out == lookFor
        then return next
        else throw . toException $ errorHelper lookFor out line
    Left _ -> expectLineFromStream debug stream lookFor line

waitForLineInStream
  :: Bool
  -> SP.Stream (SP.Of String) IO ()
  -> String
  -> Int
  -> IO (SP.Stream (SP.Of String) IO ())
waitForLineInStream debug stream lookFor line = do
  result <- SP.next stream
  case result of
    Right (out, next) -> do
      when debug $ putStrLn $ "DBG(W): " <> out
      if out == lookFor
        then return next
        else waitForLineInStream debug next lookFor line
    Left _ -> expectLineFromStream debug stream lookFor line

errorHelper :: String -> String -> Int -> ErrorCall
errorHelper expected found loc =
  ErrorCallWithLocation
    ("expected `" <> expected <> "`, found `" <> found <> "`")
    ("at line " <> show loc)

tests :: TestTree
tests = askOption $ \(JtagDebug debug) ->
  testGroup
    "JTAG chaining"
    [ testCase "Basic GDB commands, breakpoints, and program loading" (test debug)
    ]

main :: IO ()
main =
  defaultMainWithIngredients
    (includingOptions [Option (Proxy :: Proxy JtagDebug)] : defaultIngredients)
    tests
