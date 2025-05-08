-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bittide.Instances.Hitl.Utils.Program where

import Prelude

import Paths_bittide_instances

import Clash.Prelude (BitVector, Unsigned, Vec, bitCoerce, toList)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromJust)
import Debug.Trace (traceId)
import Project.FilePath
import Project.Handle (Error (..), Filter (..))
import System.Directory (canonicalizePath)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO
import System.Process
import System.Timeout (timeout)

brackets :: (MonadMask m) => [m a] -> (a -> m b) -> ([a] -> m c) -> m c
brackets acqs rel act = go [] acqs
 where
  go resL [] = act (reverse resL)
  go resL (acq : acqs1) = bracket acq rel $ \res -> go (res : resL) acqs1

getStartScriptPath :: IO FilePath
getStartScriptPath = getDataFileName "data/start.sh"

getTcpSprayPath :: IO FilePath
getTcpSprayPath = getDataFileName "data/tcpspray/start.sh"

data ProcessStdIoHandles = ProcessStdIoHandles
  { stdinHandle :: Handle
  , stdoutHandle :: Handle
  , stderrHandle :: Handle
  }

awaitProcessTermination :: String -> ProcessHandle -> Maybe Int -> IO ()
awaitProcessTermination name h Nothing = do
  putStrLn $ "Waiting for process " <> name <> " to terminate"
  _ <- waitForProcess h
  return ()
awaitProcessTermination name h (Just t) = do
  putStrLn $ "Waiting for process " <> name <> " to terminate"
  result <- timeout t $ waitForProcess h
  case result of
    Just _ -> return ()
    Nothing -> error "Waiting for pocess termination timed out."

startProgram ::
  -- | Program name
  String ->
  -- | Program args
  [String] ->
  -- | Environment variables
  [(String, String)] ->
  IO (ProcessStdIoHandles, ProcessHandle, IO ())
startProgram name args extraEnv = do
  startPath <- getStartScriptPath
  currentEnv <- getEnvironment

  let
    progProc =
      (proc startPath args)
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , env = Just (currentEnv <> [("EXEC_PATH", name)] <> extraEnv)
        , new_session = True
        }

  progHandles@(progStdin, progStdout, progStderr, progPh) <- createProcess progProc

  let
    progHandles' =
      ProcessStdIoHandles
        { stdinHandle = fromJust progStdin
        , stdoutHandle = fromJust progStdout
        , stderrHandle = fromJust progStderr
        }

  pure (progHandles', progPh, cleanupProcess progHandles)

type CpuMap = (BitVector 32, Int)

cpuMapToJson :: CpuMap -> String
cpuMapToJson (identifier, port) =
  "{\"whoami\":{\"String\":\""
    <> whoAmI
    <> "\"},\"gdb_port\":"
    <> show port
    <> "}"
 where
  bytes :: Vec 4 (BitVector 8)
  bytes = bitCoerce identifier
  bytesList :: [BitVector 8]
  bytesList = toList bytes
  beBytesList :: [BitVector 8]
  beBytesList = reverse bytesList
  whoAmI :: String
  whoAmI = toEnum . fromIntegral <$> beBytesList

data CpuMapJson
  = Literal String
  | FilePath FilePath
  | Build [CpuMap]

mapToArgs :: CpuMapJson -> [String]
mapToArgs (Literal string) = ["--json", string]
mapToArgs (FilePath path) = ["--json-path", path]
mapToArgs (Build maps) = ["--json='" <> mapsJson <> "'"]
 where
  mapsJson = "[" <> intercalate "," (cpuMapToJson <$> maps) <> "]"

data GdbAdaptersConfig = GdbAdaptersConfig
  { usbDev :: String
  , memMapAddress :: Unsigned 32
  , cpuMap :: CpuMapJson
  , stdoutPath :: Maybe FilePath
  , stderrPath :: Maybe FilePath
  }

startGdbAdapters ::
  GdbAdaptersConfig ->
  IO (ProcessStdIoHandles, ProcessHandle, IO ())
startGdbAdapters adaptersConfig = do
  root <- findParentContaining "cabal.project"
  let
    adapterArgs =
      [ "--usb-port"
      , adaptersConfig.usbDev
      , "--scan-address"
      , show adaptersConfig.memMapAddress
      ]
    jsonArgs =
      case adaptersConfig.cpuMap of
        Literal s -> do
          jsonFilePath <- jsonFile
          writeFile jsonFilePath s
          return ["--json-path", jsonFilePath]
        Build maps -> do
          let s = "[" <> intercalate "," (cpuMapToJson <$> maps) <> "]"
          jsonFilePath <- jsonFile
          writeFile jsonFilePath s
          return ["--json-path", jsonFilePath]
        FilePath s -> return ["--json-path", s]
     where
      jsonFile = do
        projectDir0 <- findParentContaining "cabal.project"
        projectDir1 <- canonicalizePath projectDir0
        let escapedUsbDev = fmap (\c -> if c == ':' then '!' else c) adaptersConfig.usbDev
        return $
          projectDir1 </> "_build" </> "hitl" </> ("cpu-map-" <> escapedUsbDev <> ".json")
    singleEnvVar varName varVal = [(varName, varVal)]
    out = maybe [] (singleEnvVar "STDOUT_LOG") adaptersConfig.stdoutPath
    err = maybe [] (singleEnvVar "STDERR_LOG") adaptersConfig.stderrPath
    logging = [("RUST_LOG", "TRACE")]
    adapterEnv = out <> err <> logging
    programName = root </> gdbAdaptersPath Release
  progJsonArgs <- jsonArgs
  startProgram programName (adapterArgs <> (traceId <$> progJsonArgs)) adapterEnv

withGdbAdapters ::
  (MonadIO m, MonadMask m) =>
  GdbAdaptersConfig ->
  (ProcessStdIoHandles -> m a) ->
  m a
withGdbAdapters config action = do
  (adapters, _, clean) <- liftIO $ startGdbAdapters config
  finally (action adapters) (liftIO clean)

-- | Wait until we see "All sessions halted", fail if we see an error.
adaptersWaitForHalt :: String -> Filter
adaptersWaitForHalt s
  | "Error:" `isPrefixOf` s = Stop (Error ("Found error in gdb-adapters output: " <> s))
  | "All sessions halted" `isPrefixOf` s = Stop Ok
  | otherwise = Continue

gdbWaitForLoad :: String -> Filter
gdbWaitForLoad s
  | "Remote communication error." `isPrefixOf` s =
      Stop (Error ("GDB remote communication error: " <> s))
  | "Start address 0x80000000" `isPrefixOf` s = Stop Ok
  | otherwise = Continue
