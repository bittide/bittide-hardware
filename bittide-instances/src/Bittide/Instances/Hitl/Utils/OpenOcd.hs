-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Utils.OpenOcd where

import Bittide.Instances.Hitl.Utils.Program
import Project.Handle
import Prelude

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.List (isInfixOf, isPrefixOf, sort, sortOn)
import Data.Maybe (fromJust)
import Numeric (readHex)
import Paths_bittide_instances
import System.Posix.Env (getEnvironment)
import System.Process
import Text.Read (readMaybe)

getStartPath :: IO FilePath
getStartPath = getDataFileName "data/openocd/start.sh"

-- | Wait until we see \"Initialization complete\", fail if we see an error.
waitForInitComplete :: String -> Filter
waitForInitComplete s
  | "Error:" `isPrefixOf` s = Stop (Error ("Found error in OpenOCD output: " <> s))
  | initCompleteMarker `isPrefixOf` s = Stop Ok
  | otherwise = Continue

initCompleteMarker :: String
initCompleteMarker = "Initialization complete"

withOpenOcd ::
  (MonadMask m, MonadIO m) =>
  -- | USB device location
  String ->
  -- | GDB port
  Int ->
  -- | TCL port
  Int ->
  -- | Telnet port
  Int ->
  -- | Action to run with OpenOCD
  (ProcessHandles -> m a) ->
  m a
withOpenOcd = withOpenOcdWithEnv []

{- | Run an action with an openocd process initialized according to the scripts
located in the data/openocd directory.
-}
withOpenOcdWithEnv ::
  (MonadMask m, MonadIO m) =>
  -- | Extra environment variables to pass to OpenOCD in form (name, value)
  [(String, String)] ->
  -- | USB device location
  String ->
  -- | GDB port
  Int ->
  -- | TCL port
  Int ->
  -- | Telnet port
  Int ->
  -- | Action to run with OpenOCD
  (ProcessHandles -> m a) ->
  m a
withOpenOcdWithEnv extraEnv usbLoc gdbPort tclPort telnetPort action = do
  (ocd, _handle, clean) <-
    liftIO $ startOpenOcdWithEnv extraEnv usbLoc gdbPort tclPort telnetPort
  finally (action ocd) (liftIO clean)

{- | Starts openocd with the given USB device location and ports.
Sets all handles to line buffering.
-}
startOpenOcdWithEnv ::
  -- | Extra environment variables to pass to OpenOCD in form (name, value)
  [(String, String)] ->
  -- | USB device location
  String ->
  -- | GDB port
  Int ->
  -- | TCL port
  Int ->
  -- | Telnet port
  Int ->
  IO (ProcessHandles, ProcessHandle, IO ())
startOpenOcdWithEnv extraEnv usbLoc gdbPort tclPort telnetPort =
  startOpenOcdWithEnvAndArgs
    ["-f", "ports.tcl", "-f", "sipeed.tcl", "-f", "ports.tcl", "-f", "vexriscv_init.tcl"]
    ( [ ("USB_DEVICE", usbLoc)
      , ("GDB_PORT", show gdbPort)
      , ("TCL_PORT", show tclPort)
      , ("TELNET_PORT", show telnetPort)
      ]
        <> extraEnv
    )

startOpenOcdWithEnvAndArgs ::
  [String] ->
  [(String, String)] ->
  IO (ProcessHandles, ProcessHandle, IO ())
startOpenOcdWithEnvAndArgs args extraEnv = do
  startPath <- getStartPath
  currentEnv <- getEnvironment
  let
    openOcdProc =
      (proc startPath args)
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , env = Just (currentEnv <> extraEnv)
        }

  ocdHandles@(openOcdStdin, openOcdStdout, openOcdStderr, openOcdPh) <-
    createProcess openOcdProc

  let
    ocdHandles' =
      ProcessHandles
        { stdinHandle = fromJust openOcdStdin
        , stdoutHandle = fromJust openOcdStdout
        , stderrHandle = fromJust openOcdStderr
        , process = openOcdPh
        }

  pure (ocdHandles', openOcdPh, cleanupProcess ocdHandles)

type JtagId = Int
type TapId = Int
type GdbPort = Int

-- | Given the output of OpenOCD, parse the tap ids and gdb ports.
parseJtagIdsAndGdbPorts :: [String] -> Either String [(JtagId, TapId, GdbPort)]
parseJtagIdsAndGdbPorts logLines = do
  gdbPorts <- parseGdbPorts logLines
  jtagIds <- parseJtagIds logLines
  mergeGdbJtag gdbPorts jtagIds

{- | Merge the outputs of parseGdbPorts and parseJtagIds. Returns a list of
@('JtagId', 'TapId', 'GdbPort')@ tuples. Errors if the tap numbers don't match
perfectly between the two inputs
-}
mergeGdbJtag :: [(Int, Int)] -> [(Int, Int)] -> Either String [(JtagId, TapId, GdbPort)]
mergeGdbJtag gdbPorts jtagIds =
  if gdbTaps /= jtagTaps
    then
      Left $
        "Tap numbers don't match. GDB taps: " ++ show gdbTaps ++ ", JTAG taps: " ++ show jtagTaps
    else
      Right $ sort $ zipWith mergePair sortedGdb sortedJtag
 where
  sortedGdb = sortOn fst gdbPorts
  sortedJtag = sortOn fst jtagIds
  gdbTaps = map fst sortedGdb
  jtagTaps = map fst sortedJtag

  mergePair :: (Int, Int) -> (Int, Int) -> (JtagId, TapId, GdbPort)
  mergePair (tapId, gdbPort) (_, jtagId) = do
    (fromIntegral jtagId, fromIntegral tapId, fromIntegral gdbPort)

{- | Parse OpenOCD log output to extract tap numbers and their associated GDB ports
Returns a list of (tap number, GDB port) tuples
-}
parseGdbPorts :: [String] -> Either String [(Int, Int)]
parseGdbPorts logLines = mapM parseLine relevantLines
 where
  relevantLines = filter isGdbServerLine logLines

  isGdbServerLine :: String -> Bool
  isGdbServerLine line = "starting gdb server on" `isInfixOf` line

  parseLine :: String -> Either String (Int, Int)
  parseLine line = do
    -- Extract tap number from "[riscv.tapN]"
    tapNum <- extractTapNumber line
    -- Extract port from "on NNNN"
    port <- extractPort line
    return (tapNum, port)

  extractTapNumber :: String -> Either String Int
  extractTapNumber line =
    case dropWhile (/= '[') line of
      ('[' : 'r' : 'i' : 's' : 'c' : 'v' : '.' : 't' : 'a' : 'p' : rest) ->
        case span (`elem` ['0' .. '9']) rest of
          (digits, _) | not (null digits) ->
            case readMaybe digits of
              Just n -> Right n
              Nothing -> Left $ "Failed to parse tap number: " ++ digits
          _ -> Left $ "No tap number found in: " ++ line
      _ -> Left $ "No tap prefix found in: " ++ line

  extractPort :: String -> Either String Int
  extractPort line =
    case words line of
      ws ->
        case dropWhile (/= "on") ws of
          ("on" : portStr : _) ->
            case readMaybe portStr of
              Just p -> Right p
              Nothing -> Left $ "Failed to parse port number: " ++ portStr
          _ -> Left $ "No port found in: " ++ line

{- | Parse OpenOCD log output to extract JTAG IDs for each tap
Returns a list of (tap number, JTAG ID) tuples
-}
parseJtagIds :: [String] -> Either String [(Int, Int)]
parseJtagIds logLines = mapM parseLine relevantLines
 where
  relevantLines = filter isJtagIdLine logLines

  isJtagIdLine :: String -> Bool
  isJtagIdLine line = "JTAG_ID:" `isPrefixOf` line

  parseLine :: String -> Either String (Int, Int)
  parseLine line = do
    -- Extract tap number from "riscv.tapN:"
    tapNum <- extractTapNumber line
    -- Extract JTAG ID (hex string like "0x2514C001") and parse to Int
    jtagId <- extractJtagId line
    return (tapNum, jtagId)

  extractTapNumber :: String -> Either String Int
  extractTapNumber line =
    case dropWhile (/= 'r') line of
      ('r' : 'i' : 's' : 'c' : 'v' : '.' : 't' : 'a' : 'p' : rest) ->
        case span (`elem` ['0' .. '9']) rest of
          (digits, _) | not (null digits) ->
            case readMaybe digits of
              Just n -> Right n
              Nothing -> Left $ "Failed to parse tap number: " ++ digits
          _ -> Left $ "No tap number found in: " ++ line
      _ -> Left $ "No tap prefix found in: " ++ line

  extractJtagId :: String -> Either String Int
  extractJtagId line =
    case words line of
      ws ->
        case dropWhile (not . ("0x" `isPrefixOf`)) ws of
          (idStr : _) -> parseHexInt idStr
          _ -> Left $ "No JTAG ID found in: " ++ line

  parseHexInt :: String -> Either String Int
  parseHexInt str =
    let stripped = if "0x" `isPrefixOf` str then drop 2 str else str
     in case readHex stripped of
          [(val, "")] -> Right val
          _ -> Left $ "Failed to parse hex value: " ++ str
