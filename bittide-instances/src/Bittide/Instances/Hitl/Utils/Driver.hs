-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

module Bittide.Instances.Hitl.Utils.Driver where

import Prelude

import Bittide.Hitl
import Bittide.Instances.Hitl.Setup (demoRigInfo)
import Bittide.Instances.Hitl.Utils.Vivado
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i, __i)
import GHC.Stack (HasCallStack)
import Project.FilePath (CargoBuildType (..))
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (hGetContents)
import System.Process (
  CreateProcess (..),
  StdStream (CreatePipe),
  createProcess,
  shell,
  waitForProcess,
 )
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM

import qualified Data.List as L

getTargetIndex :: (HasCallStack) => HwTarget -> Int
getTargetIndex hwT =
  fromMaybe
    (error ("Could not find " <> show hwT))
    (L.findIndex (\di -> di.deviceId == idFromHwT hwT) demoRigInfo)

-- | Asserts the value '"0"' on the given probe for the given hardware target.
deassertProbe :: String -> (HwTarget, DeviceInfo) -> VivadoM ()
deassertProbe probe (hwT, d) = do
  liftIO $ putStrLn $ "Deasserting probe " <> probe <> " on " <> d.deviceId
  openHardwareTarget hwT
  updateVio "vioHitlt" [(probe, "0")]

-- | Asserts the value '"1"' on the given probe for the given hardware target.
assertProbe :: String -> (HwTarget, DeviceInfo) -> VivadoM ()
assertProbe probe (hwT, d) = do
  liftIO $ putStrLn $ "Asserting probe " <> probe <> " on " <> d.deviceId
  openHardwareTarget hwT
  updateVio "vioHitlt" [(probe, "1")]

{- | Get the handshake status for the list of hardware targets/devices given.

Assumes that each target has a probe named 'probe_handshakes_done', which should be tied
to an 'and' reduction of the handshakes signals from the transceivers.
-}
getHandshakesStatus :: [(HwTarget, DeviceInfo)] -> [Bool] -> VivadoM [Bool]
getHandshakesStatus [] _ = return []
getHandshakesStatus _ [] = return []
getHandshakesStatus ((hwT, d) : hwtdRest) (handsShaken : hsRest) = do
  let getRest = getHandshakesStatus hwtdRest hsRest
  if handsShaken
    then do
      rest <- getRest
      return $ handsShaken : rest
    else do
      openHardwareTarget hwT
      vals <- readVio "vioHitlt" ["probe_handshakes_done"]
      let
        getResult =
          case vals of
            [("probe_handshakes_done", "1")] -> do
              liftIO $
                putStrLn $
                  "!!!!! Handshake completed on device "
                    <> d.deviceId
                    <> " ("
                    <> show (getTargetIndex hwT)
                    <> ") !!!!!"
              return True
            _ -> return False
      result <- getResult
      rest <- getRest
      return $ result : rest

-- | Stalls until 'getHandshakesStatus' returns a list of only 'True's.
awaitHandshakes :: [(HwTarget, DeviceInfo)] -> VivadoM ()
awaitHandshakes targets = do
  let
    innerInit = L.repeat False
    inner prev = do
      new <- getHandshakesStatus targets prev
      if and new
        then return ()
        else inner new
  inner innerInit

buildRustTarget :: FilePath -> FilePath -> CargoBuildType -> IO ()
buildRustTarget baseDir binName build = do
  let
    buildArg = case build of
      Debug -> ""
      Release -> "--release"

  currentEnv <- getEnvironment

  (_rustStdin, rustStdoutH, rustStderrH, rustProc) <-
    createProcess $
      (shell [i|cd #{baseDir </> "firmware-binaries"}; cargo build #{buildArg} --bin="#{binName}"|])
        { env = Just currentEnv
        , std_in = CreatePipe
        , std_err = CreatePipe
        , std_out = CreatePipe
        }

  rustProcExit <- waitForProcess rustProc

  unless (rustProcExit == ExitSuccess) $ do
    rustStdout <- case rustStdoutH of
      Just h -> hGetContents h
      _ -> return ""
    rustStderr <- case rustStderrH of
      Just h -> hGetContents h
      _ -> return ""
    let indentString = concatMap (\c -> if c == '\n' then "\n  " else [c])
    error
      [__i|
        Failed to build Rust binary!
        exit code: #{rustProcExit}
        stdout:
        #{indentString rustStdout}
        stderr:
        #{indentString rustStderr}
      |]
