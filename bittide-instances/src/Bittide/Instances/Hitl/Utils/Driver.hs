-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}

module Bittide.Instances.Hitl.Utils.Driver where

import Prelude

import Bittide.Hitl
import Bittide.Instances.Hitl.Setup (demoRigInfo)
import Bittide.Instances.Hitl.Utils.Program
import Bittide.Instances.Hitl.Utils.Vivado
import Control.Monad.IO.Class
import Data.Maybe (fromJust, fromMaybe)
import GHC.Stack (HasCallStack)
import Project.Handle
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM
import "extra" Data.List.Extra (trim)

import qualified Bittide.Instances.Hitl.Utils.Gdb as Gdb
import qualified Control.Monad.Trans.Control as CMTC
import qualified Data.List as L
import qualified System.Timeout.Lifted as STL

getTargetIndex :: (HasCallStack) => HwTarget -> Int
getTargetIndex hwT =
  fromMaybe
    (error ("Could not find " <> show hwT))
    (L.findIndex (\di -> di.deviceId == idFromHwT hwT) demoRigInfo)

{- | A generic wrapper around 'System.Timeout.Lifted.timeout' that 'fail's if the
timeout is hit.
-}
tryWithTimeout ::
  forall m a.
  (CMTC.MonadBaseControl IO m, MonadFail m) =>
  String ->
  Int ->
  m a ->
  m a
tryWithTimeout actionName dur action = do
  result <- STL.timeout dur action
  case result of
    Nothing -> do
      fail $ "Timeout while performing action: " <> actionName
    Just r -> pure r

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

readSingleGdbValue :: ProcessStdIoHandles -> String -> String -> IO String
readSingleGdbValue gdb value cmd = do
  let
    startString = "START OF READ (" <> value <> ")"
    endString = "END OF READ (" <> value <> ")"
  Gdb.runCommands
    gdb.stdinHandle
    [ "printf \"" <> startString <> "\\n\""
    , cmd
    , "printf \"" <> endString <> "\\n\""
    ]
  _ <-
    tryWithTimeout ("GDB read prepare: " <> value) 15_000_000 $
      readUntil gdb.stdoutHandle startString
  untrimmed <-
    tryWithTimeout ("GDB read: " <> value) 15_000_000 $
      readUntil gdb.stdoutHandle endString
  let
    trimmed = trim untrimmed
    gdbLines = L.lines trimmed
    outputLine = fromJust $ L.find ("(gdb)" `L.isPrefixOf`) gdbLines
    untilColon = L.dropWhile (/= ':') outputLine
    lineFinal = trim $ L.drop 2 untilColon
  return lineFinal
