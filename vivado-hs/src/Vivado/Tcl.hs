-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

{- | Haskell abstractions over Vivado Hardware Manager Tcl objects and commands.
See section "Description of Hardware Manager Tcl Objects and Commands" of the
"Vivado Design Suite User Guide Programming and Debugging" (UG908) for more
information.
-}
module Vivado.Tcl where

import Control.Monad (unless, void, when)
import Data.Maybe (listToMaybe)
import Vivado

-- | Executes a TCL command with an optional list of arguments.
execCmd :: VivadoHandle -> String -> [String] -> IO String
execCmd v cmd args = execPrint v $ unwords $ cmd : args

-- | Executes a TCL command with an optional list of arguments.
execCmd_ :: VivadoHandle -> String -> [String] -> IO ()
execCmd_ v cmd = void . execCmd v cmd

{- | Attempts to interpret a Tcl expression as a list and return it as a Haskell
list. May very well fail, even with valid Tcl lists.
-}
tclToList :: String -> [String]
tclToList = go []
 where
  go :: [String] -> String -> [String]
  go acc [] = acc
  go acc (' ' : xs) = go acc xs
  go acc ('\n' : xs) = go acc xs
  go acc list@('"' : xs) = do
    let (word, list') = span (/= '"') xs
    unless (listToMaybe list' == Just '"') $
      error $
        "No closing '\"' found in " <> show list
    go (acc <> [word]) (tail list')
  go acc list@('{' : xs) = do
    let (word, list') = span (/= '}') xs
    unless (listToMaybe list' == Just '}') $
      error $
        "No closing brace '}' found in " <> show list
    when ('{' `elem` list) $
      error "Nested Tcl braces ('{', '}') are not supported by this function."
    go (acc <> [word]) (tail list')
  go acc xs = go (acc <> [head $ words xs]) (unwords $ tail $ words xs)

embrace :: String -> String
embrace s = '{' : s <> "}"

-- | Produces a Tcl expression for a list from a given Haskell list.
listToTcl :: [String] -> String
listToTcl l = "[list " <> unwords (toWord <$> l) <> "]"
 where
  toWord s = if ' ' `elem` s then embrace s else s

-- * Hardware Manager Tcl objects and commands

-- ** hw_server Tcl commands

-- | hw_server Tcl object
newtype HwServer = HwServer {fromHwServer :: String}
  deriving (Eq)

instance Show HwServer where
  show = fromHwServer

-- | Open a connection to a hardware server. Wrapper for the equally named Vivado Hardware Server Tcl command.
connect_hw_server ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO HwServer
connect_hw_server v = fmap HwServer . execCmd v "connect_hw_server"

-- | Get or set the current hardware server.
current_hw_server ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO HwServer
current_hw_server v = fmap HwServer . execCmd v "current_hw_server"

-- | Close a connection to a hardware server.
disconnect_hw_server ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO ()
disconnect_hw_server v = execCmd_ v "disconnect_hw_server"

-- | Get list of hardware server names for the hardware servers.
get_hw_servers ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO [HwServer]
get_hw_servers v args = do
  hwServers <- execCmd v "get_hw_servers" args
  return $ HwServer <$> tclToList hwServers

-- | Refresh a connection to a hardware server.
refresh_hw_server ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO ()
refresh_hw_server v = execCmd_ v "refresh_hw_server"

-- ** hw_target Tcl commands

-- | hw_target Tcl object
newtype HwTarget = HwTarget {fromHwTarget :: String}
  deriving (Eq, Ord)

instance Show HwTarget where
  show = fromHwTarget

-- | Close a hardware target.
close_hw_target ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO ()
close_hw_target v = execCmd_ v "close_hw_target"

-- | Get or set the current hardware target.
current_hw_target ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO HwTarget
current_hw_target v = fmap HwTarget . execCmd v "current_hw_target"

-- | Get list of hardware targets for the hardware servers.
get_hw_targets ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO [HwTarget]
get_hw_targets v args = do
  hwTargets <- execCmd v "get_hw_targets" args
  return $ HwTarget <$> tclToList hwTargets

-- | Open a connection to a hardware target on the hardware server.
open_hw_target ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO ()
open_hw_target v = execCmd_ v "open_hw_target"

-- | Refresh a connection to a hardware server.
refresh_hw_target ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO ()
refresh_hw_target v = execCmd_ v "refresh_hw_target"

-- ** hw_device Tcl commands

-- | hw_device Tcl object
newtype HwDevice = HwDevice {fromHwDevice :: String}
  deriving (Eq)

instance Show HwDevice where
  show = fromHwDevice

-- | Get or set the current hardware device.
current_hw_device ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO HwDevice
current_hw_device v = fmap HwDevice . execCmd v "current_hw_device"

-- | Get list of hardware devices for the target.
get_hw_devices ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO [HwDevice]
get_hw_devices v args = do
  hwDevices <- execCmd v "get_hw_devices" args
  return $ HwDevice <$> tclToList hwDevices

-- | Program AMD FPGA devices.
program_hw_devices ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO [HwDevice]
program_hw_devices v args = do
  hwDevices <- execCmd v "program_hw_devices" args
  return $ HwDevice <$> tclToList hwDevices

-- | Refresh a hardware device.
refresh_hw_device ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO ()
refresh_hw_device v = execCmd_ v "refresh_hw_device"

-- ** hw_ila Tcl commands

-- | hw_ila Tcl object
newtype HwIla = HwIla {fromHwIla :: String}
  deriving (Eq)

instance Show HwIla where
  show = fromHwIla

-- | Get or set the current hardware ILA.
current_hw_ila ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO HwIla
current_hw_ila v = fmap HwIla . execCmd v "current_hw_ila"

-- | Get list of hardware ILAs for the target.
get_hw_ilas ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO [HwIla]
get_hw_ilas v args = do
  hwIlas <- execCmd v "get_hw_ilas" args
  return $ HwIla <$> tclToList hwIlas

-- | Reset hw_ila control properties to default values.
reset_hw_ila ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO ()
reset_hw_ila v = execCmd_ v "reset_hw_ila"

-- | Arm hw_ila triggers.
run_hw_ila ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO ()
run_hw_ila v = execCmd_ v "run_hw_ila"

-- | Wait until all data has been captured.
wait_on_hw_ila ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO ()
wait_on_hw_ila v = execCmd_ v "wait_on_hw_ila"

-- ** hw_probe Tcl commands

-- | hw_probe Tcl object
newtype HwProbe = HwProbe {fromHwProbe :: String}
  deriving (Eq)

instance Show HwProbe where
  show = fromHwProbe

-- | Creates a new hardware probe from physical ILA probe ports and/or constant values.
create_hw_probe ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO HwProbe
create_hw_probe v = fmap HwProbe . execCmd v "create_hw_probe"

-- | Deletes a user-defined hardware probe creating using the create_hw_probe command.
delete_hw_probe ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO ()
delete_hw_probe v = execCmd_ v "delete_hw_probe"

-- | Get list of hardware probes.
get_hw_probes ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO [HwProbe]
get_hw_probes v args = do
  hwProbes <- execCmd v "get_hw_probes" args
  return $ HwProbe <$> tclToList hwProbes

-- ** hw_vio Tcl commands

-- | hw_vio Tcl object
newtype HwVio = HwVio {fromHwVio :: String}
  deriving (Eq)

instance Show HwVio where
  show = fromHwVio

-- | Write hw_probe OUTPUT_VALUE properties values to VIO cores.
commit_hw_vio ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO ()
commit_hw_vio v = execCmd_ v "commit_hw_vio"

-- | Get a list of hw_vios.
get_hw_vios ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO [HwVio]
get_hw_vios v args = do
  hwVios <- execCmd v "get_hw_vios" args
  return $ HwVio <$> tclToList hwVios

-- | Update hw_probe INPUT_VALUE and ACTIVITY_VALUE properties with values read from VIO cores.
refresh_hw_vio ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO ()
refresh_hw_vio v = execCmd_ v "refresh_hw_vio"

-- | Reset VIO ACTIVITY_VALUE properties, for hw_probes associated with specified hw_vio objects.
reset_hw_vio_activity ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO ()
reset_hw_vio_activity v = execCmd_ v "reset_hw_vio_activity"

-- | Reset VIO core outputs to initial values.
reset_hw_vio_outputs ::
  VivadoHandle ->
  -- | Arguments
  [String] ->
  IO ()
reset_hw_vio_outputs v = execCmd_ v "refresh_hw_vio"
