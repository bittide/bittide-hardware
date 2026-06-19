-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}

-- | Monad for common Vivado commands
module Vivado.VivadoM (
  VivadoM,
  ObjectRef,
  askVivado,
  openHardwareTarget,
  setProbesFile,
  refreshHwDevice,
  commitVios,
  setProp,
  getProp,
  setVioValue,
  getVioValue,
  updateVio,
  readVio,
) where

import Control.Monad (forM, forM_)
import Control.Monad.Reader

import Vivado.Internal
import Vivado.Tcl

type ObjectRef = String

type VivadoM a = ReaderT VivadoHandle IO a

-- | Obtain the 'VivadoHandle' of the current 'VivadoM' session.
askVivado :: VivadoM VivadoHandle
askVivado = ask

openHardwareTarget :: HwTarget -> VivadoM ()
openHardwareTarget hwT = do
  v <- ask
  liftIO $ openHwTarget v hwT
  refreshHwDevice

{- | Associate a debug probes file (@.ltx@) with the current hardware device and
refresh it. Required before reading/writing any VIO or ILA probes on a freshly
opened device.
-}
setProbesFile :: FilePath -> VivadoM ()
setProbesFile probesFilePath = do
  setProp "[current_hw_device]" "PROBES.FILE" (embrace probesFilePath)
  refreshHwDevice

refreshHwDevice :: VivadoM ()
refreshHwDevice = do
  v <- ask
  liftIO $ refresh_hw_device v []

commitVios :: VivadoM ()
commitVios = do
  v <- ask
  liftIO $ commit_hw_vio v ["[get_hw_vios]"]

setProp :: ObjectRef -> String -> String -> VivadoM ()
setProp objRef name val = do
  v <- ask
  liftIO $ execCmd_ v "set_property" [name, val, objRef]

getProp :: ObjectRef -> String -> VivadoM String
getProp objRef name = do
  v <- ask
  liftIO $ execCmd v "get_property" [name, objRef]

vioProbeRef :: String -> String
vioProbeRef name = "[get_hw_probes -of_objects [get_hw_vios] " <> name <> "]"

setVioValue :: String -> String -> String -> VivadoM ()
setVioValue vioName name val = do
  let vioRef = vioProbeRef $ "*" <> vioName <> "/" <> name
  setProp vioRef "OUTPUT_VALUE" val

getVioValue :: String -> String -> VivadoM String
getVioValue vioName name = do
  let vioRef = vioProbeRef $ "*" <> vioName <> "/" <> name
  getProp vioRef "INPUT_VALUE"

updateVio :: String -> [(String, String)] -> VivadoM ()
updateVio vioName values = do
  forM_ values $ \(name, value) -> do
    setVioValue vioName name value

  commitVios

{- | Read values from a VIO, the values will be returned
in an assoc-list in the same order as the probe name list.
-}
readVio ::
  -- | Name of the VIO
  String ->
  -- | Names of the values in the VIO to read
  [String] ->
  VivadoM [(String, String)]
readVio vioName names = do
  refreshHwDevice
  forM names $ \name -> do
    val <- getVioValue vioName name
    pure (name, val)
