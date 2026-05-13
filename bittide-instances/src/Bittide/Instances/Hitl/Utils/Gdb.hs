-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Utils.Gdb where

import Prelude

import Bittide.Hitl (DeviceInfo (..))
import Bittide.Instances.Hitl.Utils.Driver (getTargetIndex)
import Bittide.Instances.Hitl.Utils.OpenOcd (TapInfo (..))
import Gdb (Gdb)
import Project.FilePath
import System.FilePath ((</>))
import Vivado.Tcl (HwTarget)

import qualified Gdb

initGdb :: FilePath -> String -> Gdb -> TapInfo -> (HwTarget, DeviceInfo) -> IO ()
initGdb hitlDir binName gdb tapInfo (hwT, _d) = do
  Gdb.setLogging gdb $
    hitlDir
      </> "gdb-" <> binName <> "-" <> show (getTargetIndex hwT) <> ".log"
  Gdb.setFile gdb $ firmwareBinariesDir "riscv32imc" Release </> binName
  Gdb.setTarget gdb tapInfo.gdbPort
  Gdb.setTimeout gdb Nothing
  Gdb.runCommand gdb "echo connected to target device"
  pure ()
