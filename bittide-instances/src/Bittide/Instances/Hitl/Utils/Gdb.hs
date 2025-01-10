-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Utils.Gdb where

import Clash.Prelude

import Control.Monad (forM_)
import System.IO

runGdbCommands :: Handle -> [String] -> IO ()
runGdbCommands h commands =
  forM_ commands $ \command -> do
    putStrLn $ "gdb-in: " <> command
    hPutStrLn h command

gdbEcho :: String -> String
gdbEcho s = "echo \\n" <> s <> "\\n"
