-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Utils.Program where

import Prelude

import System.IO
import System.Process
import System.Timeout (timeout)

data ProcessHandles = ProcessHandles
  { stdinHandle :: Handle
  , stdoutHandle :: Handle
  , stderrHandle :: Handle
  , process :: ProcessHandle
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
