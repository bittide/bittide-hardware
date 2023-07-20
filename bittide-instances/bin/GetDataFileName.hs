-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Prelude

import Control.Exception
import Paths_bittide_instances
import System.Directory
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      fPath <- getDataFileName file
      fileExists <- doesFileExist fPath
      if fileExists
      then putStrLn fPath
      else throwIO (userError $ unwords
             ["File", show file, "does not exist in bittide-instances."])
    [] -> throwIO (userError "Expected one file argument, got none")
    _  -> throwIO (userError $ "Expected one file argument, got: " <> unwords args)
  pure ()
