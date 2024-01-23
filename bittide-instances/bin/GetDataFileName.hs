-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Prelude

import Control.Exception
import Control.Monad (unless)
import Control.Monad.Extra (unlessM)
import Paths_bittide_instances
import System.Console.GetOpt
import System.Directory
import System.Environment

data Flag = NoCheck deriving Eq

options :: [OptDescr Flag]
options = [ Option [] ["no-check"] (NoArg NoCheck) "Do not check if the file exists" ]

main :: IO ()
main = do
  args <- getArgs
  let (flags, files, _) = getOpt Permute options args
  case files of
    [file] -> do
      fPath <- getDataFileName file
      unless (NoCheck `elem` flags)
        $ unlessM (doesFileExist fPath)
          $ throwIO $ userError $ unwords
            ["File", show file, "does not exist in bittide-instances."]
      putStrLn fPath
    [] -> throwIO (userError "Expected one file argument, got none")
    _  -> throwIO (userError $ "Expected one file argument, got: " <> unwords files)
  pure ()
