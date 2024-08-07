-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Prelude

import Control.Exception
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.FilePath.Glob (glob)

import Bittide.Instances.Hitl.Post.PostProcess
import Bittide.Instances.Hitl.Post.BoardTestExtended


main :: IO ()
main = do
  args <- getArgs
  case args of
    ilaDir : [testExitCode]  -> do
      csvPaths <- glob (ilaDir </> "*" </> "*" </> "*.csv")
      let ilaCsvPaths = toFlattenedIlaCsvPathList ilaDir csvPaths
      let exitCode = read testExitCode
      postBoardTestExtended exitCode ilaCsvPaths

    [] -> throwIO (userError "Expected 2 arguments (ILA data dir and HITL test exit code), got none")
    _  -> throwIO (userError $ "Expected 2 arguments (ILA data dir and HITL test exit code), got: " <> unwords args)
  pure ()
