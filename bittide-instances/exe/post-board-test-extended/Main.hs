-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Prelude

import Control.Exception
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.FilePath.Glob (glob)

import Bittide.Instances.Hitl.Post.BoardTestExtended
import Bittide.Instances.Hitl.Post.PostProcess

main :: IO ()
main = do
  args <- getArgs
  case args of
    ilaDataDir : [testExitCode] -> do
      csvPaths <- glob (ilaDataDir </> "*" </> "*" </> "*.csv")
      let ilaCsvPaths = toFlattenedIlaCsvPathList ilaDataDir csvPaths
      let exitCode = read testExitCode
      postBoardTestExtended exitCode ilaCsvPaths
    [] ->
      throwIO
        (userError "Expected 2 arguments (ILA data dir and HITL test exit code), got none")
    _ ->
      throwIO
        ( userError $
            "Expected 2 arguments (ILA data dir and HITL test exit code), got: " <> unwords args
        )
  pure ()
