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
  putStrLn "Hello :)"
