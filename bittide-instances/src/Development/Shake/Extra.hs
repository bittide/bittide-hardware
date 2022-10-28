-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Development.Shake.Extra where

import Prelude

import Control.Monad (filterM)
import Data.Maybe (fromJust)
import Development.Shake
import Development.Shake.FilePath
import System.FilePath.Glob (glob)
import System.Posix (isRegularFile, getFileStatus)

import qualified Data.Aeson as Aeson

-- | Calculate SHA256 of all files in a directory. Ignores directories and
-- file metadata.
needDirectory :: FilePath -> Action ()
needDirectory dir = do
  paths <- liftIO (glob (dir </> "**" </> "*"))
  files <- liftIO (filterM (fmap isRegularFile . getFileStatus) paths)
  need files

suppressOutput :: [CmdOption]
suppressOutput = [EchoStdout False, EchoStderr True]

decodeFile :: Aeson.FromJSON a => FilePath -> Action a
decodeFile path = do
  need [path]
  fromJust <$> liftIO (Aeson.decodeFileStrict path)
