-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Utils.FilePath where

import Control.Exception (throwIO)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (isDrive, takeDirectory, (</>))
import Prelude

buildDir :: FilePath -> FilePath
buildDir baseDir = baseDir </> "target"

cabalProject :: String
cabalProject = "cabal.project"

data BuildType = Debug | Release deriving (Eq)

instance Show BuildType where
  show buildType = case buildType of
    Release -> "release"
    Debug -> "debug"

rustBinsDir :: FilePath -> String -> BuildType -> FilePath
rustBinsDir baseDir rsTargetArch buildType = buildDir baseDir </> rsTargetArch </> show buildType

findParentContaining :: String -> IO FilePath
findParentContaining filename = getCurrentDirectory >>= findParentContaining'
 where
  findParentContaining' dir
    | isDrive dir = throwIO $ userError $ "Could not find " <> filename
    | otherwise = do
        exists <- doesFileExist (dir </> filename)
        if exists
          then return dir
          else findParentContaining' $ takeDirectory dir
