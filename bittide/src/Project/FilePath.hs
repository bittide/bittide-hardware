-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Project.FilePath where

import Prelude

import Control.Exception
import System.Directory
import System.FilePath

{- $setup
>>> import Clash.Prelude
-}

{- | Relative path to the build directory.

Example:

>>> buildDir
"_build"
-}
buildDir :: FilePath
buildDir = "_build"

{- | Relative path to the build directory.

Example:

>>> cargoDir
"_build/cargo"
-}
cargoDir :: FilePath
cargoDir = buildDir </> "cargo"

data CargoBuildType = Release | Debug
  deriving (Eq)

{- | Relative path to the firmware binaries directory.

Example:

>>> firmwareBinariesDir "riscv32imc-unknown-none-elf" Release
"_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf/release"
-}
firmwareBinariesDir :: String -> CargoBuildType -> FilePath
firmwareBinariesDir rustTargetArchitecture buildType =
  cargoDir
    </> "firmware-binaries"
    </> rustBinSubDir rustTargetArchitecture buildType

{- | Firmware binaries directory relative to cargo's target directory.

Example:

>>> rustBinSubDir "riscv32imc-unknown-none-elf" Release
"riscv32imc-unknown-none-elf/release"
-}
rustBinSubDir :: String -> CargoBuildType -> FilePath
rustBinSubDir rustTargetArchitecture buildType =
  rustTargetArchitecture
    </> case buildType of
      Release -> "release"
      Debug -> "debug"

-- | Recursive function that returns a parent directory containing a certain filename.
findParentContaining :: String -> IO FilePath
findParentContaining filename = goUp =<< getCurrentDirectory
 where
  goUp :: FilePath -> IO FilePath
  goUp path
    | isDrive path = throwIO $ userError $ "Could not find " <> filename
    | otherwise = do
        exists <- doesFileExist (path </> filename)
        if exists
          then return path
          else goUp (takeDirectory path)
