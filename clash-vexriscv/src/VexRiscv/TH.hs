-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE MultiWayIf #-}

module VexRiscv.TH where

import Data.Char
import GHC.Stack
import Language.Haskell.TH
import System.Directory
import System.FilePath
import System.FilePath.Glob
import Prelude

packageName :: String
packageName =
  let
    locName =
      $( do
          loc <- location
          pure $ LitE $ StringL $ loc_package loc
       )

    dropLast [] = []
    dropLast [_] = []
    dropLast (x : xs) = x : dropLast xs
   in
    dropLast $ takeWhile (not . isNumber) locName

getPackageRelFilePath :: FilePath -> IO FilePath
getPackageRelFilePath path = do
  putStrLn $ "package name: " <> packageName
  packageManifestPath <- findCabalPackage packageName
  putStrLn $ "Package manifest path: " <> packageManifestPath
  let packagePath = takeDirectory packageManifestPath
  pure $ packagePath </> path

{- | Searches for a file called @package.cabal@, where @package@ is given as an
argument. It will look for it in the current directory. If it can't find it
there, it will traverse up until it finds the file or a file called
@cabal.project@. In case of the latter, it will traverse down recursively
until it encounters a @package.cabal@.

The returned path points to the @package.cabal@. Errors if it could not
find @package.cabal@ anywhere, or when it found multiple.
-}
findCabalPackage :: (HasCallStack) => String -> IO FilePath
findCabalPackage pkgName = goUp =<< canonicalizePath pkgName
 where
  goUp :: FilePath -> IO FilePath
  goUp path
    | isDrive path = error ("Could not find '" <> packageFilename <> "'")
    | otherwise = do
        packageExists <- doesFileExist (path </> packageFilename)
        projectExists <- doesFileExist (path </> projectFilename)

        if
          | packageExists -> pure (path </> packageFilename)
          | projectExists -> goDown path
          | otherwise -> goUp (takeDirectory path)

  goDown :: FilePath -> IO FilePath
  goDown path = do
    candidates <- glob (path </> "**" </> packageFilename)
    case candidates of
      [] -> error ("Could not find " <> packageFilename <> " in project " <> path)
      (_ : _ : _) -> error ("Ambiguous packages in project " <> path <> ": " <> show candidates)
      [c] -> pure c

  packageFilename = pkgName <> ".cabal"
  projectFilename = "cabal.project"
