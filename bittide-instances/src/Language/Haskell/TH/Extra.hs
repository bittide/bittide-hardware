-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Language.Haskell.TH.Extra where

import Prelude

import Data.List (intercalate)
import Data.List.Split (splitOn)

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

{- $setup
>>> import Language.Haskell.TH.Syntax (PkgName(..))
>>> import Bittide.Instances.Calendar (switchCalendar1k)
-}

type PkgNameS    = String
type PkgVersionS = String
type PkgHashS    = String
type ModNameS    = String
type FuncNameS   = String

-- | Split a package name into the package name, package version, and package
-- hash. For example:
--
-- >>> splitPkgName (PkgName "assoc-1.0.2-aba0a117dd6de51d6a539")
-- ("assoc","1.0.2","aba0a117dd6de51d6a539")
-- >>> splitPkgName (PkgName "base-orphans-0.8.7-2e63b02a231c60dc1867d037a0c")
-- ("base-orphans","0.8.7","2e63b02a231c60dc1867d037a0c")
--
-- If a package name does not conform to $name-$version-$hash, this function will
-- error.
--
splitPkgName :: TH.PkgName -> (PkgNameS, PkgVersionS, PkgHashS)
splitPkgName (TH.PkgName s) =
  case reverse (splitOn "-" s) of
    (hashS:versionS:pkgNameRevS) ->
      (intercalate "-" (reverse pkgNameRevS), versionS, hashS)
    _ ->
      error ("Malformed package name: " <> s <> ". Expected: foo-bar-0.1-hash.")

-- | Split globally bound reference to a variable into its package, module, and
-- function name. Errors if given name is not such a reference. For example:
--
-- >>> splitName 'switchCalendar1k
-- ("bittide-instances","Bittide.Instances.Calendar","switchCalendar1k")
--
splitName :: TH.Name -> (PkgNameS, ModNameS, FuncNameS)
splitName (TH.Name (TH.OccName funcNameS) flavor) =
  case flavor of
    TH.NameG TH.VarName pkgName (TH.ModName modNameS) ->
      case splitPkgName pkgName of
        (pkgNameS, _, _) ->
          (pkgNameS, modNameS, funcNameS)
    _ ->
      error ("Expected globally bound variable, not:\n" <> show flavor)
