-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}

-- | Collect all types referenced in a 'MemoryMap' and create a type-map.
module Protocols.MemoryMap.TypeCollect where

import Clash.Prelude hiding (def)

import Protocols.MemoryMap (
  DeviceDefinition (registers),
  DeviceDefinitions,
  NamedLoc (..),
  Register (..),
  RegisterType (..),
 )
import Protocols.MemoryMap.TypeDescription

import Data.Data (Proxy (..))
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq ((:<|)))
import qualified Data.Sequence as Seq
import qualified Language.Haskell.TH as TH

tyReferences ::
  forall a.
  (WithTypeDescription a) =>
  Proxy a ->
  Seq.Seq (TH.Name, WithSomeTypeDescription)
tyReferences p@Proxy = Seq.fromList (dependsOn p) <> Seq.fromList (argTypes p)

allReferences :: [RegisterType] -> Map.Map TH.Name WithSomeTypeDescription
allReferences regs = go Map.empty regsAsList
 where
  regToNameDesc (RegisterType proxy) = ((typeDescription proxy).name, WithSomeTypeDescription proxy)
  regsAsList = Seq.fromList $ regToNameDesc <$> regs

  go alreadyChecked Seq.Empty = alreadyChecked
  go alreadyChecked ((name, desc@(WithSomeTypeDescription proxy)) :<| toCheck)
    | Just _ <- Map.lookup name alreadyChecked = go alreadyChecked (toCheck <> deps)
    | otherwise = go (Map.insert name desc alreadyChecked) (toCheck <> deps)
   where
    deps = tyReferences proxy

collect :: DeviceDefinitions -> Map.Map TH.Name WithSomeTypeDescription
collect defs0 = allReferences regTypes
 where
  defs1 = Map.elems defs0

  regTypes = (.value.fieldType) <$> L.concatMap (.registers) defs1
