-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Clash.Signal.TH.Extra where

import Prelude

import Clash.Signal
import Data.String.Interpolate
import GHC.Records (HasField (..))
import Language.Haskell.TH

{- | Derive instances of 'HasField' for a Signals of Records:

Example:

@
data MyRecord = MyRecord { field1 :: Int, field2 :: Bool }
deriveSignalHasFields ''MyRecord
@

This will generate the following instances:

@
instance HasField "field1" (Signal dom MyRecord) (Signal dom Int) where
  getField = fmap field1
instance HasField "field2" (Signal dom MyRecord) (Signal dom Bool) where
  getField = fmap field2
@

The record type may also take type arguments. For instance, this type:

@
data MyRecord n = MyRecord { field1 :: BitVector n, field2 :: BitVector n (Unsigned 8) }
@

Will generate the following instances:

@
instance HasField "field1" (Signal dom (MyRecord n)) (Signal dom (BitVector n)) where
  getField = fmap field1
instance HasField "field2" (Signal dom (MyRecord n)) (Signal dom (Vec n (Unsigned 8))) where
  getField = fmap field2
@
-}
deriveSignalHasFields :: Name -> Q [Dec]
deriveSignalHasFields recordName = do
  -- Extract information
  info <- reify recordName
  case info of
    -- If the name refers to a record type, generate instances for each field
    TyConI (DataD _ _ tvb _ [RecC _ fields] _) -> do
      runIO $ mapM_ print tvb
      let
        recordType = conT recordName
        recordTyInputs = varT . go <$> tvb
         where
          go (PlainTV n _) = n
          go (KindedTV n _ _) = n
        recordAppT = go recordType recordTyInputs
         where
          go ty [] = ty
          go ty (h : t) = go (appT ty h) t
        mkInstance :: (Name, Bang, Type) -> Q [Dec]
        mkInstance (fieldName, _fieldBang, fieldType) =
          [d|
            instance HasField $fieldStr (Signal dom $recordAppT) (Signal dom $(pure fieldType)) where
              getField = fmap $(varE fieldName)
            |]
         where
          fieldStr = litT (strTyLit (nameBase fieldName))
      concat <$> mapM mkInstance fields
    -- If the name refers to something else, throw an error
    _ ->
      let msg =
            [__i|
          Expected a record type, but got something else.
          The name '#{recordName}' refers to a different kind of type, namely '#{info}'.
          |]
       in error msg
