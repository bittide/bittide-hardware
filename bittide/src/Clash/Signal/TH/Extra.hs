-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Clash.Signal.TH.Extra where

import Prelude

import Clash.Signal
import Data.String.Interpolate
import GHC.Records (HasField (..))
import Language.Haskell.TH

{- | Derive instances of 'HasField' for a Signals of Records

Calling this function requires the `UndecidableInstances` language pragma to be enabled,
as can be seen from the example below.

Example:

> data MyRecord n = MyRecord { field1 :: BitVector n, field2 :: Vec n (Unsigned 8) }
> deriveSignalHasFields ''MyRecord

Will generate the following instances:

> instance (t ~ (BitVector n)) HasField "field1" (Signal dom (MyRecord n)) (Signal dom t) where
>   getField = fmap field1
> instance (t ~ (Vec n (Unsigned 8))) HasField "field2" (Signal dom (MyRecord n)) (Signal dom t) where
>   getField = fmap field2
-}
deriveSignalHasFields :: Name -> Q [Dec]
deriveSignalHasFields recordName = do
  -- Extract information
  info <- reify recordName
  case info of
    -- If the name refers to a record type, generate instances for each field
    TyConI (DataD _ _ tvb _ [RecC _ fields] _) -> do
      let
        recordType = foldl appT (conT recordName) recordTyInputs
        recordTyInputs = varT . go <$> tvb
         where
          go (PlainTV n _) = n
          go (KindedTV n _ _) = n
        mkInstance :: (Name, Bang, Type) -> Q [Dec]
        mkInstance (fieldName, _fieldBang, fieldType) =
          [d|
            instance (t ~ $(pure fieldType)) => HasField $fieldStr (Signal dom $recordType) (Signal dom t) where
              getField = fmap $(varE fieldName)
            |]
         where
          fieldStr = litT (strTyLit (nameBase fieldName))
      concat <$> mapM mkInstance fields
    -- If the name refers to something else, throw an error
    _ ->
      error
        [__i|
          Expected a record type, but got something else.
          The name '#{recordName}' refers to a different kind of type, namely '#{info}'.
          |]
