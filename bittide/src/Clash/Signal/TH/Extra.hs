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
data MyRecord = MyRecord { field1 :: Int, field2 :: Bool }
deriveSignalHasFields ''MyRecord
This will generate the following instances:
instance HasField "field1" (Signal dom MyRecord) (Signal dom Int) where
  getField = fmap field1
instance HasField "field2" (Signal dom MyRecord) (Signal dom Bool) where
  getField = fmap field2
-}
deriveSignalHasFields :: Name -> Q [Dec]
deriveSignalHasFields recordName = do
  -- Extract information
  info <- reify recordName
  case info of
    -- If the name refers to a record type, generate instances for each field
    TyConI (DataD _ _ _ _ [RecC _ fields] _) -> do
      let
        recordType = conT recordName
        mkInstance :: (Name, Bang, Type) -> Q [Dec]
        mkInstance (fieldName, _fieldBang, fieldType) =
          [d|
            instance HasField $fieldStr (Signal dom $recordType) (Signal dom $(pure fieldType)) where
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
