-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Protocols.MemoryMap.FieldType where

import Clash.Prelude
import Data.Int
import Data.Word
import GHC.Generics hiding (moduleName, packageName)
import qualified GHC.Generics

data TypeName = TypeName
  { name :: String
  , moduleName :: String
  , packageName :: String
  , isNewType :: Bool
  }
  deriving (Show, Eq, Ord)

type Named a = (String, a)

data FieldType
  = BoolFieldType
  | BitVectorFieldType Word
  | SignedFieldType Word
  | SumOfProductFieldType TypeName [Named [Named FieldType]]
  | UnsignedFieldType Word
  | VecFieldType Word FieldType
  | TypeReference FieldType [FieldType]
  | TypeVariable Integer
  deriving (Show, Eq)

class (KnownNat (Generics a)) => ToFieldType (a :: Type) where
  type Generics a :: Nat
  type Generics a = 0

  type WithVars a :: Type
  type WithVars a = a

  generics :: SNat (Generics a)
  generics = SNat @(Generics a)

  toFieldType :: FieldType

  args :: Vec (Generics a) FieldType
  args = repeat undefined

  default toFieldType ::
    (Generic (WithVars a), GToFieldType (Rep (WithVars a))) => FieldType
  toFieldType = case (gToFieldType @(Rep (WithVars a)), toList (args @a)) of
    (ft, []) -> ft
    (TypeReference _ft _args', _args'') -> error "shouldn't happen!??"
    (ft, args') -> TypeReference ft args'

newtype Var (n :: Nat) = Var Integer

instance (KnownNat n) => ToFieldType (Var n) where
  type Generics (Var n) = 0
  type WithVars (Var n) = Var n

  toFieldType = TypeVariable $ snatToInteger (SNat @n)

instance ToFieldType ()

instance (ToFieldType a) => ToFieldType (Maybe a) where
  type Generics (Maybe a) = 1
  type WithVars (Maybe a) = Maybe (Var 0)
  args = toFieldType @a :> Nil

instance (ToFieldType a, ToFieldType b) => ToFieldType (a, b) where
  type Generics (a, b) = 2
  type WithVars (a, b) = (Var 0, Var 1)
  args = toFieldType @a :> toFieldType @b :> Nil

instance (ToFieldType a, ToFieldType b, ToFieldType c) => ToFieldType (a, b, c) where
  type Generics (a, b, c) = 3
  type WithVars (a, b, c) = (Var 0, Var 1, Var 2)
  args = toFieldType @a :> toFieldType @b :> toFieldType @c :> Nil

instance (ToFieldType a, ToFieldType b, ToFieldType c, ToFieldType d) => ToFieldType (a, b, c, d) where
  type Generics (a, b, c, d) = 4
  type WithVars (a, b, c, d) = (Var 0, Var 1, Var 2, Var 3)
  args = toFieldType @a :> toFieldType @b :> toFieldType @c :> toFieldType @d :> Nil

instance (ToFieldType a, ToFieldType b) => ToFieldType (Either a b) where
  type Generics (Either a b) = 2
  type WithVars (Either a b) = Either (Var 0) (Var 1)
  args = toFieldType @a :> toFieldType @b :> Nil

data MyTestType
  = SomeVar
  | AnotherVar (Maybe (BitVector 8))
  | YetAnotherVar Bool (Either (Signed 8) (Unsigned 8))
  deriving (Generic, ToFieldType)

data MyTestGen a
  = Yep
  | UhOh (Maybe a)
  deriving (Generic)

instance (ToFieldType a) => ToFieldType (MyTestGen a) where
  type Generics (MyTestGen a) = 1
  type WithVars (MyTestGen a) = MyTestGen (Var 0)

  args = toFieldType @a :> Nil

instance ToFieldType Bool where
  toFieldType = BoolFieldType

instance ToFieldType Bit where
  toFieldType = BitVectorFieldType 1

instance forall n. (KnownNat n) => ToFieldType (BitVector n) where
  toFieldType = BitVectorFieldType (fromIntegral $ snatToInteger $ SNat @n)

instance forall n. (KnownNat n) => ToFieldType (Signed n) where
  toFieldType = SignedFieldType (fromIntegral $ snatToInteger $ SNat @n)

instance forall n. (KnownNat n) => ToFieldType (Unsigned n) where
  toFieldType = SignedFieldType (fromIntegral $ snatToInteger $ SNat @n)

instance forall n a. (KnownNat n, ToFieldType a) => ToFieldType (Vec n a) where
  toFieldType = VecFieldType (fromIntegral $ snatToInteger $ SNat @n) (toFieldType @a)

instance ToFieldType Word8 where toFieldType = UnsignedFieldType 8
instance ToFieldType Word16 where toFieldType = UnsignedFieldType 16
instance ToFieldType Word32 where toFieldType = UnsignedFieldType 32
instance ToFieldType Word64 where toFieldType = UnsignedFieldType 64
instance ToFieldType Int8 where toFieldType = SignedFieldType 8
instance ToFieldType Int16 where toFieldType = SignedFieldType 16
instance ToFieldType Int32 where toFieldType = SignedFieldType 32
instance ToFieldType Int64 where toFieldType = SignedFieldType 64

instance ToFieldType Int where toFieldType = SignedFieldType 32

class GToFieldType f where
  gToFieldType :: FieldType

instance (Datatype m, GFieldTypeCons inner) => GToFieldType (D1 m inner) where
  gToFieldType = SumOfProductFieldType name' $ gToFieldTypeCons @inner
   where
    name' = TypeName{name = tyName, moduleName = mod', packageName = pck, isNewType = newtype'}
    mod' = GHC.Generics.moduleName @m undefined
    tyName = datatypeName @m undefined
    newtype' = isNewtype @m undefined
    pck = GHC.Generics.packageName @m undefined

instance (ToFieldType inner) => GToFieldType (Rec0 inner) where
  gToFieldType = toFieldType @inner

class GFieldTypeCons f where
  gToFieldTypeCons :: [Named [Named FieldType]]

instance GFieldTypeCons V1 where
  gToFieldTypeCons = []

instance (Constructor m, GFieldTypeFields inner) => GFieldTypeCons (C1 m inner) where
  gToFieldTypeCons = [(conName', gFieldTypeFields @inner)]
   where
    conName' = conName @m undefined

instance (GFieldTypeCons a, GFieldTypeCons b) => GFieldTypeCons (a :+: b) where
  gToFieldTypeCons = gToFieldTypeCons @a <> gToFieldTypeCons @b

class GFieldTypeFields f where
  gFieldTypeFields :: [Named FieldType]

instance GFieldTypeFields U1 where
  gFieldTypeFields = []

instance (Selector m, GToFieldType inner) => GFieldTypeFields (S1 m inner) where
  gFieldTypeFields = [(fieldName, gToFieldType @inner)]
   where
    fieldName = selName @m undefined

instance (GFieldTypeFields a, GFieldTypeFields b) => GFieldTypeFields (a :*: b) where
  gFieldTypeFields = gFieldTypeFields @a <> gFieldTypeFields @b
