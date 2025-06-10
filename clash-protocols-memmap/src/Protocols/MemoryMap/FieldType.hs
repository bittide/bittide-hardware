-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generate a description of a type for use in a memory map.
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

-- | Description of an allowed type in a device register within a memory map.
data FieldType
  = BoolFieldType
  | BitVectorFieldType Word
  | SignedFieldType Word
  | UnsignedFieldType Word
  | IndexFieldType Word
  | FloatSingleType
  | FloatDoubleType
  | SumOfProductFieldType TypeName [Named [Named FieldType]]
  | VecFieldType Word FieldType
  | TypeReference FieldType [FieldType]
  | TypeVariable Integer
  deriving (Show, Eq)

{- | Generate a description of the type for use in a memory map.

This class can be derived automatically for non-generic types.
Types with generic arguments can provide a minimal instance definition
which specifies 'Generics', 'WithVars' and 'args'.
-}
class (KnownNat (Generics a)) => ToFieldType (a :: Type) where
  -- | The number of type variables this type has.
  type Generics a :: Nat

  type Generics a = 0

  -- | A version of this type where each type variable is instantiated to be
  -- numerated versions of 'Var', a placeholder used to keep track of type
  -- variable instances within the type.
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
    (ft, []) -> TypeReference ft []
    (TypeReference _ft _args', _args'') -> error "shouldn't happen!??"
    (ft, args') -> TypeReference ft args'

-- | Placeholder type to keep track of generic arguments in a type.
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

{- | XXX: We lie about the constructor name here, as the generator needs the
       type name to be the same as the constructor name. From GHC 9.6 up though
       the _type_ name of a tuple is @Tuple2@, but the constructor name is
       @(,)@.
-}
tupType :: Int -> FieldType
tupType n =
  SumOfProductFieldType
    TypeName
      { name = "Tuple" <> show n
      , moduleName = "GHC.Tuple.Prim"
      , packageName = "ghc-prim"
      , isNewType = False
      }
    [
      ( "Tuple" <> show n
      , [("", TypeVariable nRef) | nRef <- [0 .. fromIntegral (n - 1)]]
      )
    ]

instance (ToFieldType a, ToFieldType b) => ToFieldType (a, b) where
  type Generics (a, b) = 2
  type WithVars (a, b) = (Var 0, Var 1)
  args = toFieldType @a :> toFieldType @b :> Nil
  toFieldType = TypeReference (tupType 2) [toFieldType @a, toFieldType @b]

instance (ToFieldType a, ToFieldType b, ToFieldType c) => ToFieldType (a, b, c) where
  type Generics (a, b, c) = 3
  type WithVars (a, b, c) = (Var 0, Var 1, Var 2)
  args = toFieldType @a :> toFieldType @b :> toFieldType @c :> Nil
  toFieldType = TypeReference (tupType 3) [toFieldType @a, toFieldType @b, toFieldType @c]

instance (ToFieldType a, ToFieldType b, ToFieldType c, ToFieldType d) => ToFieldType (a, b, c, d) where
  type Generics (a, b, c, d) = 4
  type WithVars (a, b, c, d) = (Var 0, Var 1, Var 2, Var 3)
  args = toFieldType @a :> toFieldType @b :> toFieldType @c :> toFieldType @d :> Nil
  toFieldType =
    TypeReference
      (tupType 4)
      [toFieldType @a, toFieldType @b, toFieldType @c, toFieldType @d]

instance (ToFieldType a, ToFieldType b) => ToFieldType (Either a b) where
  type Generics (Either a b) = 2
  type WithVars (Either a b) = Either (Var 0) (Var 1)
  args = toFieldType @a :> toFieldType @b :> Nil

instance ToFieldType Bool where
  toFieldType = BoolFieldType

instance ToFieldType Bit where
  toFieldType = BitVectorFieldType 1

instance forall n. (KnownNat n) => ToFieldType (BitVector n) where
  toFieldType = BitVectorFieldType (fromIntegral $ snatToInteger $ SNat @n)

instance forall n. (KnownNat n) => ToFieldType (Signed n) where
  toFieldType = SignedFieldType (fromIntegral $ snatToInteger $ SNat @n)

instance forall n. (KnownNat n) => ToFieldType (Unsigned n) where
  toFieldType = UnsignedFieldType (fromIntegral $ snatToInteger $ SNat @n)

instance forall n. (KnownNat n) => ToFieldType (Index n) where
  toFieldType = IndexFieldType (fromIntegral $ snatToInteger $ SNat @n)

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

instance ToFieldType Float where toFieldType = FloatSingleType
instance ToFieldType Double where toFieldType = FloatDoubleType

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
