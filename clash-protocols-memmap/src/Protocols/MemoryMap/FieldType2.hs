{-# LANGUAGE AllowAmbiguousTypes #-}
module Protocols.MemoryMap.FieldType2 where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Data.Tuple (Solo (MkSolo))

type TypeName = String

data TypeArgKind = TKType | TKNat

data TypeDefinition :: Nat -> Type where
  Primitive :: PrimitiveType n -> TypeDefinition n
  SumOfProduct :: SopDefinition n -> TypeDefinition n

data SopDefinition :: Nat -> Type where
  SopDefinition ::
    { name :: TypeName
    , args :: Vec n (String, TypeArgKind)
    , variants :: [(String, [(String, FieldType)])]
    } -> SopDefinition n

data PrimitiveType :: Nat -> Type where
  PUnit :: PrimitiveType 0
  PBool :: PrimitiveType 0
  PBitVector :: PrimitiveType 1
  PSigned :: PrimitiveType 1
  PUnsigned :: PrimitiveType 1
  PIndex :: PrimitiveType 1
  PFloat :: PrimitiveType 0
  PDouble :: PrimitiveType 0
  PVec :: PrimitiveType 2

data FieldType :: Type where
  Reference :: TypeDefinition n -> Vec n TypeArgument -> FieldType
  TypeVariable :: Integer -> FieldType

data TypeArgument
  = TNatLit Integer
  | TNatVar Integer
  | TTypeVar Integer
  | TType FieldType

class ToFieldType (a :: Type) where
  type Generics a :: Nat
  type Generics a = 0

  genericTypeDesc :: TypeDefinition (Generics a)
  concreteFieldType :: Vec (Generics a) TypeArgument -> FieldType
  concreteFieldType args = Reference (genericTypeDesc @a) args

  getTypeArgs :: Vec (Generics a) TypeArgument

instance ToFieldType () where
  genericTypeDesc = Primitive PUnit
  concreteFieldType _ = Reference (Primitive PUnit) Nil
  getTypeArgs = Nil

instance forall n. (KnownNat n) => ToFieldType (BitVector n) where
  type Generics (BitVector n) = 1

  genericTypeDesc = Primitive PBitVector
  getTypeArgs = (TNatLit $ natToInteger @n) :> Nil

instance forall n a . (KnownNat n, ToFieldType a) => ToFieldType (Vec n a) where
  type Generics (Vec n a) = 2
  genericTypeDesc = Primitive PVec
  getTypeArgs = (TNatLit $ natToInteger @n) :> (TType $ concreteFieldType @a undefined) :> Nil


instance (ToFieldType a) => ToFieldType (Maybe a) where
  type Generics (Maybe a) = 1
  genericTypeDesc = SumOfProduct $ SopDefinition
    { name = "Maybe"
    , args = (("a", TKType) :> Nil)
    , variants = [("Just", [("", TypeVariable 0)])]
    }
