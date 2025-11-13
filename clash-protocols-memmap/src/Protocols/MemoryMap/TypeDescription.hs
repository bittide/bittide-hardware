-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Due to TemplateHaskell stage restriction, this module
-- contains numerous instances of 'WithTypeDescription' that would otherwise
-- live "next to" the actual type-class definition.
-- Because of this, GHC produces orphan warnings, as this module neither
-- contains the type class definition or the defintion of the primitive types we're
-- creating the instances for.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generate a description of a type for use in a memory map.
module Protocols.MemoryMap.TypeDescription (
  module Protocols.MemoryMap.TypeDescription.TH,
) where

import Clash.Prelude

import Data.Data (Proxy (..))
import Data.Int
import Data.Word

import Protocols.MemoryMap.TypeDescription.TH

import qualified Language.Haskell.TH as TH

builtIn :: TH.Name -> Builtin -> [TypeArgumentDecl] -> TypeDescription
builtIn name builtinTy args =
  TypeDescription
    { name = name
    , args = args
    , definition = Builtin builtinTy
    }

instance (KnownNat n) => WithTypeDescription (BitVector n) where
  typeDescription Proxy = builtIn ''BitVector BitVector [TadNat ''n]
  dependsOn Proxy = []
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''BitVector [TypeNat (natToInteger @n)]

instance (KnownNat n, WithTypeDescription a) => WithTypeDescription (Vec n a) where
  typeDescription Proxy = builtIn ''Vec Vector [TadNat ''n, TadType ''a]
  dependsOn Proxy = []
  argTypes Proxy = [((typeDescription (Proxy @a)).name, WithSomeTypeDescription (Proxy @a))]
  asTypeRef Proxy = TypeInst ''Vec [TypeNat (natToInteger @n), asTypeRef (Proxy @a)]

instance (KnownNat n) => WithTypeDescription (Unsigned n) where
  typeDescription Proxy = builtIn ''Unsigned Unsigned [TadNat ''n]
  dependsOn Proxy = []
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''Unsigned [TypeNat $ natToInteger @n]

instance (KnownNat n) => WithTypeDescription (Signed n) where
  typeDescription Proxy = builtIn ''Signed Signed [TadNat ''n]
  dependsOn Proxy = []
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''Signed [TypeNat $ natToInteger @n]

instance (KnownNat n) => WithTypeDescription (Index n) where
  typeDescription Proxy = builtIn ''Index Index [TadNat ''n]
  dependsOn Proxy = []
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''Index [TypeNat $ natToInteger @n]

instance WithTypeDescription Bool where
  typeDescription Proxy = builtIn ''Bool Bool []
  dependsOn Proxy = []
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''Bool []

instance WithTypeDescription Float where
  typeDescription Proxy = builtIn ''Float Float []
  dependsOn Proxy = []
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''Float []

instance WithTypeDescription Double where
  typeDescription Proxy = builtIn ''Double Double []
  dependsOn Proxy = []
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''Double []

synonym :: TH.Name -> TypeRef -> TypeDescription
synonym name aliasTy =
  TypeDescription
    { name = name
    , args = []
    , definition = Synonym aliasTy
    }

instance WithTypeDescription Word8 where
  typeDescription Proxy = synonym ''Word8 (TypeInst ''Unsigned [TypeNat 8])
  dependsOn Proxy = [(''Unsigned, WithSomeTypeDescription (Proxy @(Unsigned 8)))]
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''Word8 []

instance WithTypeDescription Word16 where
  typeDescription Proxy = synonym ''Word16 (TypeInst ''Unsigned [TypeNat 16])
  dependsOn Proxy = [(''Unsigned, WithSomeTypeDescription (Proxy @(Unsigned 16)))]
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''Word16 []

instance WithTypeDescription Word32 where
  typeDescription Proxy = synonym ''Word32 (TypeInst ''Unsigned [TypeNat 32])
  dependsOn Proxy = [(''Unsigned, WithSomeTypeDescription (Proxy @(Unsigned 32)))]
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''Word32 []

instance WithTypeDescription Word64 where
  typeDescription Proxy = synonym ''Word64 (TypeInst ''Unsigned [TypeNat 64])
  dependsOn Proxy = [(''Unsigned, WithSomeTypeDescription (Proxy @(Unsigned 64)))]
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''Word64 []

instance WithTypeDescription Int8 where
  typeDescription Proxy = synonym ''Int8 (TypeInst ''Signed [TypeNat 8])
  dependsOn Proxy = [(''Signed, WithSomeTypeDescription (Proxy @(Signed 8)))]
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''Int8 []

instance WithTypeDescription Int16 where
  typeDescription Proxy = synonym ''Int16 (TypeInst ''Signed [TypeNat 16])
  dependsOn Proxy = [(''Signed, WithSomeTypeDescription (Proxy @(Signed 16)))]
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''Int16 []

instance WithTypeDescription Int32 where
  typeDescription Proxy = synonym ''Int32 (TypeInst ''Signed [TypeNat 32])
  dependsOn Proxy = [(''Signed, WithSomeTypeDescription (Proxy @(Signed 32)))]
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''Int32 []

instance WithTypeDescription Int64 where
  typeDescription Proxy = synonym ''Int64 (TypeInst ''Signed [TypeNat 64])
  dependsOn Proxy = [(''Signed, WithSomeTypeDescription (Proxy @(Signed 64)))]
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''Int64 []

instance WithTypeDescription Int where
  typeDescription Proxy = synonym ''Int (TypeInst ''Signed [TypeNat 32])
  dependsOn Proxy = [(''Signed, WithSomeTypeDescription (Proxy @(Signed 32)))]
  argTypes Proxy = []
  asTypeRef Proxy = TypeInst ''Int []

deriveTypeDescription ''Maybe
deriveTypeDescription ''Either

deriveTypeDescription ''()
deriveTypeDescription ''(,)
deriveTypeDescription ''(,,)
deriveTypeDescription ''(,,,)
deriveTypeDescription ''(,,,,)
deriveTypeDescription ''(,,,,,)
deriveTypeDescription ''(,,,,,,)
