-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-|
'Foreign.Storable.Storable' allows to define the size and alignment of
raw memory storable types. However, those values currently are defined
on the term level, although they can be statically determined at
compile time for most standard types. This module extends
'Foreign.Storable.Storable' via the type families 'SizeOf' and
'Alignment', which allow to define the respective values on type level
via type level 'GHC.TypeNats.Nat's.

Template Haskell is used to determine the values of most of the common
base types at compile time.
-}

-- DEVELOPER NOTE: This module currently encounters a lot of
-- redundancies regarding the utilized Template Haskell. They don't
-- have been simplified on purpose, because that would require some
-- multi-stage TH compilation, which triggers a bug in cabal, when
-- statically linking a foreign library.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Bittide.Simulate.RustFFI.Sizes where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.TypeLits (Nat)
import Language.Haskell.TH.Syntax (Type(..), TyLit(..))

import Foreign.C.Types
import Foreign.Ptr (Ptr, IntPtr, WordPtr)
import Foreign.Storable (sizeOf, alignment)

-- | Type family for 'Foreign.Storable.Storable.sizeOf'.
type family SizeOf a :: Nat

-- Base Types
type instance SizeOf Int        = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: Int))
type instance SizeOf Word       = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: Word))
type instance SizeOf Float      = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: Float))
type instance SizeOf Double     = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: Double))

-- Data.Int
type instance SizeOf Int8       = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: Int8))
type instance SizeOf Int16      = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: Int16))
type instance SizeOf Int32      = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: Int32))
type instance SizeOf Int64      = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: Int64))

-- Data.Word
type instance SizeOf Word8      = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: Word8))
type instance SizeOf Word16     = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: Word16))
type instance SizeOf Word32     = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: Word32))
type instance SizeOf Word64     = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: Word64))

-- Foreign.C.Types
type instance SizeOf CChar      = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CChar))
type instance SizeOf CSChar     = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CSChar))
type instance SizeOf CUChar     = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CUChar))
type instance SizeOf CShort     = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CShort))
type instance SizeOf CUShort    = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CUShort))
type instance SizeOf CInt       = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CInt))
type instance SizeOf CUInt      = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CUInt))
type instance SizeOf CLong      = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CLong))
type instance SizeOf CULong     = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CULong))
type instance SizeOf CPtrdiff   = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CPtrdiff))
type instance SizeOf CSize      = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CSize))
type instance SizeOf CWchar     = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CWchar))
type instance SizeOf CSigAtomic = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CSigAtomic))
type instance SizeOf CLLong     = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CLLong))
type instance SizeOf CULLong    = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CULLong))
type instance SizeOf CBool      = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CBool))
type instance SizeOf CIntPtr    = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CIntPtr))
type instance SizeOf CUIntPtr   = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CUIntPtr))
type instance SizeOf CIntMax    = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CIntMax))
type instance SizeOf CUIntMax   = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CUIntMax))
type instance SizeOf CClock     = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CClock))
type instance SizeOf CTime      = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CTime))
type instance SizeOf CUSeconds  = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CUSeconds))
type instance SizeOf CSUSeconds = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CSUSeconds))
type instance SizeOf CFloat     = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CFloat))
type instance SizeOf CDouble    = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: CDouble))

-- Foreign.Ptr
type instance SizeOf (Ptr a)    = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: Ptr Int))
type instance SizeOf IntPtr     = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: IntPtr))
type instance SizeOf WordPtr    = $(pure $ LitT $ NumTyLit $ toInteger $ sizeOf (undefined :: WordPtr))

-- | Type family for 'Foreign.Storable.Storable.alignment'.
type family Alignment a :: Nat

-- Base Types
type instance Alignment Int        = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: Int))
type instance Alignment Word       = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: Word))
type instance Alignment Float      = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: Float))
type instance Alignment Double     = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: Double))

-- Data.Int
type instance Alignment Int8       = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: Int8))
type instance Alignment Int16      = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: Int16))
type instance Alignment Int32      = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: Int32))
type instance Alignment Int64      = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: Int64))

-- Data.Word
type instance Alignment Word8      = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: Word8))
type instance Alignment Word16     = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: Word16))
type instance Alignment Word32     = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: Word32))
type instance Alignment Word64     = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: Word64))

-- Foreign.C.Types
type instance Alignment CChar      = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CChar))
type instance Alignment CSChar     = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CSChar))
type instance Alignment CUChar     = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CUChar))
type instance Alignment CShort     = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CShort))
type instance Alignment CUShort    = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CUShort))
type instance Alignment CInt       = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CInt))
type instance Alignment CUInt      = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CUInt))
type instance Alignment CLong      = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CLong))
type instance Alignment CULong     = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CULong))
type instance Alignment CPtrdiff   = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CPtrdiff))
type instance Alignment CSize      = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CSize))
type instance Alignment CWchar     = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CWchar))
type instance Alignment CSigAtomic = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CSigAtomic))
type instance Alignment CLLong     = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CLLong))
type instance Alignment CULLong    = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CULLong))
type instance Alignment CBool      = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CBool))
type instance Alignment CIntPtr    = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CIntPtr))
type instance Alignment CUIntPtr   = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CUIntPtr))
type instance Alignment CIntMax    = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CIntMax))
type instance Alignment CUIntMax   = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CUIntMax))
type instance Alignment CClock     = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CClock))
type instance Alignment CTime      = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CTime))
type instance Alignment CUSeconds  = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CUSeconds))
type instance Alignment CSUSeconds = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CSUSeconds))
type instance Alignment CFloat     = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CFloat))
type instance Alignment CDouble    = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: CDouble))

-- Foreign.Ptr
type instance Alignment (Ptr a)    = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: Ptr Int))
type instance Alignment IntPtr     = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: IntPtr))
type instance Alignment WordPtr    = $(pure $ LitT $ NumTyLit $ toInteger $ alignment (undefined :: WordPtr))
