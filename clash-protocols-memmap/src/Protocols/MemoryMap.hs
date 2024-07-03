-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Protocols.MemoryMap where

import Clash.Prelude

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import GHC.Stack (HasCallStack, SrcLoc)
import Protocols

import Protocols.MemoryMap.FieldType (FieldType)

-- | Absolute address or offset in bytes
type Address = Integer

-- | Size in bytes
type Size = Integer

{- | A protocol agnostic wrapper for memory mapped protocols. It provides a way
for memory mapped subordinates to propagate their memory map to the top level,
possibly with interconnects merging multiple memory maps.

The current implementation has a few objectives:

  1. Provide a way for memory mapped peripherals to define their memory layout

  2. Make it possible for designers to define hardware designs without specifying
     exact memory addresses if they can be mapped to arbitrary addresses, while
     not giving up the ability to.

  3. Provide a way to check whether memory maps are valid, i.e. whether
     perhipherals do not overlap or exceed their allocated space.

  4. Provide a way to generate human readable documentation

  5. Provide a way to generate data structures for target languages. I.e., if
     a designer instantiates a memory mapped register on type @a@, it should
     be possible to generate an equivalent data structure in Rust or C.
-}
data BackwardAnnotated (annotation :: Type) (a :: Type)

instance (Protocol a) => Protocol (BackwardAnnotated annotation a) where
  type Fwd (BackwardAnnotated annotation a) = Fwd a
  type Bwd (BackwardAnnotated annotation a) = (annotation, Bwd a)

type MemoryMapped a = BackwardAnnotated (SimOnly MemoryMap) a
type RegisterMapped a = BackwardAnnotated (SimOnly (Named Register)) a

unAnnotate :: Circuit a (BackwardAnnotated ann a)
unAnnotate = Circuit $ \(fwd, (_, bwd)) -> (bwd, fwd)

annotation' ::
  (NFDataX (Fwd a), NFDataX (Bwd b)) => Circuit (BackwardAnnotated ann a) b -> ann
annotation' (Circuit f) = ann'
 where
  ((ann', _), _) = f (deepErrorX "")

annotationSnd' ::
  (NFDataX (Fwd a), NFDataX (Fwd b), NFDataX (Bwd c)) =>
  Circuit (a, BackwardAnnotated ann b) c ->
  ann
annotationSnd' (Circuit f) = ann'
 where
  ((_, (ann', _)), _) = f (deepErrorX "")

annotation ::
  (KnownDomain dom, NFDataX (Fwd a), NFDataX (Bwd b)) =>
  ((HiddenClockResetEnable dom) => Circuit (BackwardAnnotated ann a) b) ->
  ann
annotation circ = ann'
 where
  Circuit f = withClockResetEnable clockGen resetGen enableGen circ
  ((ann', _), _) = f (deepErrorX "")

annotationSnd ::
  (KnownDomain dom, NFDataX (Fwd a), NFDataX (Fwd b), NFDataX (Bwd c)) =>
  ((HiddenClockResetEnable dom) => Circuit (a, BackwardAnnotated ann b) c) ->
  ann
annotationSnd circ = ann'
 where
  Circuit f = withClockResetEnable clockGen resetGen enableGen circ
  ((_, (ann', _)), _) = f (deepErrorX "")

data Name = Name
  { name :: String
  -- ^ Name of the 'thing'. Used as an identifier in generated data structures.
  --
  -- TODO: Only allow very basic names here to make sure it can be used in all
  --       common target languages. Provide a Template Haskell helper to make
  --       sure the name is valid at compile time?
  , description :: String
  -- ^ Description of the 'thing'. Used in generated documentation.
  }
  deriving (Show, Eq, Ord)

{- | Wrapper for \"things\" that have a name and a description. These are used
to generate documentation and data structures for target languages.
-}
type Named a = (Name, a)

type NamedLoc a = (Name, SrcLoc, a)

type AbsoluteAddress = Maybe Address

type DeviceName = String

data MemoryMap = MemoryMap
  { deviceDefs :: Map.Map DeviceName DeviceDefinition
  , tree :: MemoryMapTree
  }
  deriving (Show)

data DeviceDefinition = DeviceDefinition
  { deviceName :: Name
  , registers :: [NamedLoc Register]
  , defLocation :: SrcLoc
  }
  deriving (Show)

{- | A tree structure that describes the memory map of a device. Its definitions
are using non-translatable constructs on purpose: Clash is currently pretty
bad at propagating contants properly, so designers should only /produce/
memory maps, not rely on constant folding to be able to extract addresses
from them to use in their designs.
-}
data MemoryMapTree
  = Interconnect SrcLoc AbsoluteAddress [(Address, Size, MemoryMapTree)]
  | DeviceInstance SrcLoc AbsoluteAddress String DeviceName
  deriving (Show)

data Access
  = -- | Managers should only read from this register
    ReadOnly
  | -- | Managers should only write to this register
    WriteOnly
  | -- | Managers can read from and write to this register
    ReadWrite
  deriving (Show)

data Register = Register
  { access :: Access
  , address :: Address
  -- ^ Address / offset of the register
  , fieldType :: FieldType
  -- ^ Type of the register. This is used to generate data structures for
  -- target languages.
  , fieldSize :: Size
  -- ^ Size of the register in bytes
  , reset :: Maybe Natural
  -- ^ Reset value (if any) of register
  }
  deriving (Show)

data ComponentPath
  = Root
  | InterconnectComponent Integer ComponentPath
  deriving (Show)

withPrefix ::
  (HasCallStack) =>
  BitVector n ->
  ((HasCallStack) => Circuit (BackwardAnnotated ann a) b) ->
  Circuit (BackwardAnnotated (BitVector n, ann) a) b
withPrefix p (Circuit f) = Circuit $ \(fwd, bwd) ->
  let
    ((ann, bwd'), fwd') = f (fwd, bwd)
   in
    (((p, ann), bwd'), fwd')

passAnn ::
  Circuit a b ->
  Circuit (BackwardAnnotated ann a) (BackwardAnnotated ann b)
passAnn (Circuit f) = Circuit $ \(fwd, (ann, bwd)) ->
  let (bwd', fwd') = f (fwd, bwd)
   in ((ann, bwd'), fwd')

mergeDeviceDefs :: [Map.Map String DeviceDefinition] -> Map.Map String DeviceDefinition
mergeDeviceDefs = List.foldl Map.union Map.empty
