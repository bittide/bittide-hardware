-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Protocols.MemoryMap (
  module Protocols.MemoryMap.Check.Normalised,
  MM,
  ConstB,
  ConstF,
  constB,
  constF,
  getConstB,
  getConstBAny,
  withName,
  withAbsAddr,
  withMemoryMap,
  withPrefix,
  Access(..),
  DeviceDefinitions,
  DeviceDefinition(..),
  Name(..),
  NamedLoc,
  Register(..),
  MemoryMap(..),
  mergeDeviceDefs,
  deviceSize,
) where

import Clash.Prelude

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import GHC.Stack (HasCallStack, SrcLoc, getCallStack, callStack)
import Protocols

import Protocols.MemoryMap.Check.Normalised
import Protocols.MemoryMap.FieldType (FieldType)


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

type MM = SimOnly MemoryMap

data ConstF (a :: Type)

data ConstB (a :: Type)

instance Protocol (ConstF a) where
  type Fwd (ConstF a) = a
  type Bwd (ConstF a) = ()

instance Protocol (ConstB a) where
  type Fwd (ConstB a) = ()
  type Bwd (ConstB a) = a


constF :: a -> Circuit () (ConstF a)
constF val = Circuit $ \((), ()) -> ((), val)

constB :: a -> Circuit (ConstB a) ()
constB val = Circuit $ \((), ()) -> (val, ())

getConstB :: Circuit (ConstB a) () -> a
getConstB (Circuit f) = fst $ f ((), ())

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
-- type Named a = (Name, a)

type NamedLoc a = (Name, SrcLoc, a)

type DeviceDefinitions = Map.Map DeviceName DeviceDefinition

data MemoryMap = MemoryMap
  { deviceDefs :: DeviceDefinitions
  , tree :: MemoryMapTree
  }
  deriving (Show)


data DeviceDefinition = DeviceDefinition
  { deviceName :: Name
  , registers :: [NamedLoc Register]
  , defLocation :: SrcLoc
  }
  deriving (Show)

deviceSize :: DeviceDefinition -> Integer
deviceSize def = List.foldr (\(_, _, reg) acc -> max acc (reg.address + reg.fieldSize)) 0 def.registers


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


withName :: HasCallStack => String -> Circuit (ConstB MM, a) b -> Circuit (ConstB MM, a) b
withName name' (Circuit f) = Circuit go
 where
  callLoc = case getCallStack callStack of
    ((_, loc) : _) -> loc
    _ -> error "The caller of `withName` needs `HasCallStack`"
  go (((), fwdA), bwdB) = ((SimOnly mm', bwdA), fwdB)
   where
    ((SimOnly mm, bwdA), fwdB) = f (((), fwdA), bwdB)
    mm' = mm { tree = WithName callLoc name' mm.tree }


withAbsAddr :: HasCallStack => Address -> Circuit (ConstB MM, a) b -> Circuit (ConstB MM, a) b
withAbsAddr addr (Circuit f) = Circuit go
 where
  callLoc = case getCallStack callStack of
    ((_, loc) : _) -> loc
    _ -> error "The caller of `withAbsAddr` needs `HasCallStack`"
  go (((), fwdA), bwdB) = ((SimOnly mm', bwdA), fwdB)
   where
    ((SimOnly mm, bwdA), fwdB) = f (((), fwdA), bwdB)
    mm' = mm { tree = WithAbsAddr callLoc addr mm.tree }


withMemoryMap :: MemoryMap -> Circuit a b -> Circuit (ConstB MM, a) b
withMemoryMap mm = withConstB (SimOnly mm)

withPrefix :: v -> Circuit a b -> Circuit (ConstB v, a) b
withPrefix = withConstB

withConstB :: forall v a b . v -> Circuit a b -> Circuit (ConstB v, a) b
withConstB val (Circuit f) = Circuit go
 where
  go :: (((), Fwd a), Bwd b) -> ((v, Bwd a), Fwd b)
  go (((), fwdA), bwdB) = ((val, bwdA), fwdB)
   where
    (bwdA, fwdB) = f (fwdA, bwdB)


mergeDeviceDefs :: [Map.Map String DeviceDefinition] -> Map.Map String DeviceDefinition
mergeDeviceDefs = List.foldl Map.union Map.empty

getConstBAny :: Circuit (ConstB v, a) b -> v
getConstBAny (Circuit f) = val
 where
  ((val, _), _) = f (((), errorX ""), errorX "")
