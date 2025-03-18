-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

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
  withTag,
  withTags,
  withMemoryMap,
  withPrefix,
  unMemmap,
  todoMM,
  Access (..),
  DeviceDefinitions,
  DeviceDefinition (..),
  Name (..),
  NamedLoc,
  regType,
  regTypeSplit,
  regFieldType,
  regByteSizeC,
  Register (..),
  MemoryMap (..),
  mergeDeviceDefs,
  deviceSize,
  deviceSingleton,
  locHere,
  locCaller,
) where

import Clash.Prelude (
  Eq,
  Integer,
  Maybe,
  Natural,
  Num ((+)),
  Ord (max),
  Show (show),
  SimOnly (..),
  String,
  Type,
  error,
  errorX,
  flip,
  fst,
  natToNum,
  ($),
  type (<=),
 )

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import GHC.Stack (HasCallStack, SrcLoc, callStack, getCallStack)
import Protocols

import BitPackC
import Data.Data (Proxy (Proxy))
import qualified Data.List as L
import Protocols.Idle
import Protocols.MemoryMap.Check.Normalised
import Protocols.MemoryMap.FieldType

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

instance IdleCircuit (ConstB a) where
  -- idleFwd :: Data.Proxy.Proxy (ConstB a) -> Fwd (ConstB a)
  idleFwd Proxy = ()

  -- idleBwd :: Data.Proxy.Proxy (ConstB a) -> Bwd (ConstB a)
  idleBwd Proxy = error ""

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
  , tags :: [String]
  }
  deriving (Show)

deviceSize :: DeviceDefinition -> Integer
deviceSize def' =
  List.foldr
    ( \(_, _, reg) acc ->
        max acc (reg.address + regByteSizeC reg.fieldType)
    )
    0
    def'.registers

data Access
  = -- | Managers should only read from this register
    ReadOnly
  | -- | Managers should only write to this register
    WriteOnly
  | -- | Managers can read from and write to this register
    ReadWrite
  deriving (Show)

regTypeSplit ::
  forall a b.
  (ToFieldType b, BitPackC a) =>
  (1 <= AlignmentC a) =>
  RegisterType
regTypeSplit = RegisterType (Proxy @(RegTypeSplit a b))

data RegTypeSplit a b

instance (ToFieldType b) => ToFieldType (RegTypeSplit a b) where
  type Generics (RegTypeSplit a b) = Generics b
  type WithVars (RegTypeSplit a b) = WithVars b

  generics = generics @b

  toFieldType = toFieldType @b

  args = args @b

instance (BitPackC a) => BitPackC (RegTypeSplit a b) where
  type ByteSizeC (RegTypeSplit a b) = ByteSizeC a
  type AlignmentC (RegTypeSplit a b) = AlignmentC a
  packC = error "`BitPackC` instance used from `RegTypeSplit`, this should not happen"
  unpackC = error "`BitPackC` instance used from `RegTypeSplit`, this should not happen"

data RegisterType where
  RegisterType :: (ToFieldType a, BitPackC a) => Proxy a -> RegisterType

regFieldType :: RegisterType -> FieldType
regFieldType (RegisterType proxy) = inner proxy
 where
  inner :: forall x. (ToFieldType x) => Proxy x -> FieldType
  inner Proxy = toFieldType @x

regByteSizeC :: (Num a) => RegisterType -> a
regByteSizeC (RegisterType proxy) = inner proxy
 where
  inner :: forall x a. (BitPackC x, Num a) => Proxy x -> a
  inner Proxy = natToNum @(ByteSizeC x)

regType :: forall a. (ToFieldType a, BitPackC a) => RegisterType
regType = RegisterType (Proxy @a)

instance Show RegisterType where
  show regType' = show $ regFieldType regType'

data Register = Register
  { access :: Access
  , address :: Address
  -- ^ Address / offset of the register
  , fieldType :: RegisterType
  -- ^ Size of the register in bytes
  , reset :: Maybe Natural
  -- ^ Reset value (if any) of register
  , tags :: [String]
  }
  deriving (Show)

withName ::
  (HasCallStack) => String -> Circuit (ConstB MM, a) b -> Circuit (ConstB MM, a) b
withName name' (Circuit f) = Circuit go
 where
  go (((), fwdA), bwdB) = ((SimOnly mm', bwdA), fwdB)
   where
    ((SimOnly mm, bwdA), fwdB) = f (((), fwdA), bwdB)
    mm' = mm{tree = WithName locCaller name' mm.tree}

withTag ::
  (HasCallStack) => String -> Circuit (ConstB MM, a) b -> Circuit (ConstB MM, a) b
withTag tag (Circuit f) = Circuit go
 where
  go (((), fwdA), bwdB) = ((SimOnly mm', bwdA), fwdB)
   where
    ((SimOnly mm, bwdA), fwdB) = f (((), fwdA), bwdB)
    mm' = mm{tree = WithTag locCaller tag mm.tree}

withTags ::
  (HasCallStack) => [String] -> Circuit (ConstB MM, a) b -> Circuit (ConstB MM, a) b
withTags tags' (Circuit f) = Circuit go
 where
  go (((), fwdA), bwdB) = ((SimOnly mm', bwdA), fwdB)
   where
    ((SimOnly mm, bwdA), fwdB) = f (((), fwdA), bwdB)
    mm' = mm{tree = tree'}
    tree' = L.foldl (flip (WithTag locCaller)) mm.tree tags'

withAbsAddr ::
  (HasCallStack) => Address -> Circuit (ConstB MM, a) b -> Circuit (ConstB MM, a) b
withAbsAddr addr (Circuit f) = Circuit go
 where
  go (((), fwdA), bwdB) = ((SimOnly mm', bwdA), fwdB)
   where
    ((SimOnly mm, bwdA), fwdB) = f (((), fwdA), bwdB)
    mm' = mm{tree = WithAbsAddr locCaller addr mm.tree}

withMemoryMap :: MemoryMap -> Circuit a b -> Circuit (ConstB MM, a) b
withMemoryMap mm = withConstB (SimOnly mm)

withPrefix :: v -> Circuit a b -> Circuit (ConstB v, a) b
withPrefix = withConstB

withConstB :: forall v a b. v -> Circuit a b -> Circuit (ConstB v, a) b
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

deviceSingleton :: DeviceDefinition -> DeviceDefinitions
deviceSingleton def' = Map.singleton def'.deviceName.name def'

unMemmap :: Circuit (ConstB MM, a) b -> Circuit a b
unMemmap (Circuit f) = Circuit go
 where
  go (fwdA, bwdB) = (bwdA, fwdB)
   where
    ((_, bwdA), fwdB) = f (((), fwdA), bwdB)

locHere :: (HasCallStack) => SrcLoc
locHere = case getCallStack callStack of
  (_, callLoc) : _ -> callLoc
  _ -> error "`locHere` needs to be called in a `HasCallStack` context"

locCaller :: (HasCallStack) => SrcLoc
locCaller = case getCallStack callStack of
  (_, _) : (_, callerLoc) : _ -> callerLoc
  (fn, _) : _ -> error $ "`" List.++ fn List.++ "` needs to be called in a `HasCallStack` context"
  _ ->
    error "`locCaller` needs to be called with at least two levels of `HasCallStack` context"

todoMM :: (HasCallStack) => SimOnly MemoryMap
todoMM =
  SimOnly
    $ MemoryMap
      { deviceDefs = deviceSingleton deviceDef
      , tree =
          WithTag locCaller "no-generate"
            $ DeviceInstance locCaller "TODO"
      }
 where
  deviceDef =
    DeviceDefinition
      { deviceName = Name "TODO" "This component has not been memory mapped yet."
      , registers = []
      , defLocation = locHere
      , tags = ["no-generate"]
      }
