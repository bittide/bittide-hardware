-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

{- | Memory Maps for circuits. Provides a way for memory mapped subordinates to
propagate their memory map to the top level, possibly with interconnects merging
multiple memory maps.

The current implementation has a few objectives:

  1. Provide a way for memory mapped peripherals to define their memory layout

  2. Make it possible for designers to define hardware designs without specifying
     exact memory addresses if they can be mapped to arbitrary addresses, while
     not giving up the ability to.

  3. Provide a way to check whether memory maps are valid, i.e. whether
     peripherals do not overlap or exceed their allocated space.

  4. Provide a way to generate human readable documentation

  5. Provide a way to generate data structures for target languages. I.e., if
     a designer instantiates a memory mapped register on type @a@, it should
     be possible to generate an equivalent data structure in Rust or C.
-}
module Protocols.MemoryMap (
  module Protocols.MemoryMap.Check.Normalized,
  Mm,
  Protocols.ToConstBwd,
  Protocols.ToConst,
  getConstBwdAny,
  getMMAny,
  withName,
  withAbsAddr,
  withTag,
  withTags,
  withDeviceTag,
  withDeviceTags,
  withMemoryMap,
  withPrefix,
  unMemmap,
  todoMM,
  ignoreMM,
  Access (..),
  DeviceDefinitions,
  DeviceDefinition (..),
  Name (..),
  NamedLoc (..),
  regType,
  regFieldType,
  regByteSizeC,
  Register (..),
  RegisterType (..),
  MemoryMap (..),
  mergeDeviceDefs,
  deviceSize,
  deviceSingleton,
  locHere,
  locCaller,
  locN,
) where

import Clash.Prelude (
  Eq,
  Integer,
  Maybe,
  NFDataX,
  Natural,
  Num ((+), (-)),
  Ord (max),
  Show (show),
  SimOnly (..),
  String,
  deepErrorX,
  error,
  flip,
  natToNum,
  ($),
  (<>),
 )

import Protocols

import Clash.Class.BitPackC
import Data.Data (Proxy (Proxy))
import GHC.Stack (HasCallStack, SrcLoc, callStack, getCallStack)
import Protocols.MemoryMap.Check.Normalized
import Protocols.MemoryMap.TypeDescription (TypeRef, WithTypeDescription (asTypeRef))

import qualified Data.List as L
import qualified Data.Map.Strict as Map

-- | Abbreviation for a simulation-only 'MemoryMap'
type Mm = SimOnly MemoryMap

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
data NamedLoc a = NamedLoc
  { name :: Name
  , loc :: SrcLoc
  , value :: a
  }
  deriving (Show, Eq)

type DeviceDefinitions = Map.Map DeviceName DeviceDefinition

data MemoryMap = MemoryMap
  { deviceDefs :: DeviceDefinitions
  , tree :: MemoryMapTree
  }
  deriving (Show)

data DeviceDefinition = DeviceDefinition
  { deviceName :: Name
  , registers :: [NamedLoc Register]
  , definitionLoc :: SrcLoc
  , tags :: [String]
  }
  deriving (Show)

deviceSize :: DeviceDefinition -> Integer
deviceSize dev =
  L.foldr
    ( \reg acc ->
        max acc (reg.value.address + regByteSizeC reg.value.fieldType)
    )
    0
    dev.registers

{- | Access modifiers for a register

Later we want to support more modes, see
https://corsair.readthedocs.io/en/latest/regmap.html#bit-field
-}
data Access
  = -- | Managers should only read from this register
    ReadOnly
  | -- | Managers should only write to this register
    WriteOnly
  | -- | Managers can read from and write to this register
    ReadWrite
  deriving (Show, Eq)

{- | Value carrying the type of a register

See 'regType' and 'regTypeSplit' to construct values of this type.
-}
data RegisterType where
  RegisterType :: (WithTypeDescription a, BitPackC a) => Proxy a -> RegisterType

regFieldType :: RegisterType -> TypeRef
regFieldType (RegisterType proxy) = inner proxy
 where
  inner :: forall x. (WithTypeDescription x) => Proxy x -> TypeRef
  inner Proxy = asTypeRef (Proxy @x)

regByteSizeC :: (Num a) => RegisterType -> a
regByteSizeC (RegisterType proxy) = inner proxy
 where
  inner :: forall x a. (BitPackC x, Num a) => Proxy x -> a
  inner Proxy = natToNum @(ByteSizeC x)

regType :: forall a. (WithTypeDescription a, BitPackC a) => RegisterType
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
  (HasCallStack) => String -> Circuit (ToConstBwd Mm, a) b -> Circuit (ToConstBwd Mm, a) b
withName name' (Circuit f) = Circuit go
 where
  go (((), fwdA), bwdB) = ((SimOnly mm', bwdA), fwdB)
   where
    ((SimOnly mm, bwdA), fwdB) = f (((), fwdA), bwdB)
    mm' = mm{tree = WithName locCaller name' mm.tree}

withDeviceTag ::
  (HasCallStack) => String -> Circuit (ToConstBwd Mm, a) b -> Circuit (ToConstBwd Mm, a) b
withDeviceTag tag (Circuit f) = Circuit go
 where
  go (((), fwdA), bwdB) = ((SimOnly mm', bwdA), fwdB)
   where
    ((SimOnly mm, bwdA), fwdB) = f (((), fwdA), bwdB)
    mm' = mm{deviceDefs = newDefs}
    newDefs = case Map.toAscList mm.deviceDefs of
      [(key, def')] ->
        Map.fromAscList
          [
            ( key
            , DeviceDefinition
                { tags = tag : def'.tags
                , deviceName = def'.deviceName
                , registers = def'.registers
                , definitionLoc = def'.definitionLoc
                }
            )
          ]
      [] -> error "`withDeviceTag` called on a tree with no device definitions"
      _ -> error "`withDeviceTag` called on a tree with more than one device definition"

withDeviceTags ::
  (HasCallStack) => [String] -> Circuit (ToConstBwd Mm, a) b -> Circuit (ToConstBwd Mm, a) b
withDeviceTags tags' (Circuit f) = Circuit go
 where
  go (((), fwdA), bwdB) = ((SimOnly mm', bwdA), fwdB)
   where
    ((SimOnly mm, bwdA), fwdB) = f (((), fwdA), bwdB)
    mm' = mm{deviceDefs = newDefs}
    newDefs = case Map.toAscList mm.deviceDefs of
      [(key, def')] ->
        Map.fromAscList
          [
            ( key
            , DeviceDefinition
                { tags = tags' <> def'.tags
                , deviceName = def'.deviceName
                , registers = def'.registers
                , definitionLoc = def'.definitionLoc
                }
            )
          ]
      [] -> error "`withDeviceTags` called on a tree with no device definitions"
      _ -> error "`withDeviceTags` called on a tree with more than one device definition"

withTag ::
  (HasCallStack) => String -> Circuit (ToConstBwd Mm, a) b -> Circuit (ToConstBwd Mm, a) b
withTag tag (Circuit f) = Circuit go
 where
  go (((), fwdA), bwdB) = ((SimOnly mm', bwdA), fwdB)
   where
    ((SimOnly mm, bwdA), fwdB) = f (((), fwdA), bwdB)
    mm' = mm{tree = WithTag locCaller tag mm.tree}

withTags ::
  (HasCallStack) => [String] -> Circuit (ToConstBwd Mm, a) b -> Circuit (ToConstBwd Mm, a) b
withTags tags' (Circuit f) = Circuit go
 where
  go (((), fwdA), bwdB) = ((SimOnly mm', bwdA), fwdB)
   where
    ((SimOnly mm, bwdA), fwdB) = f (((), fwdA), bwdB)
    mm' = mm{tree = tree'}
    tree' = L.foldl (flip (WithTag locCaller)) mm.tree tags'

withAbsAddr ::
  (HasCallStack) => Address -> Circuit (ToConstBwd Mm, a) b -> Circuit (ToConstBwd Mm, a) b
withAbsAddr addr (Circuit f) = Circuit go
 where
  go (((), fwdA), bwdB) = ((SimOnly mm', bwdA), fwdB)
   where
    ((SimOnly mm, bwdA), fwdB) = f (((), fwdA), bwdB)
    mm' = mm{tree = WithAbsAddr locCaller addr mm.tree}

withMemoryMap :: MemoryMap -> Circuit a b -> Circuit (ToConstBwd Mm, a) b
withMemoryMap mm = withConstBwd (SimOnly mm)

withPrefix :: v -> Circuit a b -> Circuit (ToConstBwd v, a) b
withPrefix = withConstBwd

withConstBwd :: forall v a b. v -> Circuit a b -> Circuit (ToConstBwd v, a) b
withConstBwd val (Circuit f) = Circuit go
 where
  go :: (((), Fwd a), Bwd b) -> ((v, Bwd a), Fwd b)
  go (((), fwdA), bwdB) = ((val, bwdA), fwdB)
   where
    (bwdA, fwdB) = f (fwdA, bwdB)

mergeDeviceDefs :: [Map.Map String DeviceDefinition] -> Map.Map String DeviceDefinition
mergeDeviceDefs = L.foldl Map.union Map.empty

getConstBwdAny ::
  (HasCallStack, NFDataX (Fwd a), NFDataX (Bwd b)) => Circuit (ToConstBwd v, a) b -> v
getConstBwdAny (Circuit f) = val
 where
  ((val, _), _) = f (((), deepErrorX "getConstBwdAny0"), deepErrorX "getConstBwdAny1")

getMMAny ::
  (HasCallStack, NFDataX (Fwd a), NFDataX (Bwd b)) =>
  Circuit (ToConstBwd Mm, a) b ->
  MemoryMap
getMMAny circ = let SimOnly memoryMap = getConstBwdAny circ in memoryMap

deviceSingleton :: DeviceDefinition -> DeviceDefinitions
deviceSingleton def' = Map.singleton def'.deviceName.name def'

unMemmap :: Circuit (ToConstBwd Mm, a) b -> Circuit a b
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
  (fn, _) : _ -> error $ "`" L.++ fn L.++ "` needs to be called in a `HasCallStack` context"
  _ ->
    error "`locCaller` needs to be called with at least two levels of `HasCallStack` context"

{- | Return the Nth caller in the call stack. @locN 0@ returns the location of
the call site of @locN@, i.e., @locHere@.
-}
locN :: (HasCallStack) => Integer -> SrcLoc
locN n = go (getCallStack callStack) n
 where
  go [] _ = error "locN: internal error: not enough call stack"
  go ((_, loc) : _) 0 = loc
  go [_] _ =
    error
      $ "locN "
      <> show n
      <> ": should be called with at least "
      <> show n
      <> " levels of `HasCallStack` context."
  go (_ : rest) m = go rest (m - 1)

ignoreMM :: Circuit () (ToConstBwd Mm)
ignoreMM = Circuit $ \_ -> ((), ())

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
      , definitionLoc = locHere
      , tags = ["no-generate"]
      }
