{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Protocols.MemoryMap where

import Clash.Prelude

import Protocols
import Protocols.Wishbone (Wishbone, WishboneMode(Standard), emptyWishboneS2M, emptyWishboneM2S, WishboneM2S (..), WishboneS2M (..))
import GHC.Natural (Natural)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import GHC.Stack (HasCallStack, CallStack, callStack, getCallStack, SrcLoc)
import GHC.Generics
import BitPackC (BitPackC (packC, unpackC, ByteSizeC))

import Protocols.MemoryMap.FieldType (ToFieldType(..), FieldType)

-- | Absolute address or offset in bytes
type Address = Integer

-- | Size in bytes
type Size = Integer

-- | A protocol agnostic wrapper for memory mapped protocols. It provides a way
-- for memory mapped subordinates to propagate their memory map to the top level,
-- possibly with interconnects merging multiple memory maps.
--
-- The current implementation has a few objectives:
--
--   1. Provide a way for memory mapped peripherals to define their memory layout
--
--   2. Make it possible for designers to define hardware designs without specifying
--      exact memory addresses if they can be mapped to arbitrary addresses, while
--      not giving up the ability to.
--
--   3. Provide a way to check whether memory maps are valid, i.e. whether
--      perhipherals do not overlap or exceed their allocated space.
--
--   4. Provide a way to generate human readable documentation
--
--   5. Provide a way to generate data structures for target languages. I.e., if
--      a designer instantiates a memory mapped register on type @a@, it should
--      be possible to generate an equivalent data structure in Rust or C.
--

data BackwardAnnotated (annotation :: Type) (a :: Type)

instance Protocol a => Protocol (BackwardAnnotated annotation a) where
  type Fwd (BackwardAnnotated annotation a) = Fwd a
  type Bwd (BackwardAnnotated annotation a) = (annotation, Bwd a)

type MemoryMapped a = BackwardAnnotated (SimOnly MemoryMap) a
type RegisterMapped a = BackwardAnnotated (SimOnly (Named Register)) a

unAnnotate :: Circuit a (BackwardAnnotated ann a)
unAnnotate = Circuit $ \(fwd, (_, bwd)) -> (bwd, fwd)

annotation' :: (NFDataX (Fwd a), NFDataX (Bwd b)) => Circuit (BackwardAnnotated ann a) b -> ann
annotation' (Circuit f) = ann'
  where
    ((ann', _), _) = f (deepErrorX "")

annotation ::
  (KnownDomain dom, NFDataX (Fwd a), NFDataX (Bwd b)) =>
  ((HiddenClockResetEnable dom) => Circuit (BackwardAnnotated ann a) b)
  -> ann
annotation circ = ann'
  where
    Circuit f = withClockResetEnable clockGen resetGen enableGen circ
    ((ann', _), _) = f (deepErrorX "")

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

-- | Wrapper for \"things\" that have a name and a description. These are used
-- to generate documentation and data structures for target languages.
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

-- | A tree structure that describes the memory map of a device. Its definitions
-- are using non-translatable constructs on purpose: Clash is currently pretty
-- bad at propagating contants properly, so designers should only /produce/
-- memory maps, not rely on constant folding to be able to extract addresses
-- from them to use in their designs.
data MemoryMapTree
  = Interconnect SrcLoc AbsoluteAddress [(Address, Size, MemoryMapTree)]
  | DeviceInstance SrcLoc AbsoluteAddress String DeviceName
  deriving (Show)

data Access
  = ReadOnly
  -- ^ Managers should only read from this register
  | WriteOnly
  -- ^ Managers should only write to this register
  | ReadWrite
  -- ^ Managers can read from and write to this register
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



deviceDef' ::
  forall dom a n addrWidth .
  Circuit
    (MemoryMapped (Wishbone dom 'Standard addrWidth a))
    (Vec n (Wishbone dom 'Standard addrWidth ()))
deviceDef' = undefined

deviceDefinition ::
  forall dom a n addrWidth .
  ( HasCallStack
  , ToFieldType a
  , NFDataX a
  , KnownNat (BitSize a)
  , KnownNat n
  , KnownNat addrWidth
  , HiddenClockResetEnable dom
  , 1 <= n
  ) =>
  String ->
  AbsoluteAddress ->
  Name ->
  Circuit
    (MemoryMapped (Wishbone dom 'Standard addrWidth a))
    (Vec n
      (BackwardAnnotated
        (BitVector addrWidth, SimOnly (NamedLoc Register))
        (Wishbone dom 'Standard addrWidth a)
      )
    )
deviceDefinition instanceName absAddr' deviceDesc = Circuit $ \(m2s, bwds) ->
    let
      annotations = fst <$> bwds
      s2ms = bundle $ snd <$> bwds
      addrs = fst <$> annotations
      regs = (\(_, SimOnly x) -> x) <$> annotations
      memMap = MemoryMap
        { deviceDefs = Map.singleton (name deviceDesc) definition
        , tree = DeviceInstance instanceLoc absAddr' instanceName (name deviceDesc) }
      definition = DeviceDefinition
        { deviceName =  deviceDesc
        , registers = toList regs
        , defLocation = defLoc
        }

      (s2m, m2ss) = go addrs m2s s2ms
    in
    ((SimOnly memMap, s2m), m2ss)

  where
    ((_, defLoc):(_, instanceLoc):_) = getCallStack callStack
    
    go addrs m2s s2ms = (s2m, m2ss)
      where
        active :: Signal dom (Maybe (Index n))
        active = register Nothing newActive
        (unbundle -> (s2m, unbundle -> m2ss, newActive)) = inner (addrRange addrs) <$> m2s <*> s2ms <*> active

    inner ranges m2s@WishboneM2S{..} s2ms active
      | not (busCycle && strobe) = (emptyWishboneS2M, m2ss, Nothing)
      -- no existing transaction, no new transaction
      -- => ERROR, we're in a cycle but couldn't find a subordinate that matches the address
      -- which shouldn't ever happen, but if it does it should be an error!
      | Nothing <- active, Nothing <- newActive = (emptyWishboneS2M { err = True }, m2ss, Nothing)
      -- no previous transaction, but a new one!
      | Nothing <- active, Just idx <- newActive =
          (s2ms !! idx, replace idx m2s m2ss, Just idx)
      -- have an existing transaction, use that one
      | Just idx <- active = (s2ms !! idx, replace idx m2s m2ss, active)
      where
        m2ss = repeat emptyWishboneM2S
        newActive = findIndex (\(start, end) -> addr >= start && addr < end) ranges

deviceReg' ::
  forall dom a addrWidth .
  ( HasCallStack
  , ToFieldType a
  , NFDataX a
  , BitPackC a
  , KnownNat addrWidth
  , HiddenClockResetEnable dom
  ) =>
  String ->
  Access ->
  BitVector addrWidth ->
  a ->
  Circuit
    (BackwardAnnotated
      (BitVector addrWidth, SimOnly (NamedLoc Register))
      (Wishbone dom 'Standard addrWidth (BitVector 32)))
    ()
deviceReg' name' access' addr' def' = Circuit $ \(m2s0, ()) ->
    (((addr', SimOnly reg'), go m2s0), ())
  where
    defBytes :: BitVector 32
    defBytes = resize $ packC def'

    (_, loc):_ = getCallStack callStack
    reg' = (Name
      { name = name'
      , description = ""
      }, loc, Register
        { access = access'
        , address = toInteger addr'
        , fieldType = toFieldType @a
        , fieldSize = snatToInteger (SNat @(ByteSizeC a))
        , reset = Nothing
        }
      )
    go m2s = s2m
      where
        registerVal = register defBytes $ fromMaybe <$> registerVal <*> nextVal
        (unbundle -> (s2m, nextVal)) = inner <$> m2s <*> registerVal

    inner WishboneM2S{..} val
      | not (busCycle && strobe) = (emptyWishboneS2M, Nothing)
      | writeEnable = (emptyWishboneS2M { acknowledge = True }, Just writeData)
      | otherwise = ((emptyWishboneS2M @a) { acknowledge = True, readData = 0 }, Nothing)

deviceReg ::
  forall dom a addrWidth .
  ( HasCallStack
  , ToFieldType a
  , NFDataX a
  , KnownNat addrWidth
  , HiddenClockResetEnable dom
  ) =>
  String ->
  Access ->
  BitVector addrWidth ->
  a ->
  Circuit
    (BackwardAnnotated
      (BitVector addrWidth, SimOnly (NamedLoc Register))
      (Wishbone dom 'Standard addrWidth a))
    ()
deviceReg name' access' addr' def' = Circuit $ \(m2s0, ()) ->
    (((addr', SimOnly reg'), go m2s0), ())
  where
    (_, loc):_ = getCallStack callStack
    reg' = (Name
      { name = name'
      , description = ""
      }, loc, Register
        { access = access'
        , address = toInteger addr'
        , fieldType = toFieldType @a
        , fieldSize = error "use deviceReg' with BitPackC"
        , reset = Nothing
        }
      )
    go m2s = s2m
      where
        registerVal = register def' $ fromMaybe <$> registerVal <*> nextVal
        (unbundle -> (s2m, nextVal)) = inner <$> m2s <*> registerVal

    inner WishboneM2S{..} val
      | not (busCycle && strobe) = (emptyWishboneS2M, Nothing)
      | writeEnable = (emptyWishboneS2M { acknowledge = True }, Just writeData)
      | otherwise = ((emptyWishboneS2M @a) { acknowledge = True, readData = val }, Nothing)


deviceVecField' ::
  forall dom n a addrWidth .
  ( HasCallStack
  , KnownNat n
  , ToFieldType a
  , NFDataX a
  , BitPackC a
  , BitPackC (Vec n a)
  , KnownNat addrWidth
  , HiddenClockResetEnable dom
  ) =>
  String ->
  Access ->
  BitVector addrWidth ->
  SNat n ->
  a ->
  Circuit
    (BackwardAnnotated
      (BitVector addrWidth, SimOnly (NamedLoc Register))
      (Wishbone dom 'Standard addrWidth (BitVector 32)))
    ()
deviceVecField' name' access' addr' _size def' = Circuit $ \(m2s0, ()) ->
    (((addr', SimOnly reg'), go m2s0), ())
  where
    (_, loc):_ = getCallStack callStack

    defBytes :: BitVector 32
    defBytes = resize $ packC def'

    reg' = (Name
      { name = name'
      , description = ""
      }, loc, Register
        { access = access'
        , address = toInteger addr'
        , fieldType = toFieldType @(Vec n a)
        , fieldSize = snatToInteger (SNat @(ByteSizeC (Vec n a)))
        , reset = Nothing
        }
      )
    go m2s = s2m
      where
        registerVal = register defBytes $ fromMaybe <$> registerVal <*> nextVal
        (unbundle -> (s2m, nextVal)) = inner <$> m2s <*> registerVal

    inner WishboneM2S{..} val
      | not (busCycle && strobe) = (emptyWishboneS2M, Nothing)
      | writeEnable = (emptyWishboneS2M { acknowledge = True }, Just writeData)
      | otherwise = ((emptyWishboneS2M @a) { acknowledge = True, readData = val }, Nothing)


deviceVecField ::
  forall dom n a addrWidth .
  ( HasCallStack
  , KnownNat n
  , ToFieldType a
  , NFDataX a
  , KnownNat addrWidth
  , HiddenClockResetEnable dom
  ) =>
  String ->
  Access ->
  BitVector addrWidth ->
  SNat n ->
  a ->
  Circuit
    (BackwardAnnotated
      (BitVector addrWidth, SimOnly (NamedLoc Register))
      (Wishbone dom 'Standard addrWidth a))
    ()
deviceVecField name' access' addr' _size def' = Circuit $ \(m2s0, ()) ->
    (((addr', SimOnly reg'), go m2s0), ())
  where
    (_, loc):_ = getCallStack callStack
    reg' = (Name
      { name = name'
      , description = ""
      }, loc, Register
        { access = access'
        , address = toInteger addr'
        , fieldType = toFieldType @(Vec n a)
        , fieldSize = error "use deviceVecField' with BitPackC"
        , reset = Nothing
        }
      )
    go m2s = s2m
      where
        registerVal = register def' $ fromMaybe <$> registerVal <*> nextVal
        (unbundle -> (s2m, nextVal)) = inner <$> m2s <*> registerVal

    inner WishboneM2S{..} val
      | not (busCycle && strobe) = (emptyWishboneS2M, Nothing)
      | writeEnable = (emptyWishboneS2M { acknowledge = True }, Just writeData)
      | otherwise = ((emptyWishboneS2M @a) { acknowledge = True, readData = val }, Nothing)


withPrefix ::
  HasCallStack =>
  BitVector n ->
  Circuit (BackwardAnnotated ann a) b ->
  Circuit (BackwardAnnotated (BitVector n, ann) a) b
withPrefix p (Circuit f) = Circuit $ \(fwd, bwd) ->
    let
      ((ann, bwd'), fwd') = f (fwd, bwd)
    in (((p, ann), bwd'), fwd')


interconnect ::
  forall dom addrWidth a n.
  ( HasCallStack
  , KnownNat n
  , KnownNat addrWidth
  , NFDataX a
  , KnownNat (BitSize a)
  , (CLog 2 n <= addrWidth)
  , 1 <= n
  , KnownNat (addrWidth - CLog 2 n)
  ) =>
  AbsoluteAddress ->
  Circuit (MemoryMapped (Wishbone dom 'Standard addrWidth a))
  (Vec n (BackwardAnnotated (BitVector (CLog 2 n), SimOnly MemoryMap) (Wishbone dom 'Standard (addrWidth - CLog 2 n) a)))
interconnect absAddr' = Circuit go
  where
    (_, loc):_ = getCallStack callStack

    go ::
      ( Signal dom (WishboneM2S addrWidth (Div (BitSize a + 7) 8) a)
      , Vec n ((BitVector (CLog 2 n), SimOnly MemoryMap), Signal dom (WishboneS2M a)))
      ->
      ( (SimOnly MemoryMap, Signal dom (WishboneS2M a))
      , Vec n (Signal dom (WishboneM2S (addrWidth - CLog 2 n) (Div (BitSize a + 7) 8) a)))
    go (m2s, unzip -> (unzip -> (prefixes, mmaps), s2ms)) = ((SimOnly memoryMap, s2m), unbundle m2ss)
      where

        memoryMap = MemoryMap
          { deviceDefs = unionAll (deviceDefs . unSim <$> toList mmaps)
          , tree = Interconnect loc absAddr' (toList descs) }


        unionAll :: [Map.Map String DeviceDefinition] -> Map.Map String DeviceDefinition
        unionAll = List.foldl Map.union Map.empty

        size = maxBound :: BitVector (addrWidth - CLog 2 n)
        relAddrs = prefixToAddr <$> prefixes 
        descs = zip3 relAddrs (repeat (toInteger size)) (tree . unSim <$> mmaps)
        unSim (SimOnly x) = x

        prefixToAddr :: BitVector (CLog 2 n) -> Address
        prefixToAddr prefix = toInteger prefix `shiftL` fromInteger shift'
          where
            shift' = snatToInteger $ SNat @(addrWidth - CLog 2 n) 

        (unbundle -> (s2m, m2ss)) = inner prefixes <$> m2s <*> bundle s2ms

    inner ::
      Vec n (BitVector (CLog 2 n)) ->
      WishboneM2S addrWidth (Div (BitSize a + 7) 8) a ->
        Vec n (WishboneS2M a) ->
        (WishboneS2M a, Vec n (WishboneM2S (addrWidth - CLog 2 n) (Div (BitSize a + 7) 8) a))
    inner prefixes m2s@WishboneM2S{..} s2ms
      | not (busCycle && strobe) = (emptyWishboneS2M, m2ss)
      -- no valid index selected
      | Nothing <- selected = (emptyWishboneS2M { err = True }, m2ss)
      | Just idx <- selected = (s2ms !! idx, replace idx m2sStripped m2ss)

      where
        m2ss = repeat emptyWishboneM2S
        m2sStripped = m2s { addr = internalAddr }
        (compIdx, internalAddr) = split @_ @(CLog 2 n) @(addrWidth - CLog 2 n) addr
        selected = elemIndex compIdx prefixes



addrRange :: forall a n . (Bounded a) => Vec n a -> Vec n (a, a)
addrRange startAddrs = zip startAddrs bounds
  where
    bounds = startAddrs <<+ maxBound


{-
-- | Remove memory map information from a circuit
unMemoryMapped :: Circuit a (MemoryMapped a)
unMemoryMapped = Circuit $ \(fwdA, (_mm, bwdA)) -> (bwdA, fwdA)

-- | Add memory map information to a circuit
memoryMapped :: Named MemoryMap -> Circuit (MemoryMapped a) a
memoryMapped mm = Circuit $ \(fwdA, bwdA) -> ((SimOnly mm, bwdA), fwdA)

-- | Extract the memory map from a circuit. Note that the circuit needs to be
-- lazy enough: if the memory map depends on any signals, this function will
-- error.
extractMemoryMap :: (NFDataX (Fwd a), NFDataX (Bwd b)) => Circuit (MemoryMapped a) b -> Named MemoryMap
extractMemoryMap (Circuit f) = 
  let ((SimOnly mm, _), _) = f (deepErrorX "") in
  mm

-}