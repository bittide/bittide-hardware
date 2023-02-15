-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Shared where

import Clash.Prelude

import Clash.Hedgehog.Sized.Unsigned
import Clash.Signal.Internal

import Data.Constraint (Dict(Dict))
import Data.Constraint.Nat.Extra (divWithRemainder)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import GHC.Stack (HasCallStack)
import Hedgehog
import Numeric.Natural
import Protocols (toSignals)
import Protocols.Axi4.Stream
import Protocols.Wishbone as Wb
import Protocols.Wishbone.Standard.Hedgehog (validatorCircuit)

import Bittide.Calendar
import Bittide.SharedTypes

import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty
import qualified GHC.TypeNats as TypeNats
import qualified Hedgehog.Range as Range


data IsInBounds a b c where
  InBounds :: (a <= b, b <= c) => IsInBounds a b c
  NotInBounds :: IsInBounds a b c

deriving instance Show (IsInBounds a b c)

data SomeSNat where
  SomeSNat :: forall m. SNat m -> SomeSNat

-- Like 'TypeNats.someNatVal', but generates 'SomeSNat'
someSNat :: Natural -> SomeSNat
someSNat n =
  case TypeNats.someNatVal n of
    SomeNat p ->
      SomeSNat (snatProxy p)

-- | Returns 'InBounds' when a <= b <= c, otherwise returns 'NotInBounds'.
isInBounds :: SNat a -> SNat b -> SNat c -> IsInBounds a b c
isInBounds a b c = case (compareSNat a b, compareSNat b c) of
  (SNatLE, SNatLE) -> InBounds
  _ -> NotInBounds

-- | We use a custom generator for BitVector's because the current Clash implementation
-- uses genVec which is slow.
genDefinedBitVector :: forall n m . (MonadGen m, KnownNat n) => m (BitVector n)
genDefinedBitVector = pack <$> genUnsigned Range.constantBounded

-- | Single datatype to represent successful and unsuccessful Wishbone transactions.
data Transaction addrW selWidth a
  = WriteSuccess (WishboneM2S addrW selWidth a) (WishboneS2M a)
  | ReadSuccess (WishboneM2S addrW selWidth a) (WishboneS2M a)
  | Error (WishboneM2S addrW selWidth a)
  | Retry (WishboneM2S addrW selWidth a)
  | Stall (WishboneM2S addrW selWidth a)
  | Ignored (WishboneM2S addrW selWidth a)
  | Illegal (WishboneM2S addrW selWidth a) (WishboneS2M a)
   deriving (Generic)

-- | Show Instance for 'Transaction' that hides fields irrelevant for the transaction.
instance (KnownNat addrW, Show a) => Show (Transaction addrW selWidth a) where
  show (WriteSuccess WishboneM2S{..} _)
    = "WriteSuccess: (addr: "
    <> show addr
    <> ", writeData:"
    <> show writeData
    <> ")"
  show (ReadSuccess WishboneM2S{..} WishboneS2M{..})
    = "ReadSuccess: ("
    <> show addr
    <> ", "
    <> show readData
    <> ")"
  show (Error _) = "Error"
  show (Retry _) = "Retry"
  show (Stall _) = "Stall"
  show (Illegal _ _) = "Illegal"
  show (Ignored _) = "Ignored"

-- | Show Instance for 'Transaction' that hides fields irrelevant for the transaction.
instance (KnownNat addrW, KnownNat selWidth, ShowX a) => ShowX (Transaction addrW selWidth a) where
  showX (WriteSuccess WishboneM2S{..} _)
    = "WriteSuccess: (addr: "
    <> showX addr
    <> ", writeData:"
    <> showX writeData
    <> ")"
  showX (ReadSuccess WishboneM2S{..} WishboneS2M{..})
    = "ReadSuccess: ("
    <> showX addr
    <> ", "
    <> showX readData
    <> ")"
  showX (Error _) = "Error"
  showX (Retry _) = "Retry"
  showX (Stall _) = "Stall"
  showX (Illegal _ _) = "Illegal"
  showX (Ignored _) = "Ignored"

-- | Equality instance for 'Transaction' that only looks at the fields relevant for the
-- transaction (e.g. 'writeData' is not relevant during a read transaction).
instance (KnownNat addrW, KnownNat selWidth, Eq a, NFDataX a) =>
  Eq (Transaction addrW selWidth a) where
  (WriteSuccess mA _) == (WriteSuccess mB _) =
    checkField "addr" addr mA mB &&
    checkField "buSelect" busSelect mA mB &&
    checkField "writeData" writeData mA mB
  (ReadSuccess mA sA) == (ReadSuccess mB sB) =
    checkField "addr" addr mA mB &&
    checkField "busSelect" busSelect mA mB &&
    checkField "readData" readData sA sB
  (Error _) == (Error _) = True
  (Retry _) == (Retry _) = True
  (Stall _) == (Stall _) = True
  (Illegal _ _) == (Illegal _ _) = True
  (Ignored _) == (Ignored _) = True
  _ == _ = False

checkField :: (NFDataX a, Eq a) => String -> (t -> a) -> t -> t -> Bool
checkField str f a b
  | hasUndefined (f a) || hasUndefined (f b) =
    deepErrorX ("checkField: " <> str <> ", is undefined for one of the transactions.")
  | otherwise = f a == f b

-- | Convert a 'RamOp' to 'WishboneM2S'
ramOpToWb
  ::
  forall addrW i a .
  ( NFDataX a
  , KnownNat addrW
  , KnownNat (BitSize a)
  , KnownNat i
  , 1 <= i)
  => RamOp i a
  -> WishboneM2S addrW (DivRU (BitSize a) 8) a

ramOpToWb (RamRead i) = (emptyWishboneM2S @addrW @a)
  { addr = resize (pack i)
  , busCycle = True
  , strobe = True
  , busSelect = maxBound}

ramOpToWb (RamWrite i a) = (emptyWishboneM2S @addrW @a)
  { addr = resize (pack i)
  , busCycle = True
  , strobe = True
  , busSelect = maxBound
  , writeEnable = True
  , writeData = a}

ramOpToWb RamNoOp = emptyWishboneM2S @addrW @a

-- | Consumes a list of 'WishboneM2S' requests and a list of 'WishboneS2M' responses
-- and transforms them to a list of 'Transaction'.
wbToTransaction
  :: (Eq a, KnownNat addressWidth, KnownNat selWidth, ShowX a)
  => [WishboneM2S addressWidth selWidth a]
  -> [WishboneS2M a]
  -> [Transaction addressWidth selWidth a]
wbToTransaction (m@WishboneM2S{..}:restM) (s@WishboneS2M{..}:restS)
  | not strobe || not busCycle                        = nextTransaction
  | hasMultipleTrues [acknowledge, err, retry, stall] = Illegal m s      : nextTransaction
  | acknowledge && writeEnable                        = WriteSuccess m s : nextTransaction
  | acknowledge                                       = ReadSuccess m s  : nextTransaction
  | err                                               = Error m          : nextTransaction
  | retry                                             = Retry m          : nextTransaction
  | stall                                             = Stall m          : nextTransaction
  | Wb.busCycle nextM && Wb.strobe nextM              = nextTransaction
  | otherwise                                         = Ignored m        : nextTransaction
 where
  nextM = L.head restM
  nextTransaction = wbToTransaction restM restS
  hasMultipleTrues :: [Bool] -> Bool
  hasMultipleTrues [] = False
  hasMultipleTrues [_] = False
  hasMultipleTrues (b0:(b1:brest))
    | b0 && b1  = True
    | otherwise = hasMultipleTrues ((b0 || b1) : brest)

wbToTransaction _ _ = []

-- | Consumes a list of 'RamOp's and a list of corresponding results @a@ and transforms
-- them into a list of 'Transaction's.
ramOpToTransaction
  ::
  forall i addrW a .
  ( 1 <= i
  , KnownNat addrW
  , KnownNat (BitSize a)
  , KnownNat i
  , NFDataX a
  )
  => RamOp i a
  -> a
  -> Maybe (Transaction addrW (DivRU (BitSize a) 8) a)
ramOpToTransaction ramOp response = case ramOp of
  RamNoOp       -> Nothing
  RamRead _     -> Just (ReadSuccess (ramOpToWb ramOp) slaveResponse)
  RamWrite _ _  -> Just (WriteSuccess (ramOpToWb ramOp) slaveResponse)
 where
  slaveResponse = (emptyWishboneS2M @a) {acknowledge = True, readData = response}

validateWb ::
  forall dom aw bs.
  (HasCallStack, HiddenClockResetEnable dom, KnownNat aw, KnownNat bs) =>
  Signal dom (WishboneM2S aw bs (Bytes bs)) ->
  Signal dom (WishboneS2M (Bytes bs)) ->
  (Signal dom (WishboneM2S aw bs (Bytes bs)), Signal dom (WishboneS2M (Bytes bs)))
validateWb m2s0 s2m0 = (m2s1, s2m1)
 where
  validate = toSignals $ validatorCircuit @dom @aw @(Bytes bs)
  (s2m1, m2s1) =
    case divWithRemainder @bs @8 @7 of
      Dict ->
        validate (m2s0, s2m0)

-- | Satisfies implicit control signal constraints by using default values.
wcre :: KnownDomain dom => (HiddenClockResetEnable dom => r) -> r
wcre = withClockResetEnable clockGen resetGen enableGen

-- | Make any @a@ into a non-repeating `ValidEntry` without repetition bits.
nonRepeatingEntry :: a -> ValidEntry a 0
nonRepeatingEntry a = ValidEntry{veEntry = a, veRepeat = 0}

-- | Converts a list of elements into a signal of elements. Unlike 'fromList'
-- it also takes a clock, reset, and enable. When the reset is asserted, it
-- will insert the first value of the given list. When the enable is deasserted,
-- elements are repeated. As usual, the reset takes precedence over the enable.
--
-- __NB__: Not translatable to HDL
fromListWithResetAndEnable ::
  forall dom a .
  (HiddenClockResetEnable dom, NFDataX a, Show a) =>
  NonEmpty a ->
  Signal dom a
fromListWithResetAndEnable inp =
  go
    (fromEnable hasEnable)
    (unsafeToHighPolarity hasReset)
    inpAsSignal

 where
  eolError = deepErrorX "fromListWithResetAndEnable: end of list"
  inpAsSignal = fromList (NonEmpty.toList inp <> L.repeat eolError)

  go :: Signal dom Bool -> Signal dom Bool -> Signal dom a -> Signal dom a
  go (ena :- enables) (rst :- resets) (x :- xs) =
    output :- go enables resets remaining
   where
    output
      | rst       = NonEmpty.head inp
      | otherwise = x
    remaining
      | rst       = inpAsSignal
      | ena       = xs
      | otherwise = x :- xs

-- | A simple simulation-only driver that receives a list of Wishbone master requests
-- alongside the slave bus and drives the master bus. The driver presents the next
-- request after the current request has been terminated by `acknowledge`, `retry` or `err`.
wishboneSimDriver ::
  (HiddenClockResetEnable dom, KnownNat addrW, KnownNat (BitSize a), NFDataX a, ShowX a) =>
  [WishboneM2S addrW (Regs a 8) a] ->
  Signal dom (WishboneS2M a) ->
  Signal dom (WishboneM2S addrW (Regs a 8) a)
wishboneSimDriver inputList wbS2M = wbM2S
 where
  wbM2S = andEnable (getEna <$> wbM2S <*> wbS2M) $ fromListWithResetAndEnable
    (NonEmpty.fromList $ inputList <> L.repeat emptyWishboneM2S)
  getEna WishboneM2S{..} WishboneS2M{..} =
    not (busCycle && strobe) || (busCycle && strobe && (acknowledge || err || retry))

-- | A simple simulation-only driver that receives a list of Axi4Stream master operations
-- alongside the slave bus and drives the master bus. The driver presents the next
-- request after the current request has been terminated by accepted by `_tready`.
axi4StreamSimDriver ::
  (HiddenClockResetEnable dom, KnownAxi4StreamConfig conf, NFDataX userType, Show userType) =>
  [Maybe (Axi4StreamM2S conf userType)] ->
  Signal dom Axi4StreamS2M ->
  Signal dom (Maybe (Axi4StreamM2S conf userType))
axi4StreamSimDriver inputList axisS2M = axisM2S
 where
  axisM2S = andEnable (getEna <$> axisM2S <*> axisS2M) $ fromListWithResetAndEnable
    (NonEmpty.fromList $ inputList <> L.repeat Nothing)
  getEna axi4StreamM2S Axi4StreamS2M{..} = isNothing axi4StreamM2S || _tready
