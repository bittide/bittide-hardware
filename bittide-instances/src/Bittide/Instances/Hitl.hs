-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Bittide.Instances.Hitl
  ( -- * Test Names
    KnownTests(..)
    -- * HITL Type Interface
  , HitltConfig
  , SimpleTest(..)
  , SimpleTests(..)
    -- * HITL VIO Interface
  , TestDone
  , TestSuccess
  , vioHitlt
    -- * Test Selection
  , Selected
  , testActive
  , testConfig
    -- * Type Families
  , TestCount
  , TestIndex
  ) where

import Clash.Explicit.Prelude

import Data.Kind (Constraint)
import Data.Aeson (FromJSON)
import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import Data.Type.Bool (If, type (||))
import Data.Type.Equality ((:~:)(..), type (==))
import GHC.Stack (HasCallStack)
import GHC.TypeLits.Compare ((:<=?)(..))
import GHC.TypeLits.Witnesses ((%<=?))
import qualified GHC.TypeLits.Witnesses as TLW (SNat(..))

import Clash.Cores.Xilinx.VIO (VIO, vioProbe)

-- | A type family for getting the length of a symbols list.
type family TestCount
  (testnames :: [Symbol])
  :: Nat
 where
  TestCount '[]      = 0
  TestCount (_ : xr) = 1 + TestCount xr

-- | A type family to constrain that a list of symbols contains a
-- particular element.
type family IsMember
   (testnames :: [Symbol])
   (test :: Symbol)
   :: Constraint
 where
  IsMember xs x = IsMember# xs x xs

-- | A type family helper for passing an accumulator to the failing
-- type check to produce a more useful type error, in case the
-- constraint is not satisfied.
type family IsMember#
  (testnames :: [Symbol])
  (x :: Symbol)
  (xs :: [Symbol])
  :: Constraint
 where
  IsMember# all y (x : xr) = If (x == y) (0~0) (IsMember# all y xr)
  -- fail on the 'Member' type family, which hides the base case of
  -- the recursion
  IsMember# all y '[] = Member y all

-- | A type family helper, which cannot be satisfied. It is only used
-- for displaying a useful type error, in case 'IsMember` fails.
type family Member
  (test :: Symbol)
  (testnames :: [Symbol])
  :: Constraint

-- | A type family to constrain that a list of symbols is empty or
-- contains a particular element.
type family IsMemberOrEmpty
  (testnames :: [Symbol])
  (test :: Symbol)
  :: Constraint
 where
  IsMemberOrEmpty '[]    _ = (0~0)
  IsMemberOrEmpty (x:xr) y = IsMember (x:xr) y

-- | A type family for getting the index of an element of a symbols
-- list.
type family TestIndex
  (testnames :: [Symbol])
  (test :: Symbol)
  :: Nat
 where
  TestIndex (x : xr) y = If (x == y) 0 (1 + TestIndex xr y)

-- | A type family to constrain that a list of symbols contains no
-- duplicates.
type family IsNubList
  (testnames :: [Symbol])
  :: Constraint
 where
  IsNubList xs = IsNubList# xs xs

-- | A type family helper to
type family IsNubList#
  (testnames :: [Symbol])
  (xs :: [Symbol])
  :: Constraint
 where
  IsNubList# xs '[] = 0~0
  -- fail on the 'NubList' type family, which hides the base case of
  -- the recursion
  IsNubList# xs (x : xr) = If (Member# x xr) (NubList xs) (IsNubList# xs xr)

-- | A type family helper, which cannot be satisfied. It is only used
-- for displaying a useful type error, in case 'IsNubList` fails.
type family NubList
  (xs :: [Symbol])
  :: Constraint

-- | Boolean version of 'IsMember'.
type family Member#
  (y :: Symbol)
  (xs :: [Symbol])
  :: Bool
 where
  Member# y (x : xr) = y == x || Member# y xr
  Member# y '[] = 'False

-- | Extension of the 'KnownSymbol' constraint class towards lists of
-- symbols.
class KnownTests (xs :: [Symbol]) where
  -- | Reifies the symbol list to a 'String' vector of known size.
  testNames :: proxy xs -> Vec (TestCount xs) String

instance KnownTests '[] where
  testNames _ = Nil
instance (KnownSymbol x, KnownTests xr) => KnownTests (x : xr) where
  testNames _ = symbolVal (Proxy @x) :> testNames (Proxy @xr)

-- | A type family to constrain a list of symbols to be non-empty,
-- except the 'SimpleTest' type is passed as the first argument. This
-- allows 'SimpleTest' to be the only special type without any test
-- names.
type family NonEmpty a (xs :: [Symbol]) :: Constraint where
  NonEmpty SimpleTest _  = 0~0
  NonEmpty _          xs = NonEmptyList xs

-- | A type family helper to hide the first argument from 'NonEmpty'
-- to produce better type errors.
type family NonEmptyList (xs :: [Symbol]) :: Constraint where
  NonEmptyList (_:_) = 0~0

-- | A class constraint for the identification of type interfaces of
-- HITL test VIOs. The class assigns a non-empty list of unique
-- symbols to a type, which identify the different test cases using
-- that type interface.
--
-- Type instances of that class must also have a 'BitPack' instance,
-- to be passed via the VIOs, and a 'FromJSON' instance, to be read
-- from a YAML configuration file. Moreover, a 'Default' instance must
-- exist, to provide a fallback in case no (or only a partial) YAML
-- configuration is used.
class
  ( KnownTests testnames
  , IsNubList testnames
  , NonEmpty a testnames
  , BitPack a, FromJSON a, Default a
  ) =>
  HitltConfig a (testnames :: [Symbol]) | a -> testnames

-- | A type synonym for choosing indices on the basis of symbol lists.
type Selected (testnames :: [Symbol]) = Index (TestCount testnames)

-- | A type for simple tests consisting only of a single unique test
-- case and needing no configuration data to be passed via the VIOs.
--
-- This type is special, as it internally maps to the empty list of
-- test names (being the only type, for which this is possible). This
-- allows 'SimpleTest' to be independent of the actual test name being
-- used in the YAML configuration. In the same fashion, tests without
-- a dedicated YAML configuration file also fall back to the
-- 'SimpleTest' type by default.
data SimpleTest = SimpleTest
  deriving (Generic, NFDataX, BitPack, FromJSON, Default)

instance HitltConfig SimpleTest '[]

-- | A type for simple tests consisting of multiple test cases, but
-- with no need of any configuration data to be passed via the VIOs.
data SimpleTests (testnames :: [Symbol]) = SimpleTests
  deriving (Generic, NFDataX, BitPack, FromJSON, Default)

instance
  ( KnownTests testnames
  , IsNubList testnames
  , NonEmptyList testnames
  ) => HitltConfig (SimpleTests testnames) testnames

-- | Test completion is signaled via raising 'TestDone'.
type TestDone = Bool

-- | Test success is signaled via raising 'TestSuccess'.
type TestSuccess = Bool

-- | The main interface for instantiating a HITL VIO bridge returning
-- a @Maybe (Selected testnames, a)@ signal. The signal captures a
-- 'Just' value during test activation, which selects the currently
-- active test case via the first component, while the second
-- component offers the corresponding configuration data for the test.
vioHitlt ::
  forall a dom b n testnames. HasCallStack =>
  (KnownDomain dom, KnownNat n, HitltConfig a testnames) =>
  (VIO dom b (Maybe (Selected testnames, a))) =>
  Vec n String ->
  -- ^ A vector for naming additional input probes to the VIO.
  Clock dom ->
  Signal dom TestDone ->
  -- ^ Input signal to the VIO, which must raise on test completion.
  Signal dom TestSuccess ->
  -- ^ Input signal to the VIO, which must raise on test success.
  b
  -- ^ User defined input signals + the VIO output of type
  -- @(Maybe (Selected testnames, a))@
vioHitlt inputNames clk testDone testSuccess =
  setName @"vioHitlt" $ vioProbe
    ("probe_test_done" :> "probe_test_success" :> inputNames)
    ("probe_test_start" :> Nil)
    Nothing
    clk
    testDone
    testSuccess

-- | Converts the output of 'vioHitlt' into a Boolean signal, which
-- turns active during the execution of a particular test case.
--
-- If multiple tests cases are present then the particular test gets
-- selected via passing the name of the test case as the first type
-- argument using type applications. Otherwise, the unique test case
-- gets selected automatically and no explicit test argument is
-- required.
testActive ::
  forall test dom a testnames. HasCallStack =>
  KnownDomain dom =>
  HitltConfig a testnames =>
  IsMemberOrEmpty testnames test =>
  KnownNat (TestCount testnames) =>
  If (2 <=? TestCount testnames) (KnownSymbol test) (0~0) =>
  If (2 <=? TestCount testnames) (KnownNat (TestIndex testnames test)) (0~0) =>
  Signal dom (Maybe (Selected testnames, a)) ->
  -- ^ The output of 'vioHitlt'.
  Signal dom Bool
  -- ^ The test enable, either being unique or being selected via
  -- the @test@ name.
testActive = fmap $ case TLW.SNat @2 %<=? TLW.SNat @(TestCount testnames) of
  LE Refl -> maybe False ((== (natToNum @(TestIndex testnames test))) . fst)
  NLE {}  -> isJust

-- | Projects the output of 'vioHitlt' to a particular test case,
-- which is only presented during the execution of a particular
-- test.
--
-- If multiple tests cases are present then the particular test gets
-- selected via passing the name of the test case as the first type
-- argument using type applications. Otherwise, the unique test case
-- gets selected automatically and no explicit test argument is
-- required.
testConfig ::
  forall test dom a testnames. HasCallStack =>
  KnownDomain dom =>
  HitltConfig a testnames =>
  IsMemberOrEmpty testnames test =>
  KnownNat (TestCount testnames) =>
  If (2 <=? TestCount testnames) (KnownSymbol test) (0~0) =>
  If (2 <=? TestCount testnames) (KnownNat (TestIndex testnames test)) (0~0) =>
  Signal dom (Maybe (Selected testnames, a)) ->
  -- ^ The output of 'vioHitlt'.
  Signal dom (Maybe a)
  -- ^ The configuration data, either being unique or being selected
  -- via the @test@ name.
testConfig = fmap $ case TLW.SNat @2 %<=? TLW.SNat @(TestCount testnames) of
  NLE {}  -> (snd <$>)
  LE Refl ->
    let selected = natToNum @(TestIndex testnames test)
     in (>>= (\(i, x) -> if i == selected then return x else Nothing))
