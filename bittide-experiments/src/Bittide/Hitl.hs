-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | Tooling to define hardware-in-the-loop (HITL) tests. HITL tests in the
Bittide project involve FPGA designs that incorporate a
[VIO](https://www.xilinx.com/products/intellectual-property/vio.html) to
interface with the HITL test controller. It is used to start tests and
communicate test statusses. In practice, developers writing HITL tests should
make sure to do two things:

  1. They should incorporate a HITL VIO in their design. The HITL test controller
     expects such a VIO to have at minimum an output probe named
     @probe_test_start@ and input probes named @probe_test_done@ and
     @probe_test_success@, all of type 'Bool'. See 'hitlVio' and
     'hitlVioBool' for examples. Additional probes could be added when needed
     for a specific test.

  2. They should define the hardware targets to run the tests against
     (multiple FPGAs, or just one), and with which inputs/parameters the
     tests should be run. See 'HitlTests' for examples, together with it's
     convenience functions 'testsFromEnum', 'noConfigTest', 'allFpgas', and
     'singleFpga'.

Tests are collected in @Bittide.Instances.Hitl.Tests@. The command line
utility at @bittide-tools\/hitl\/config-gen\/Main.hs@ can create YAML
configuration files that can be processed by @HardwareTest.tcl@, and in turn
configure hardware targets appropriately.

=== __Manual test definition__
If you cannot reasonably use `HitlTests` to define your tests, you can manually
write a HITL test configuration file. This file should be a YAML file as specified in
@HardwareTest.tcl@. In order for Shake to find it, it must still be defined
in @Bittide.Instances.Hitl.Tests@, including the definition using @loadConfig@. This will load
the configuration from a file in @bittide-instances\/data\/test_configs@.

=== __Flow overview__

  1. User calls @shake \<binder\>:test@ to run HITL tests.
  2. Shake calls @cabal run bittide-instances:hitl write \<binder\>@ to generate
     a HITL configuration for @\<binder\>@. This will write a file @\<binder\>.yml@
     to @_build/hitl@.
  3. Shake builds a bitstream, programs the FPGA, and runs the HITL tests using
     the configuration file and @HardwareTest.tcl@.
-}
module Bittide.Hitl (
  HitlTests,
  HitlTestsWithPostProcData,
  MayHavePostProcData (..),
  NoPostProcData (..),
  OutProbes,
  FpgaIndex,
  TestName,

  -- * Test construction convenience functions
  allFpgas,
  singleFpga,
  testsFromEnum,
  noConfigTest,

  -- * Test definition
  Done,
  Success,
  hitlVio,
  hitlVioBool,

  -- * Packing
  packAndEncode,
)
where

import Prelude

import Clash.Prelude (
  BitPack (BitSize),
  Index,
  KnownDomain,
  Vec (Nil, (:>)),
  natToInteger,
  pack,
 )

import Clash.Cores.Xilinx.VIO (vioProbe)

import Data.Aeson (ToJSON (toJSON), Value (Number), object, (.=))
import Data.Aeson.Encode.Pretty (
  Config (..),
  NumberFormat (..),
  defConfig,
  encodePretty',
 )
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Exts (IsList (fromList, toList))
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Clash.Prelude qualified as P
import Clash.Sized.Internal.BitVector qualified as BitVector
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Data.Map qualified as Map
import Data.Text qualified as Text

{- | FPGA index pointing to a specific FPGA in the Bittide demo rig. This will be
replaced by proper device identifiers in the future.
-}
type FpgaIndex = Index 8

type TestName = Text

{- | A collection of (named) tests that should be performed with hardware in the
loop. Each test defines what data a specific FPGA should receive (see "OutProbes").
Furthermore, some additional data can be provided, if required by subsequent
post-processing steps (which must have a 'ToJSON' instance).

=== __Example: Test without configuration__
A test that runs for all FPGAs, and does not require any input:

> tests :: HitlTests ()
> tests = noConfigTest allFpgas

This must be accompanied by a @hitlVioBool@ in the design.

=== __Example: Test based on an enum__
A test that runs for each constructor of an enum:

> data ABC = A | B | C
>
> tests :: HitlTests ABC
> tests = testsFromEnum allFpgas

This must be accompanied by a @hitlVio \@ABC@ in the design.

=== __Example: Test with custom configuration and no post processing data__
A test that runs on specific FPGAs, and requires a (hypothetical) 8-bit number
indicating the \"number of stages\" to be set on each FPGA:

> type NumberOfStages = Unsigned 8
>
> tests :: HitlTests NumberOfStages
> tests = Map.fromList
>   [ ( "Twelve stages on FPGA 2 and 5"
>     , ( [ (2, 12)
>         , (5, 12)
>         ]
>       , NoPostProcData
>       )
>     )
>   , ( "Six stages on FPGA 3, seven on FPGA 4"
>     , ( [ (3, 6)
>         , (4, 7)
>         ]
>       , NoPostProcData
>       )
>     )
>   ]

This must be accompanied by a @hitlVio \@NumberOfStages@ in the design.

=== __Example: Test with custom configuration and post processing data__
A test that runs on specific FPGAs, and requires a (hypothetical) 8-bit number
indicating the \"number of stages\" to be set on each FPGA. Additionally,
some 'Int' constant gets fixed for each test, which will be written to
the generated config files, but is not passed to the HITL test:

> type NumberOfStages = Unsigned 8
>
> tests :: HitlTests NumberOfStages Int
> tests = Map.fromList
>   [ ( "Twelve stages on FPGA 2 and 5"
>     , ( [ (2, 12)
>         , (5, 12)
>         ]
>       , 42
>       )
>     )
>   , ( "Six stages on FPGA 3, seven on FPGA 4"
>     , ( [ (3, 6)
>         , (4, 7)
>         ]
>       , 13
>       )
>     )
>   ]

This must be accompanied by a @hitlVio \@NumberOfStages@ in the design.
-}
type HitlTestsWithPostProcData a b = Map TestName (OutProbes a, b)

-- | The type synonym for tests without additional post processing data.
type HitlTests a = HitlTestsWithPostProcData a NoPostProcData

{- | A list of values to be driven by output probes of VIO core instances on
specific FPGAs. See convenience methods 'allFpgas' and 'singleFpga'.
-}
type OutProbes a = [(FpgaIndex, a)]

-- | A class for extracting optional post processing data from a test.
class MayHavePostProcData b c where
  -- | Returns the test names with some post processing data of type @c@,
  -- if that data exists.
  mGetPPD ::
    forall a.
    HitlTestsWithPostProcData a b ->
    Map TestName (Maybe c)

instance MayHavePostProcData a a where
  mGetPPD = fmap (Just . snd)

{- | A custom data type for indicating tests without any additional
post processing data with a custom 'ToJSON' instance. This is
required, because the TCL -> YAML interface does not support empty
lists or empty objects.
-}
data NoPostProcData = NoPostProcData

instance ToJSON NoPostProcData where toJSON _ = Aeson.Null
instance MayHavePostProcData NoPostProcData a where mGetPPD = const []

-- | Drive a value on the `probe_test_data` VIO output probe on each FPGAs.
allFpgas :: a -> OutProbes a
allFpgas a = (,a) <$> [0 ..]

-- | Drive a value on the `probe_test_data` VIO output probe on one specific FPGA.
singleFpga :: FpgaIndex -> a -> OutProbes a
singleFpga ix a = [(ix, a)]

{- | Define a 'HitlTests' for a test that does not accept any input. Use of 'noConfigTest'
should be paired with 'hitlVioBool'.

Example invocation:

> tests :: HitlTests ()
> tests = noConfigTest allFpgas
-}
noConfigTest :: TestName -> (forall a. a -> OutProbes a) -> HitlTests ()
noConfigTest nm f = Map.singleton nm (f (), NoPostProcData)

{- | Generate a set of tests from an enum. E.g., if you defined a data type looking
like:

> data ABC = A | B | C

You can use the following to generate a test config that runs a test for each
constructor of @ABC@:

> tests :: HitlTests ABC
> tests = testsFromEnum allFpgas
-}
testsFromEnum :: (Show a, Bounded a, Enum a) => (a -> OutProbes a) -> HitlTests a
testsFromEnum f =
  Map.fromList $
    map (\a -> (Text.pack (show a), (f a, NoPostProcData))) [minBound ..]

-- | A list, but with a custom "ToJSON" instance to work around Vivado issues
newtype PackedList a = PackedList [a]

{- | XXX: Custom "ToJSON" instance for "PackedList" that converts an empty
       "PackedList" into a 'Aeson.Null' to accommodate Vivado's poorly
       implemented JSON/YAML parser.
-}
instance (ToJSON a) => ToJSON (PackedList a) where
  toJSON (PackedList []) = Aeson.Null
  toJSON (PackedList l) = toJSON l

{- | A map from a probe name to a (binary) value with a custom "ToJSON" instance
to work around Vivado issues.
-}
newtype PackedProbes = PackedProbes (Map Text Natural)

{- | XXX: Custom "ToJSON" instance for "PackedProbes" that converts an empty
       "PackedProbes" into a 'Aeson.Null' to accommodate Vivado's poorly
       implemented JSON/YAML parser.
-}
instance ToJSON PackedProbes where
  toJSON (PackedProbes []) = Aeson.Null
  toJSON (PackedProbes l) = toJSON l

-- | See "PackedTests"
newtype PackedTargetRef = ByIndex {index :: Integer}
  deriving (Generic, ToJSON)

-- | See "PackedTests"
data PackedTarget = PackedTarget
  { target :: PackedTargetRef
  , probes :: PackedProbes
  }
  deriving (Generic, ToJSON)

-- | See "PackedTests"
data PackedTest a = PackedTest
  { targets :: PackedList PackedTarget
  , postproc :: a
  }
  deriving (Generic, ToJSON)

{- | Intermediate representation of "HitlTests". There to provide trivial instances
of "ToJSON".
-}
data PackedTests a = PackedTests
  { defaults :: PackedProbes
  , tests :: Map Text (PackedTest a)
  }

instance (ToJSON a) => ToJSON (PackedTests a) where
  toJSON (PackedTests{defaults, tests}) =
    object
      [ "defaults" .= object ["probes" .= defaults]
      , "tests" .= toJSON tests
      ]

{- | Convert an \"unpacked\" "HitlTests" to a packed version. The packed version
is convertible to JSON, which in turn can be interpreted by the @HardwareTest.tcl@.
-}
toPacked ::
  forall a b.
  (BitPack a, ToJSON b) =>
  HitlTestsWithPostProcData a b ->
  PackedTests b
toPacked hitlTests = PackedTests{defaults, tests}
 where
  bitSizeA = natToInteger @(BitSize a)
  tests =
    fromList
      [ (name, goProbes probes ppData)
      | (name, (probes, ppData)) <- toList hitlTests
      ]
  defaults
    -- If @a@ is a zero-width type, we don't want to generate any data probes
    | bitSizeA == 0 = PackedProbes []
    | otherwise = PackedProbes [("probe_test_data", 0)]

  goProbes probes postproc =
    PackedTest
      { targets = PackedList $ goTargetList probes
      , ..
      }

  goTargetList probes
    | bitSizeA == 0 =
        [ PackedTarget
          { target = ByIndex (toInteger id_)
          , probes = PackedProbes []
          }
        | (id_, _) <- probes
        ]
    | otherwise =
        [ PackedTarget
          { target = ByIndex (toInteger id_)
          , probes = PackedProbes [("probe_test_data", BitVector.unsafeToNatural (pack dat))]
          }
        | (id_, dat) <- probes
        ]

{- | Convert a collection of named tests ("HitlTests") to a \"packed\" representation
readable by our TCL test infrastructure. It will generate YAML/JSON that looks
like:

> defaults:
>   probes:
>     probe_test_data: 0
>
> tests:
>   testname1:
>     targets:
>       - id: 0
>         probes:
>           probe_test_data: <binary representation of 'a'>
>       - id: 1
>         probes:
>           probe_test_data: <binary representation of 'a'>
>       ...
>   testname2:
>     ...
-}
packAndEncode ::
  forall a b.
  (BitPack a, ToJSON b) =>
  HitlTestsWithPostProcData a b ->
  LazyByteString.ByteString
packAndEncode =
  encodePretty'
    defConfig
      { confNumFormat = Custom (encodeToTextBuilder . Number)
      }
    . toPacked

-- | Whether a test has been completed, see 'hitlVio'.
type Done = Bool

-- | Whether a test has been completed successfully, see 'hitlVio'.
type Success = Bool

{- | Instantiate this VIO in a design you'd like to test with hardware in the
loop. Its output is set to 'Nothing' if a test is not running, and will be
set to 'Just' if it is.
-}
hitlVio ::
  forall a dom.
  ( KnownDomain dom
  , BitPack a
  ) =>
  -- | Default value for @a@. This is an artifact of this VIO internally representing
  -- the output value as two probes (\"valid\" and \"data\") to accommodate the
  -- TCL infrastructure. Hence, the actual value of the default doesn't matter:
  -- whenever it is output, this VIO will output 'Nothing'.
  --
  -- TODO: Allow use of 'errorX' in 'vioProbe'
  a ->
  P.Clock dom ->
  -- | Should be asserted when a test is done. For sanity checking the TCL
  -- infrastructure, this must be *deasserted* when a test is not running.
  P.Signal dom Done ->
  -- | When 'Done' is asserted, this signal indicates whether a test has been
  -- completed successfully.
  P.Signal dom Success ->
  -- | Test values supplied by the VIO. Test modules should export a symbol
  -- @tests :: HitlTests a@ that defines the data.
  P.Signal dom (Maybe a)
hitlVio dflt clk done success
  | natToInteger @(BitSize a) == 0 =
      -- XXX: This branch is a workaround for 'vioProbe' not handling zero-width
      --      ports properly.
      P.mux start (pure (Just dflt)) (pure Nothing)
 where
  start =
    P.setName @"vioHitlt" $
      vioProbe
        ("probe_test_done" :> "probe_test_success" :> Nil)
        ("probe_test_start" :> Nil)
        False
        clk
        done
        success
hitlVio dflt clk done success =
  P.mux start (Just <$> dat) (pure Nothing)
 where
  (P.unbundle -> (start, dat)) =
    P.setName @"vioHitlt" $
      vioProbe
        ("probe_test_done" :> "probe_test_success" :> Nil)
        ("probe_test_start" :> "probe_test_data" :> Nil)
        (False, dflt)
        clk
        done
        success

{- | Instantiate this VIO in a design you'd like to test with hardware in the
loop. Its output is set to 'True' if a test is not running, and will be
set to 'False' if it is.
-}
hitlVioBool ::
  forall dom.
  (KnownDomain dom) =>
  P.Clock dom ->
  -- | Should be asserted when a test is done. For sanity checking the TCL
  -- infrastructure, this must be *deasserted* when a test is not running.
  P.Signal dom Done ->
  -- | When 'Done' is asserted, this signal indicates whether a test has been
  -- completed successfully.
  P.Signal dom Success ->
  -- | Test started?
  P.Signal dom Bool
hitlVioBool clk done success = isJust <$> hitlVio () clk done success
