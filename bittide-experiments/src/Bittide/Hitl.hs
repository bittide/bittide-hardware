-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{- | Tooling to define hardware-in-the-loop (HITL) tests. HITL tests in the
Bittide project involve FPGA designs that incorporate a
[VIO IP core](https://www.xilinx.com/products/intellectual-property/vio.html)
to interface with the HITL test controller. This VIO is used to start tests,
communicate test status and to optionally (depending on the test
definition) provide the FPGA under test with an additional configurable
parameter. In practice, developers writing HITL tests should make sure to do
two things:

  1. They should incorporate a HITL VIO in their design. The HITL test controller
     expects such a VIO to have at minimum an output probe named
     @probe_test_start@ and input probes named @probe_test_done@ and
     @probe_test_success@, all booleans. See 'hitlVio' and
     'hitlVioBool' for examples. When parameters are used (see below) that
     have a BitSize larger than 0, an @probe_test_data@ output probe with an
     equivalent BitSize must be added.

  2. They should define the hardware targets to run the tests against
     (multiple FPGAs, or just one), and with which parameters each of these
     hardware targets should be provided before the test is started.
     See 'HitlTestGroup' for examples, together with its convenience functions
     'allTargets', 'paramForHwTargets', 'paramForSingleHwTarget' and 'testCasesFromEnum'.

Tests are collected in @Bittide.Instances.Hitl.Tests@.

=== __Flow overview__

  1. User calls @shake \<binder\>:test@ to run HITL tests.
  2. Shake builds a bitstream, programs the FPGA, and runs the HITL tests by
     interacting with Vivado in TCL mode using the @vivado-hs@ package.
-}
module Bittide.Hitl (
  ClashTargetName,
  FpgaId,
  DeviceInfo (..),
  HwTargetRef (..),

  -- * Test definition
  HitlTestGroup (..),
  HitlTestCase (..),
  TestStepResult (..),
  MayHavePostProcData (..),
  Done,
  Success,
  hitlVio,
  hitlVioBool,
  noPreProcess,

  -- * Test construction convenience functions
  paramForHwTargets,
  paramForSingleHwTarget,
  testCasesFromEnum,
  hwTargetRefsFromHitlTestGroup,
)
where

import Prelude

import Clash.Prelude (
  BitPack (BitSize),
  BitVector,
  KnownDomain,
  Vec (Nil, (:>)),
  natToInteger,
 )

import Clash.Cores.Xilinx.VIO (vioProbe)

import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import Data.Typeable (Typeable)
import Language.Haskell.TH.Syntax (Name)
import Numeric.Natural (Natural)

import Clash.Prelude qualified as P
import Data.Map.Strict qualified as Map

import System.Exit (ExitCode)

import Vivado (VivadoHandle)
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM

{- | Fully qualified name to a function that is the target for Clash
compilation. E.g. @Bittide.Foo.topEntity@.
-}
type ClashTargetName = Name

{- | The FPGA ID section of a Vivado hardware target. This is what Vivado seems
to call the UID of a hardware target minus the vendor string.

For example, the ID of hardware target
"localhost:3121/xilinx_tcf/Digilent/210308B0B0C2" is "210308B0B0C2".
-}
type FpgaId = String

-- | Provides information for a hardware device able to be targeted in a test.
data DeviceInfo = DeviceInfo
  { deviceId :: String
  -- ^ Can be found in the Vivado GUI or through its TCL interface.
  , dna :: BitVector 96
  -- ^ Can be found in the Vivado GUI or through its TCL interface.
  , serial :: String
  -- ^ Path to the serial device file. For example,
  -- @"/dev/serial/by-path/pci-0000:00:14.0-usb-0:5.4.4.2:1.1-port0"@
  , usbAdapterLocation :: String
  -- ^ The USB adapter location for the hardware target, for example @"1-2:1"@.
  -- Currently primarily used for the JTAG target location with OpenOCD.
  }
  deriving (Eq, Ord, Show)

{- | A reference to an FPGA hardware target, either by index/relative position
in the Bittide demo rig or by ID.
-}
data HwTargetRef
  = HwTargetByIndex Natural
  | HwTargetById FpgaId DeviceInfo
  deriving (Eq, Ord, Show)

data TestStepResult a
  = TestStepSuccess a
  | TestStepFailure String
  deriving (Eq, Ord, Show)

{- | A definition of a test that should be performed with hardware in the loop.
Such a HITL test definition can have one or more named test cases that may differ in
what hardware targets (FPGAs) they involve and in what parameters they provide
to every such hardware target (see `parameters`).
Furthermore, some additional data can be provided, if required by optional
subsequent post-processing steps.

=== __Example: Test without parameters__
A test that runs for all FPGAs, and does not require any parameters (the
parameter is set to `()`):

> test :: HitlTestGroup
> test = HitlTestGroup
>   { topEntity = ...
>   , extraXdcFiles = []
>   , testCases = [HitlTestCase "testCaseName" (paramForHwTargets allHwTargets ()) ()]
>   , mPostProc = Nothing
>   }

This must be accompanied by a @hitlVioBool@ in the design.

=== __Example: Test based on an enum__
A test that runs for each constructor of an enum:

> data ABC = A | B | C
>
> testExtended :: HitlTestGroup
> testExtended = HitlTestGroup
>   { topEntity = ...
>   , extraXdcFiles = []
>   , testCases = testCasesFromEnum @ABC allHwTargets ()
>   , mPostProc = Nothing
>   }

This must be accompanied by a @hitlVio \@ABC@ in the design.

=== __Example: Test without post processing data that runs on specific FPGAs,
and requires a (hypothetical) 8-bit number indicating the
\"number of stages\" to be set on each FPGA:

> type NumberOfStages = P.Unsigned 8
>
> test :: HitlTestGroup
> test = HitlTestGroup
>   { topEntity = '()
>   , extraXdcFiles = []
>   , testCases =
>       [ HitlTestCase
>           { name = "Twelve stages on FPGA 2 and 5"
>           , parameters = Map.fromList
>               [ (HwTargetByIndex 2, 12 :: NumberOfStages)
>               , (HwTargetByIndex 5, 12)
>               ]
>           , postProcData = ()
>           }
>       , HitlTestCase
>           { name = "Six stages on FPGA 3, seven on FPGA 4"
>           , parameters = Map.fromList
>               [ (HwTargetByIndex 3, 6)
>               , (HwTargetByIndex 4, 7)
>               ]
>           , postProcData = ()
>           }
>       ]
>   , mPostProc = Nothing
>   }

This must be accompanied by a @hitlVio \@NumberOfStages@ in the design.
-}
data HitlTestGroup where
  HitlTestGroup ::
    (Typeable a, Typeable b) =>
    { topEntity :: ClashTargetName
    -- ^ Reference to the Design Under Test
    , extraXdcFiles :: [String]
    , testCases :: [HitlTestCase HwTargetRef a b]
    -- ^ List of test cases
    , mDriverProc ::
        Maybe (String -> [(HwTarget, DeviceInfo)] -> VivadoM ExitCode)
    -- ^ Optional function driving the test. If provided, this function must:
    --   - Handle any pre-processing necessary to begin the test
    --   - Assert the start probe(s)
    --   - Wait for results on the test done and success probes
    --
    -- The HITL testing infrastructure deasserts the start probe(s) after running this function
    -- and collecting ILA data, so unless there is a good reason to this function should not also
    -- deassert it. Because the deassertions are not done simultaneously, unpredictable behaviour
    -- caused by the shutdown of only parts of a multi-FPGA system may end up recorded in the test
    -- data ILAs. Thus it's more desirable to allow the HITL testing infrastructure to handle this
    -- process after it has collected the ILA data.
    , mPostProc :: Maybe (FilePath -> ExitCode -> IO (TestStepResult ()))
    -- ^ Optional post processing step. If provided, this function is run after the test case
    -- completely finishes execution, including collection of ILA data and deassertion of the
    -- start probe(s).
    , externalHdl :: [String]
    -- ^ List of external HDL files to include in the project
    } ->
    HitlTestGroup

{- | A HITL test case. One HITL test group can have multiple test cases
associated with it.
-}
data HitlTestCase h a b where
  HitlTestCase ::
    (Show h, Show a, BitPack a, Show b, Typeable h) =>
    { name :: String
    , parameters :: Map h a
    , postProcData :: b
    } ->
    HitlTestCase h a b

deriving instance Show (HitlTestCase h a b)

-- | A class for extracting optional post processing data from a test.
class MayHavePostProcData b where
  -- | Returns the test names with some post processing data of type @c@,
  -- if that data exists.
  mGetPPD ::
    forall h a.
    [HitlTestCase h a b] ->
    Map String (Maybe b)

instance MayHavePostProcData a where
  mGetPPD cases =
    Map.fromList
      [(name, Just postProcData) | HitlTestCase{..} <- cases]

instance MayHavePostProcData () where
  mGetPPD = Map.fromList . map ((,Nothing) . name)

-- | Pre-process function that always succeeds and uses '()' as user-data.
noPreProcess ::
  VivadoHandle -> String -> FilePath -> HwTarget -> DeviceInfo -> IO (TestStepResult ())
noPreProcess _ _ _ _ _ = pure (TestStepSuccess ())

-- | Obtain a list of the hardware targets that are relevant for a given HITL test.
hwTargetRefsFromHitlTestGroup :: HitlTestGroup -> [HwTargetRef]
hwTargetRefsFromHitlTestGroup HitlTestGroup{testCases} =
  nubOrd $ concatMap (map fst . Map.toList . parameters) testCases

-- | Provide a given list of hardware targets with one parameter.
paramForHwTargets :: [HwTargetRef] -> a -> Map HwTargetRef a
paramForHwTargets hwTs param = Map.fromList $ map (,param) hwTs

-- | Returns the hardware target to parameter map for a single hardware target.
paramForSingleHwTarget :: HwTargetRef -> a -> Map HwTargetRef a
paramForSingleHwTarget = Map.singleton

{- | Generate a set of HITL test cases from an enum. E.g., if you defined a
data type looking like:

> data ABC = A | B | C
>   deriving (BitPack, Bounded, Enum, Generic, Show)

You can use the following to generate a test case for each constructor
of @ABC@. Every such case is named after the constructor that gave rise
to it and receives that constructor as test parameter.

> testCases :: [HitlTestCase HwTargetRef ABC ()]
> testCases = testCasesFromEnum @ABC allHwTargets ()
-}
testCasesFromEnum ::
  -- forall a b c.
  forall a b.
  ( Show a
  , Bounded a
  , Enum a
  , BitPack a
  , Show b
  , Typeable a
  , Typeable b
  ) =>
  [HwTargetRef] ->
  b ->
  [HitlTestCase HwTargetRef a b]
testCasesFromEnum hwTs ppd =
  [ HitlTestCase
    { name = show constr
    , parameters = Map.fromList ((,constr) <$> hwTs)
    , postProcData = ppd
    }
  | (constr :: a) <- [minBound ..]
  ]

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
  -- HITL test infrastructure. Hence, the actual value of the default doesn't
  -- matter: whenever it is output, this VIO will output 'Nothing'.
  --
  -- TODO: Allow use of 'errorX' in 'vioProbe'
  a ->
  P.Clock dom ->
  -- | Should be asserted when a test is done. For sanity checking the HITL test
  -- infrastructure, this must be *deasserted* when a test is not running.
  P.Signal dom Done ->
  -- | When 'Done' is asserted, this signal indicates whether a test has been
  -- completed successfully.
  P.Signal dom Success ->
  -- | Test parameter supplied by the VIO. Test modules should export a symbol
  -- @test :: HitlTestGroup@ that defines this parameter for every hardware target
  -- (FPGA) that the test involves.
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
  -- | Should be asserted when a test is done. For sanity checking the HITL test
  -- infrastructure, this must be *deasserted* when a test is not running.
  P.Signal dom Done ->
  -- | When 'Done' is asserted, this signal indicates whether a test has been
  -- completed successfully.
  P.Signal dom Success ->
  -- | Test started?
  P.Signal dom Bool
hitlVioBool clk done success = isJust <$> hitlVio () clk done success
