-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

module Tests.Gdb.Commands.Read where

import Clash.Prelude (NFDataX, Unsigned)
import Prelude

import Clash.Class.BitPackC (BitPackC)
import GHC.Generics (Generic)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)
import Tests.Common (getGitRoot, run)

import qualified Gdb

data WeirdAlignment = WeirdAlignment
  { a :: Unsigned 8
  , b :: Unsigned 32
  , c :: Unsigned 16
  , d :: Unsigned 64
  , e :: Unsigned 8
  }
  deriving (Show, Eq, Generic, BitPackC, NFDataX)

case_simple_adt :: Assertion
case_simple_adt = do
  Gdb.withGdb $ \gdb -> do
    Gdb.setFile gdb binaryPath
    Gdb.runCommand gdb "break main.rs:22"
    Gdb.runCommand gdb "break main.rs:28"
    Gdb.runCommand gdb "run"

    address <- (read @Integer . last . words) <$> Gdb.readCommand1 gdb "print &x"

    actual1 <- Gdb.readLe gdb address
    assertEqual "WeirdAlignment1" actual1 expected1

    Gdb.runCommand gdb "continue"
    actual2 <- Gdb.readLe gdb address
    assertEqual "WeirdAlignment2" actual2 expected2
 where
  binaryPath = simpleAdtRoot </> "target" </> "x86_64-unknown-linux-gnu" </> "debug" </> "simple_adt"

  expected1 =
    WeirdAlignment
      { a = 0x12
      , b = 0x34567890
      , c = 0xABCD
      , d = 0x123456789ABCDEF0
      , e = 0xFF
      }

  expected2 =
    WeirdAlignment
      { a = 0x34
      , b = 0x1337_4242
      , c = 0xBEEF
      , d = 0xDEAD_10C0_DEAD_71C0
      , e = 0x11
      }

tests :: TestTree
tests =
  sequentialTestGroup
    "Tests.Gdb.Commands.Read"
    AllSucceed
    [ testGroup
        "Build Rust crates"
        [ testCase "simple_adt" (run "cargo" ["build"] (Just simpleAdtRoot))
        ]
    , $(testGroupGenerator)
    ]
 where

simpleAdtRoot :: FilePath
simpleAdtRoot = dataRoot </> "simple_adt"

dataRoot :: FilePath
dataRoot = unsafePerformIO getGitRoot </> "gdb-hs" </> "tests" </> "data"
