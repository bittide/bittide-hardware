#!/usr/bin/env python3
"""
Make sure the mentioned Cabal files have the same common settings
"""
# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
import sys

COMMON = """
-- One day we'll be able to replace this by 'default-language: GHC2024' and
-- disabling a few extensions :)
common ghc2024
  default-extensions:
    -- GHC2024, excluding:
    -- ImplicitPrelude:          We want Clash.Prelude
    -- ImplicitStagePersistence: Not a thing yet in the GHC we use
    -- ImportQualifiedPost:      Okay on its own, but trips up fourmolu
    -- StarIsType:               We want '*' to mean multiplication on type level
    BangPatterns
    BinaryLiterals
    ConstrainedClassMethods
    ConstraintKinds
    DataKinds
    DeriveDataTypeable
    DeriveFoldable
    DeriveFunctor
    DeriveGeneric
    DeriveLift
    DeriveTraversable
    DerivingStrategies
    DisambiguateRecordFields
    DoAndIfThenElse
    EmptyCase
    EmptyDataDecls
    EmptyDataDeriving
    ExistentialQuantification
    ExplicitForAll
    ExplicitNamespaces
    FieldSelectors
    FlexibleContexts
    FlexibleInstances
    ForeignFunctionInterface
    GADTSyntax
    GADTs
    GeneralisedNewtypeDeriving
    HexFloatLiterals
    InstanceSigs
    KindSignatures
    LambdaCase
    MonoLocalBinds
    MonomorphismRestriction
    MultiParamTypeClasses
    NamedFieldPuns
    NamedWildCards
    NumericUnderscores
    PatternGuards
    PolyKinds
    PostfixOperators
    RankNTypes
    RelaxedPolyRec
    RoleAnnotations
    ScopedTypeVariables
    StandaloneDeriving
    StandaloneKindSignatures
    TraditionalRecordSyntax
    TupleSections
    TypeApplications
    TypeOperators
    TypeSynonymInstances

common clash
  -- TemplateHaskell is used to support convenience functions such as
  -- 'listToVecTH' and 'bLit'.
  --
  -- `NoImplicitPrelude` is used because Clash offers Clash.Prelude
  default-extensions:
    NoImplicitPrelude
    NoStarIsType
    QuasiQuotes
    TemplateHaskell

  ghc-options:
    -- Plugins to support type-level constraint solving on naturals:
    --   - GHC.TypeLits.Extra.Solver
    --   - GHC.TypeLits.Normalise
    --   - GHC.TypeLits.KnownNat.Solver
    --
    -- Clash needs access to the source code in compiled modules:
    --   -fexpose-all-unfoldings
    --
    -- Worker wrappers introduce unstable names for functions that might have
    -- blackboxes attached for them. You can disable this, but be sure to add
    -- a no-specialize pragma to every function with a blackbox.
    --   -fno-worker-wrapper
    --
    -- Strict annotations - while sometimes preventing space leaks - trigger
    -- optimizations Clash can't deal with. See:
    --
    --    https://github.com/clash-lang/clash-compiler/issues/2361
    --
    -- These flags disables the optimization:
    --   -fno-unbox-small-strict-fields
    --   -fno-unbox-strict-fields
    -Wall
    -Wcompat
    -fplugin=GHC.TypeLits.Extra.Solver
    -fplugin=GHC.TypeLits.Normalise
    -fplugin=GHC.TypeLits.KnownNat.Solver
    -fplugin=Protocols.Plugin
    -fconstraint-solver-iterations=20
    -fexpose-all-unfoldings
    -fno-worker-wrapper
    -fno-unbox-small-strict-fields
    -fno-unbox-strict-fields

  build-depends:
    clash-protocols,
    ghc-typelits-extra >=0.4.4,
    ghc-typelits-knownnat >=0.7.7,
    ghc-typelits-natnormalise >=0.7.7,

common extra
  default-extensions:
    DefaultSignatures
    DeriveAnyClass
    ImplicitParams
    OverloadedRecordDot
    TypeFamilies
    ViewPatterns

  ghc-options:
    -Wall
    -Wcompat

common sane-records
  default-extensions:
    DisambiguateRecordFields
    DuplicateRecordFields
    NamedFieldPuns
    NoFieldSelectors
    OverloadedRecordDot
"""


CABAL_FILES = [
  "bittide/bittide.cabal",
  "bittide-instances/bittide-instances.cabal",
  "bittide-extra/bittide-extra.cabal",
  "bittide-experiments/bittide-experiments.cabal",
  # clash-bitpackc doesn't need clash-protocols, but this script doesn't handle
  # cases like this.
  #
  # "clash-bitpackc/clash-bitpackc.cabal",
  "clash-protocols-memmap/clash-protocols-memmap.cabal",
]

def main():
    for cabal_file in CABAL_FILES:
        with open(cabal_file, 'r') as f:
            content = f.read()

        if COMMON not in content:
            print(f"Error: common header not found in {cabal_file}", file=sys.stderr)
            return 1

    return 0


if __name__ == '__main__':
    sys.exit(main())
