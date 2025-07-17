-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Validity checks performed on memory maps
module Protocols.MemoryMap.Check (
  module Protocols.MemoryMap.Check.AbsAddress,
  module Protocols.MemoryMap.Check.Normalized,
  getErrorMessage,
) where

import Clash.Prelude

import GHC.Stack (SrcLoc (..))
import Protocols.MemoryMap.Check.AbsAddress
import Protocols.MemoryMap.Check.Normalized
import Text.Printf (printf)

getErrorMessage :: AddressError -> String
getErrorMessage err =
  case err of
    SizeExceedsError
      { startAddr
      , availableSize
      , requestedSize
      , path
      , location
      } ->
        printf
          "Component %s at address %08X with size %08X exceeds the available size %08X (%s)"
          (show path)
          startAddr
          requestedSize
          availableSize
          (shortLocation location)
    AddressDifferentThanExpected
      { expected
      , actual
      , path
      , location
      } ->
        printf
          "Component %s has been given an absolute address %08X which is different from the computed one %08X (%s)"
          (show path)
          expected
          actual
          (shortLocation location)
 where
  shortLocation s@SrcLoc{} = s.srcLocFile <> ":" <> show s.srcLocStartLine <> ":" <> show s.srcLocStartCol
