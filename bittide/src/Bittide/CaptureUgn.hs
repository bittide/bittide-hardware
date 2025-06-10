-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}

module Bittide.CaptureUgn (captureUgn) where

import Clash.Explicit.Prelude hiding (PeriodToCycles)

import Data.Maybe (fromMaybe, isJust)
import Data.Tuple (swap)

import Protocols
import Protocols.MemoryMap
import Protocols.Wishbone

import Bittide.SharedTypes (Bytes)
import Bittide.Wishbone (wbToVec)

import qualified Clash.Prelude as C
import GHC.Stack (HasCallStack)

{- | Captures the remote counter from a bittide link and pairs it with the corresponding
local counter. All frames except for the first frame will be forwarded to the output.

Assumes that:
- When a link is not up, it produces `Nothing` consistently
- When a link comes down, it will at some point switch from only producing `Nothing`,
  to only producing `Just data`
- When a link comes up, the first frame will contain the remote counter value.
- When a link goes down, it will start producing consistently `Nothing`

The register layout is as follows:
- Address 0: lsbs local counter
- Address 1: msbs local counter
- Address 2: lsbs remote counter
- Address 3: msbs remote counter
-}
captureUgn ::
  forall dom addrW.
  ( HasCallStack
  , C.HiddenClockResetEnable dom
  , KnownNat addrW
  ) =>
  Signal dom (Unsigned 64) ->
  Signal dom (Maybe (BitVector 64)) ->
  Circuit
    (ConstBwd MM, Wishbone dom 'Standard addrW (Bytes 4))
    (CSignal dom (BitVector 64))
captureUgn localCounter linkIn = Circuit go
 where
  go ((_, wbM2S), _) = ((SimOnly mm, wbS2M), bittideData)
   where
    state =
      C.regEn
        (0x1111111111111111, 0x2222222222222222)
        trigger
        (bundle (pack <$> localCounter, fromMaybe 0 <$> linkIn))
    stateVec = concatMap swapWords . bitCoerce <$> state

    -- Swap the two words of a 64-bit Bitvector to match the word order of
    -- the Vexriscv. This allows the CPU to read the two words as one 64-bit value.
    swapWords :: BitVector 64 -> Vec 2 (BitVector 32)
    swapWords = bitCoerce . (swap @(BitVector 32) @(BitVector 32)) . bitCoerce

    trigger = C.isRising False (isJust <$> linkIn)
    bittideData = fromMaybe 0 <$> mux trigger (pure Nothing) linkIn
    (_, wbS2M) = unbundle $ wbToVec <$> stateVec <*> wbM2S

  mm =
    MemoryMap
      { tree = DeviceInstance locCaller "CaptureUGN"
      , deviceDefs = deviceSingleton deviceDef
      }
  deviceDef =
    DeviceDefinition
      { tags = []
      , registers =
          [ NamedLoc
              { name = Name "local_counter" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(BitVector 64)
                    , address = 0x0
                    , access = ReadOnly
                    , tags = []
                    , reset = Nothing
                    }
              }
          , NamedLoc
              { name = Name "remote_counter" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(BitVector 64)
                    , address = 0x8
                    , access = ReadOnly
                    , tags = []
                    , reset = Nothing
                    }
              }
          ]
      , deviceName = Name "CaptureUGN" ""
      , definitionLoc = locHere
      }
