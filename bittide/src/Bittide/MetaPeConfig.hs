-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE ImplicitParams #-}

-- | Device to store configuration data for the Switch Demo
module Bittide.MetaPeConfig where

import Clash.Prelude

import Bittide.SharedTypes (Bitbone)
import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.MemoryMap (Access (..), Mm)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access, description),
  deviceWb,
  registerConfig,
  registerWbI_,
 )

metaPeConfig ::
  forall bufferSize aw dom.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat aw
  , 4 <= aw
  , KnownNat bufferSize
  , 1 <= bufferSize
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  SNat bufferSize ->
  Circuit
    ( ToConstBwd Mm
    , Bitbone dom aw
    )
    ()
metaPeConfig SNat = circuit $ \(mm, wb) -> do
  [wMC, wO, wN, rMC, rO, rN, buffer] <- deviceWb "MetaPeConfig" -< (mm, wb)

  registerWbI_ wMcConfig (0 :: Unsigned 32) -< (wMC, Fwd noWrite)
  registerWbI_ wOConfig (0 :: Unsigned 32) -< (wO, Fwd noWrite)
  registerWbI_ wNConfig (0 :: Unsigned 32) -< (wN, Fwd noWrite)
  registerWbI_ rMcConfig (0 :: Unsigned 32) -< (rMC, Fwd noWrite)
  registerWbI_ rOConfig (0 :: Unsigned 32) -< (rO, Fwd noWrite)
  registerWbI_ rNConfig (0 :: Unsigned 32) -< (rN, Fwd noWrite)

  registerWbI_ bufferConfig (repeat 0 :: Vec (bufferSize * 3) (BitVector 64))
    -< (buffer, Fwd noWrite)

  idC -< ()
 where
  wMcConfig =
    (registerConfig "write_metacycle")
      { access = ReadWrite
      , description = "The metacycle in which the PE should write in"
      }
  wOConfig =
    (registerConfig "write_offset")
      { access = ReadWrite
      , description = "Offset in gather memory to start writing from"
      }
  wNConfig =
    (registerConfig "write_n_addresses")
      { access = ReadWrite
      , description = "How many addresses the PE should write"
      }
  rMcConfig =
    (registerConfig "read_metacycle")
      { access = ReadWrite
      , description = "The metacycle in which the PE should read"
      }
  rOConfig =
    (registerConfig "read_offset")
      { access = ReadWrite
      , description = "Offset in scatter memory to start reading from"
      }
  rNConfig =
    (registerConfig "read_n_addresses")
      { access = ReadWrite
      , description = "How many addresses the PE should read"
      }

  bufferConfig =
    (registerConfig "buffer")
      { access = ReadWrite
      , description = "Buffer to hold bittide data"
      }

  noWrite = pure Nothing
