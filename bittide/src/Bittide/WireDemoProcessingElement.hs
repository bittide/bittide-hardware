-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.WireDemoProcessingElement (
  wireDemoPe,
  wireDemoPeConfig,
) where

import Clash.Prelude
import Protocols

import Clash.Class.BitPackC (ByteOrder)
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import Protocols.Experimental.Wishbone (Wishbone, WishboneMode (Standard))
import Protocols.MemoryMap (Access (ReadOnly, ReadWrite), Mm)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access, description),
  deviceConfig,
  deviceWbI,
  registerConfig,
  registerWbI,
  registerWbI_,
 )

wireDemoPeConfig ::
  forall dom addrW nBytes linkCount linkWidth.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat linkCount
  , 1 <= linkCount
  , KnownNat linkWidth
  , ?byteOrder :: ByteOrder
  ) =>
  Circuit
    ( (ToConstBwd Mm, Wishbone dom 'Standard addrW nBytes)
    , "WRITTEN_DATA" ::: CSignal dom (Maybe (BitVector linkWidth))
    )
    ( "READ_LINK" ::: CSignal dom (Maybe (Index linkCount))
    , "WRITE_LINK" ::: CSignal dom (Maybe (Index linkCount))
    )
wireDemoPeConfig = circuit $ \(bus, writtenData) -> do
  [wbReadLink, wbWriteLink, wbWrittenData] <-
    deviceWbI (deviceConfig "WireDemoPeConfig") -< bus

  (Fwd readLink, _readLinkActivity) <-
    registerWbI
      (registerConfig "read_link")
        { access = ReadWrite
        , description = "Index of the link to read from."
        }
      Nothing
      -< (wbReadLink, Fwd (pure Nothing))

  (Fwd writeLink, _writeLinkActivity) <-
    registerWbI
      (registerConfig "write_link")
        { access = ReadWrite
        , description = "Index of the link to write to."
        }
      Nothing
      -< (wbWriteLink, Fwd (pure Nothing))

  registerWbI_
    (registerConfig "written_data")
      { access = ReadOnly
      , description =
          "The data written by the wireDemoPe in the second cycle after reset."
      }
    0
    -< (wbWrittenData, writtenData)

  idC -< Fwd (readLink, writeLink)

{- | Number of cycles the Write state is held. A wider window tolerates elastic-buffer
drift (Callisto adjustments between frame-correction application and the PE firing)
that would otherwise cause the read from the upstream node to land in the Idle state
rather than the Write state.  12 cycles covers the observed 2-7 frame post-correction
drift with comfortable margin.
-}
type WriteHoldCycles = 12

data PeState linkCount linkWidth
  = Read
  | Write (Index linkCount) (BitVector linkWidth) (Unsigned (CLog 2 WriteHoldCycles))
  | Idle
  deriving (Eq, Show, Generic, NFDataX)

{- | The wireDemoPe is active for a short burst after reset.  In the first cycle
it samples from the link indicated by the config (or sources static 'dna').  For
the following 'WriteHoldCycles' cycles it writes
'value_read_in_first_cycle XOR fpga_dna_lsbs' to the link indicated by the config
(or writes to link '0').
Writes the local counter to all links by default, which is useful when debugging.
-}
wireDemoPe ::
  forall dom linkCount linkWidth.
  ( HasCallStack
  , HiddenClock dom
  , KnownNat linkCount
  , 1 <= linkCount
  , KnownNat linkWidth
  ) =>
  Reset dom ->
  -- | DNA value
  Signal dom (Maybe (BitVector 96)) ->
  -- | Local counter
  Signal dom (Unsigned 64) ->
  Circuit
    ( "LINKS_IN" ::: Vec linkCount (CSignal dom (BitVector linkWidth))
    , "READ_INDEX" ::: CSignal dom (Maybe (Index linkCount))
    , "WRITE_INDEX" ::: CSignal dom (Maybe (Index linkCount))
    )
    ( "LINKS_OUT" ::: Vec linkCount (CSignal dom (BitVector linkWidth))
    , "WRITTEN_DATA" ::: CSignal dom (Maybe (BitVector linkWidth))
    )
wireDemoPe rst maybeDna localCounter = Circuit go
 where
  go ((linksIn, readIndex_, writeIndex_), _) = ((repeat (), (), ()), (unbundle linksOut, writtenData))
   where
    (linksOut, writtenData) =
      withClockResetEnable hasClock rst enableGen
        $ mealyB goMealy Read (bundle linksIn, readIndex_, writeIndex_, dnaLsbs, localCounter)
    -- TODO: Do the `fromMaybe` at top level, so all components use the same default DNA value.
    dnaLsbs = resize . fromMaybe 0xDEADBEEF <$> maybeDna

    goMealy ::
      PeState linkCount linkWidth ->
      ( Vec linkCount (BitVector linkWidth)
      , Maybe (Index linkCount)
      , Maybe (Index linkCount)
      , BitVector linkWidth
      , Unsigned 64
      ) ->
      ( PeState linkCount linkWidth
      , ( Vec linkCount (BitVector linkWidth)
        , Maybe (BitVector linkWidth)
        )
      )
    goMealy Read (links, readIndex, writeIndex, dna, counter) =
      let
        value = case readIndex of
          Just rdIdx -> links !! rdIdx `xor` dna
          Nothing -> dna
        nextState = Write (fromMaybe 0 writeIndex) value (natToNum @(WriteHoldCycles - 1))
       in
        (nextState, (repeat (resize (pack counter)), Nothing))
    goMealy (Write writeIndex value remaining) (_, _, _, _, counter) =
      let nextState = if remaining == 0 then Idle else Write writeIndex value (remaining - 1)
       in (nextState, (replace writeIndex value (repeat (resize (pack counter))), Just value))
    goMealy Idle (_, _, _, _, counter) =
      (Idle, (repeat (resize (pack counter)), Nothing))
