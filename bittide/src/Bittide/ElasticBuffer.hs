-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ElasticBuffer where

import Bittide.ClockControl (RelDataCount, targetDataCount)
import Bittide.Df
import Bittide.Extra.Maybe (orNothing)
import Clash.Class.BitPackC (BitPackC, ByteOrder, Bytes)
import Clash.Cores.Xilinx.DcFifo
import Clash.Cores.Xilinx.Xpm.Cdc.Extra (safeXpmCdcHandshake)
import Clash.Cores.Xilinx.Xpm.Cdc.Pulse (xpmCdcPulse)
import Clash.Prelude
import GHC.Stack
import Protocols (Ack (..), CSignal, Circuit (..), applyC)
import Protocols.MemoryMap (Access (..), ConstBwd, Mm)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (..),
  busActivityWrite,
  deviceWb,
  registerConfig,
  registerWb,
  registerWbDf,
 )
import Protocols.MemoryMap.TypeDescription.TH
import Protocols.Wishbone (Wishbone, WishboneMode (Standard))

import qualified Clash.Explicit.Prelude as E

data EbCommand
  = -- | Disable write, enable read
    Drain
  | -- | Enable write, disable read
    Fill
  deriving (Generic, NFDataX, Eq, Show, BitPack, BitPackC, ShowX)
deriveTypeDescription ''EbCommand

type Underflow = Bool
type Overflow = Bool
type Stable = Bool

{-# OPAQUE sticky #-}

-- | Create a sticky version of a boolean signal.
sticky ::
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  Signal dom Bool
sticky clk rst a = stickyA
 where
  stickyA = E.register clk rst enableGen False (stickyA .||. a)

{-# OPAQUE xilinxElasticBuffer #-}

{- | An elastic buffer backed by a Xilinx FIFO. It exposes all its control and
monitor signals in its read domain.
-}
xilinxElasticBuffer ::
  forall n readDom writeDom a.
  ( HasCallStack
  , KnownDomain readDom
  , KnownDomain writeDom
  , NFDataX a
  , KnownNat n
  , 4 <= n
  , n <= 17
  ) =>
  Clock readDom ->
  Clock writeDom ->
  -- | Operating mode of the elastic buffer. Must remain stable until an acknowledgement
  -- is received.
  Signal readDom (Maybe EbCommand) ->
  Signal writeDom a ->
  ( Signal readDom (RelDataCount n)
  , Signal readDom Underflow
  , Signal writeDom Overflow
  , Signal readDom a
  , -- Acknowledgement for EbMode
    Signal readDom Ack
  )
xilinxElasticBuffer clkRead clkWrite command wdata =
  ( -- Note that this is chosen to work for 'RelDataCount' either being
    -- set to 'Signed' with 'targetDataCount' equals 0 or set to
    -- 'Unsigned' with 'targetDataCount' equals 'shiftR maxBound 1 + 1'.
    -- This way, the representation can be easily switched without
    -- introducing major code changes.
    (+ targetDataCount)
      . bitCoerce
      . (+ (-1 - shiftR maxBound 1))
      <$> readCount
  , isUnderflow
  , isOverflow
  , fifoData
  , commandAck
  )
 where
  FifoOut{readCount, isUnderflow, isOverflow, fifoData} =
    dcFifo
      (defConfig @n){dcOverflow = True, dcUnderflow = True}
      clkWrite
      noResetWrite
      clkRead
      noResetRead
      writeData
      readEnable

  -- We don't reset the Xilix FIFO: its reset documentation is self-contradictory
  -- and mentions situations where the FIFO can end up in an unrecoverable state.
  noResetWrite = unsafeFromActiveHigh (pure False)
  noResetRead = unsafeFromActiveHigh (pure False)

  drain = command .== Just Drain
  commandAck = mux drain drainAck otherAck
  otherAck = pure $ Ack True
  readEnable = command ./= Just Fill

  -- TODO: Instead of hoisting over a flag to drain a single element, we could
  --       hoist over a count to drain multiple elements at once. This would
  --       significantly reduce the overhead induced by the handshake.
  drainFlag = orNothing <$> drain <*> pure high
  drainSynced = drainFlagSynced .== Just high
  writeData = mux drainSynced (pure Nothing) (Just <$> wdata)
  (drainAck, drainFlagSynced) =
    safeXpmCdcHandshake
      clkRead
      E.noReset
      clkWrite
      E.noReset
      drainFlag
      (pure (Ack True))

{-# OPAQUE xilinxElasticBufferWb #-}

{- | Wishbone wrapper around 'xilinxElasticBuffer' that exposes control and monitoring
via memory-mapped registers using the clash-protocols-memmap infrastructure.

This component allows software control of the elastic buffer's buffer occupancies and
provides monitoring capabilities. The primary use case is to enable a CPU to perform
buffer initialization during the system startup phase.

The registers provided are:
1. Command Register (Write-Only): Adding or removing singular frames from the buffer.
2. Data Count Register (Read-Only): Current fill level of the buffer.
3. Underflow Register (Read-Write): Sticky flag indicating if an underflow has occurred.
   Flag can be cleared by writing false to this register.
4. Overflow Register (Read-Write): Sticky flag indicating if an overflow has occurred.
   Flag can be cleared by writing false to this register.
5. Stable Register (Write-Only): Software can set this flag to indicate that the buffer
   is stable.
-}
xilinxElasticBufferWb ::
  forall n readDom writeDom addrW a.
  ( HasCallStack
  , KnownDomain readDom
  , KnownDomain writeDom
  , NFDataX a
  , KnownNat n
  , 4 <= n
  , n <= 17
  , KnownNat addrW
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  Clock readDom ->
  Reset readDom ->
  SNat n ->
  Clock writeDom ->
  Signal writeDom a ->
  Circuit
    (ConstBwd Mm, Wishbone readDom 'Standard addrW (Bytes 4))
    ( CSignal readDom (RelDataCount n)
    , CSignal readDom Underflow
    , CSignal readDom Overflow
    , CSignal readDom Stable
    , CSignal readDom a
    )
xilinxElasticBufferWb clkRead rstRead SNat clkWrite wdata = circuit $ \wb -> do
  [wbCommand, wbDataCount, wbUnderflow, wbOverflow, wbStable] <-
    deviceWb "ElasticBuffer" -< wb

  -- Command register: Writing to this register adds or removes a single element
  -- from the buffer occupancy
  (_ebCommand, ebCommandDfActivity) <-
    registerWbDf
      clkRead
      rstRead
      (registerConfig "command"){access = WriteOnly}
      Drain
      -< (wbCommand, Fwd (pure Nothing))

  ebCommandDf <- applyC (fmap busActivityWrite) id -< ebCommandDfActivity

  let (dataCount, underflow, overflow0, readData, commandAck) =
        xilinxElasticBuffer @n clkRead clkWrite ebCommandSig wdata

  Fwd ebCommandSig <- unsafeFromDf -< (ebCommandDf, Fwd commandAck)

  -- Synchronize overflow pulse from write domain to read domain
  let overflow1 = xpmCdcPulse clkWrite clkRead overflow0

  (dataCountOut, _dataCountActivity) <-
    registerWb
      clkRead
      rstRead
      (registerConfig "data_count"){access = ReadOnly}
      0
      -< (wbDataCount, Fwd (Just <$> dataCount))

  (underflowOut, _underflowActivity) <-
    registerWb
      clkRead
      rstRead
      (registerConfig "underflow"){access = ReadWrite}
      False
      -< (wbUnderflow, Fwd (flip orNothing True <$> underflow))

  (overflowOut, _overflowActivity) <-
    registerWb
      clkRead
      rstRead
      (registerConfig "overflow"){access = ReadWrite}
      False
      -< (wbOverflow, Fwd (flip orNothing True <$> overflow1))

  -- Stable register: Software can set this to indicate buffer is ready
  (stableOut, _stableActivity) <-
    registerWb
      clkRead
      rstRead
      (registerConfig "stable"){access = WriteOnly}
      False
      -< (wbStable, Fwd (pure Nothing))

  idC -< (dataCountOut, underflowOut, overflowOut, stableOut, Fwd readData)
