-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ElasticBuffer where

import Bittide.ClockControl (RelDataCount, targetDataCount)
import Bittide.Df
import Bittide.Extra.Maybe (orNothing)
import Bittide.SharedTypes (BitboneMm)
import Clash.Class.BitPackC (BitPackC, ByteOrder)
import Clash.Cores.Xilinx.DcFifo
import Clash.Cores.Xilinx.Xpm.Cdc.Extra (safeXpmCdcHandshake)
import Clash.Cores.Xilinx.Xpm.Cdc.Pulse (xpmCdcPulse)
import Clash.Prelude
import GHC.Stack
import Protocols (Ack (..), CSignal, Circuit (..), applyC)
import Protocols.Df.Extra (ackWhen, skid)
import Protocols.MemoryMap (Access (..))
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (..),
  deviceWb,
  registerConfig,
  registerWbDfI,
  registerWbI,
 )
import Protocols.MemoryMap.TypeDescription.TH

import qualified Clash.Explicit.Prelude as E

data EbCommand
  = -- | Disable write, enable read for /n/ cycles
    Drain {n :: Unsigned 32}
  | -- | Enable write, disable read for /n/ cycles
    Fill {n :: Unsigned 32}
  deriving (Generic, NFDataX, Eq, Show, BitPack, BitPackC, ShowX)
deriveTypeDescription ''EbCommand

-- | Like @ebCommand.n@, but only if the command is 'Drain'
toDrainMaybe :: EbCommand -> Maybe (Unsigned 32)
toDrainMaybe (Drain m) = Just m
toDrainMaybe _ = Nothing

-- | Like @ebCommand.n@, but only if the command is 'Fill'
toFillMaybe :: EbCommand -> Maybe (Unsigned 32)
toFillMaybe (Fill m) = Just m
toFillMaybe _ = Nothing

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

data ElasticBufferData a
  = -- | No valid data present, because FIFO was empty
    Empty
  | -- | No valid data preset, because CPU requested a fill
    FillCycle
  | -- | FIFO is in passthrough mode and was not empty
    Data a
  deriving (Generic, NFDataX, Eq, Show)

fromData :: ElasticBufferData a -> a
fromData (Data dat) = dat
fromData FillCycle = errorX "ElasticBufferData.fromData: FillCycle has no data"
fromData Empty = errorX "ElasticBufferData.fromData: Empty has no data"

fromElasticBufferData :: a -> ElasticBufferData a -> a
fromElasticBufferData _ (Data dat) = dat
fromElasticBufferData dflt _ = dflt

toElasticBufferData :: Bool -> Bool -> a -> ElasticBufferData a
toElasticBufferData requestedReadInPreviousCycle underflow a
  | not requestedReadInPreviousCycle = FillCycle
  | underflow = Empty
  | otherwise = Data a

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
  -- | Data to write into the elastic buffer. Will be ignored for a single cycle
  -- when it gets a 'Drain' command. Which cycle this is depends on clock domain
  -- crossing.
  Signal writeDom a ->
  ( Signal readDom (RelDataCount n)
  , Signal readDom Underflow
  , Signal writeDom Overflow
  , Signal readDom (ElasticBufferData a)
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
  , fifoOut
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

  -- We don't reset the Xilinx FIFO: its reset documentation is self-contradictory
  -- and mentions situations where the FIFO can end up in an unrecoverable state.
  noResetWrite = unsafeFromActiveHigh (pure False)
  noResetRead = unsafeFromActiveHigh (pure False)

  -- Muxing between drain and fills:
  commandAck = selectAck <$> command <*> drainAck <*> fillAck
  fifoOut = toElasticBufferData <$> readEnableDelayed <*> isUnderflow <*> fifoData
  readEnableDelayed = E.register clkRead noResetRead enableGen False readEnable
  readEnable = not <$> fill

  selectAck :: Maybe EbCommand -> Ack -> Ack -> Ack
  selectAck cmd dAck fAck = case cmd of
    Just Drain{} -> dAck
    Just Fill{} -> fAck
    Nothing -> errorX "xilinxElasticBuffer: No command to acknowledge"

  -- Fill logic:
  (fillAck, fill) =
    E.mooreB
      clkRead
      noResetRead
      enableGen
      goActState
      goActOutput
      Nothing
      (maybe Nothing toFillMaybe <$> command)

  goActState :: Maybe (Unsigned 32) -> Maybe (Unsigned 32) -> Maybe (Unsigned 32)
  goActState Nothing i = i
  goActState (Just 0) _ = Nothing
  goActState (Just n) _ = Just (n - 1)

  goActOutput :: Maybe (Unsigned 32) -> (Ack, Bool)
  goActOutput Nothing = (Ack False, False)
  goActOutput (Just 0) = (Ack True, False)
  goActOutput (Just _) = (Ack False, True)

  -- Drain logic (CDC based):
  writeData = mux drain (pure Nothing) (Just <$> wdata)
  (drainAckWrite, drain) =
    E.mooreB clkWrite noResetWrite enableGen goActState goActOutput Nothing maybeDrainCmd
  (drainAck, maybeDrainCmd) =
    safeXpmCdcHandshake
      clkRead
      E.noReset
      clkWrite
      E.noReset
      (maybe Nothing toDrainMaybe <$> command)
      drainAckWrite

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
    (BitboneMm readDom addrW)
    ( CSignal readDom (RelDataCount n)
    , CSignal readDom Underflow
    , CSignal readDom Overflow
    , CSignal readDom Stable
    , CSignal readDom (ElasticBufferData a)
    )
xilinxElasticBufferWb clkRead rstRead SNat clkWrite wdata =
  withClockResetEnable clkRead rstRead enableGen $ circuit $ \wb -> do
    [wbCommandPrepare, wbCommandGo, wbCommandWait, wbDataCount, wbUnderflow, wbOverflow, wbStable] <-
      deviceWb "ElasticBuffer" -< wb

    (ebCommand, _ebCommandDfActivity) <-
      registerWbDfI
        (registerConfig "command_prepare")
          { access = WriteOnly
          , description = "Command to execute when setting `command_go`"
          }
        (Drain 0 :: EbCommand)
        -< (wbCommandPrepare, Fwd (pure Nothing))

    (_ebCommandGo, ebCommandGoDfActivity) <-
      registerWbDfI
        (registerConfig "command_go")
          { access = WriteOnly
          , description =
              "Trigger execution of the command set in `command_prepare`. Will stall if a command is still in progress."
          }
        ()
        -< (wbCommandGo, Fwd (pure Nothing))

    (_ebCommandWait, ebCommandWaitDfActivity) <-
      registerWbDfI
        (registerConfig "command_wait")
          { access = WriteOnly
          , description = "Wait until ready to (immediately) accept a new command"
          }
        ()
        -< (wbCommandWait, Fwd (pure Nothing))

    -- See [Note Skid Buffer] below
    ackWhen ebReady -< ebCommandWaitDfActivity

    -- If there is bus activity on the `go` bus, we pass the command saved in `command_prepare`
    -- to the elastic buffer. Otherwise, we pass Nothing.
    ebCommandDf0 <-
      applyC
        (\(maybeBusActivity, ebCommand) -> toEbCommand <$> maybeBusActivity <*> ebCommand)
        (\ack -> (ack, ()))
        -< (ebCommandGoDfActivity, ebCommand)

    -- [Note Skid Buffer]
    --
    -- By putting a skid buffer here, we ensure that we can immediately accept a new command
    -- when writing to `command_go`. We then use the 'ready' signal from the skid buffer to
    -- implement the 'command_wait' register.
    (ebCommandDf1, Fwd ebReady) <- skid -< ebCommandDf0

    let (dataCount, underflow, overflow0, readData, commandAck) =
          xilinxElasticBuffer @n clkRead clkWrite ebCommandSig wdata

    Fwd ebCommandSig <- unsafeFromDf -< (ebCommandDf1, Fwd commandAck)

    -- Synchronize overflow pulse from write domain to read domain
    let overflow1 = xpmCdcPulse clkWrite clkRead overflow0

    (dataCountOut, _dataCountActivity) <-
      registerWbI
        (registerConfig "data_count"){access = ReadOnly}
        0
        -< (wbDataCount, Fwd (Just <$> dataCount))

    (underflowOut, _underflowActivity) <-
      registerWbI
        (registerConfig "underflow")
          { access = ReadWrite
          , description = "Sticky underflow flag; can be cleared by writing false"
          }
        False
        -< (wbUnderflow, Fwd (flip orNothing True <$> underflow))

    (overflowOut, _overflowActivity) <-
      registerWbI
        (registerConfig "overflow")
          { access = ReadWrite
          , description = "Sticky overflow flag; can be cleared by writing false"
          }
        False
        -< (wbOverflow, Fwd (flip orNothing True <$> overflow1))

    -- Stable register: Software can set this to indicate buffer is ready
    (stableOut, _stableActivity) <-
      registerWbI
        (registerConfig "stable"){access = WriteOnly}
        False
        -< (wbStable, Fwd (pure Nothing))

    idC -< (dataCountOut, underflowOut, overflowOut, stableOut, Fwd readData)
 where
  -- If there is bus activity on the `go` bus, we pass the command saved in `command_prepare`
  -- to the elastic buffer. Otherwise, we pass Nothing.
  toEbCommand busActivity ebCommand = maybe Nothing (\_ -> Just ebCommand) busActivity
