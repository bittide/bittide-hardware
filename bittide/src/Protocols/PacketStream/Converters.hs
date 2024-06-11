{-# language RecordWildCards #-}

{-|
Module      : Protocols.PacketStream.Converters
Description : Provides an upconverter and downconverter, for changing the data width of packet streams.
-}
module Protocols.PacketStream.Converters
  ( upConverterC
  , downConverterC
  ) where

import Clash.Prelude

import Protocols ( Circuit(..), fromSignals, (|>) )
import Protocols.PacketStream

import Data.Maybe ( isJust, isNothing )
import Bittide.Extra.Maybe

-- | Upconverter state, consisting of at most p `BitVector 8`s and a vector indicating which bytes are valid
data UpConverterState (dataWidth :: Nat) =
  UpConverterState {
    _ucBuf     :: Vec dataWidth (BitVector 8),
    -- ^ The buffer we are filling
    _ucIdx     :: Index dataWidth,
    -- ^ Where in the buffer we need to write the next element
    _ucFlush   :: Bool,
    -- ^ If this is true the current state can presented as packetstream word
    _ucFreshBuf  :: Bool,
    -- ^ If this is true we need to start a fresh buffer
    _ucAborted :: Bool,
    -- ^ Current packet is aborted
    _ucLastIdx :: Maybe (Index dataWidth)
    -- ^ If true the current buffer contains the last byte of the current packet
  }
  deriving (Generic, NFDataX)

toPacketStream :: UpConverterState dataWidth -> Maybe (PacketStreamM2S dataWidth ())
toPacketStream UpConverterState{..} = orNothing _ucFlush (PacketStreamM2S _ucBuf _ucLastIdx () _ucAborted)

nextState
  :: KnownNat dataWidth
  => UpConverterState dataWidth
  -> Maybe (PacketStreamM2S 1 ())
  -> PacketStreamS2M
  -> UpConverterState dataWidth
nextState st@(UpConverterState {..}) Nothing (PacketStreamS2M inReady)
  = nextSt
    where
      outReady = not _ucFlush || inReady
      -- If we can accept data we can always set _ucFlush to false,
      -- since we only change state if we can transmit and receive data
      nextStRaw = st
                    { _ucFlush = False
                    , _ucAborted = isNothing _ucLastIdx && _ucAborted
                    , _ucLastIdx = Nothing
                    }
      nextSt = if outReady then nextStRaw else st
nextState st@(UpConverterState {..}) (Just PacketStreamM2S{..}) (PacketStreamS2M inReady)
  = nextSt
    where
      inLast = isJust _last
      -- We smear an abort over the entire rest of the packet
      -- so the next abort is set:
      --  - If fragment we are potentially flushing was not the last and we were already aborting;
      --  - or if the incoming fragment is aborted
      nextAbort = (isNothing _ucLastIdx && _ucAborted) || _abort
      -- If we are not flushing we can accept data to be stored in _ucBuf,
      -- but when we are flushing we can only accept if the current
      -- output fragment is accepted by the sink
      outReady = not _ucFlush || inReady
      bufFull = _ucIdx == maxBound
      currBuf = if _ucFreshBuf then (repeat 0) else _ucBuf
      nextBuf = replace _ucIdx (head _data) currBuf

      nextFlush = inLast || bufFull
      nextIdx = if nextFlush then 0 else _ucIdx + 1

      nextStRaw = UpConverterState
                    { _ucBuf =  nextBuf
                    , _ucIdx = nextIdx
                    , _ucFlush = nextFlush
                    , _ucFreshBuf = nextFlush
                    , _ucAborted = nextAbort
                    , _ucLastIdx = orNothing inLast _ucIdx
                    }
      nextSt = if outReady then nextStRaw else st

upConverter
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  =>  KnownNat dataWidth
  => ( Signal dom (Maybe (PacketStreamM2S 1 ()))
     , Signal dom PacketStreamS2M)
  -- ^ Input packet stream from the source
  --   Input backpressure from the sink
  -> ( Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     )
  -- ^ Output backpressure to the source
  --   Output packet stream to the sink
upConverter = mealyB go s0
  where
    s0 = UpConverterState (repeat undefined) 0 False True False Nothing
    go
      :: UpConverterState dataWidth
      -> (Maybe (PacketStreamM2S 1 ()), PacketStreamS2M)
      -> ( UpConverterState dataWidth
         , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth ()))
         )
    go st@(UpConverterState {..}) (fwdIn, bwdIn)
      = (nextState st fwdIn bwdIn, (PacketStreamS2M outReady, toPacketStream st))
        where
          outReady = not _ucFlush || (_ready bwdIn)

-- | Converts packet streams of single bytes to packet streams of a higher data widths.
-- Has one cycle of latency, but optimal throughput.
upConverterC
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Circuit (PacketStream dom 1 ()) (PacketStream dom dataWidth ())
upConverterC = forceResetSanity |> fromSignals upConverter

data DownConverterState (dataWidth :: Nat) =
  DownConverterState {
    _dcBuf :: Vec dataWidth (BitVector 8),
    -- ^ Buffer
    _dcSize :: Index (dataWidth + 1),
    -- ^ Number of valid bytes in _dcBuf
    _dcLastVec :: Bool,
    -- ^ True if last byte of _dcBuf was marked as last byte by incoming stream
    _dcAborted :: Bool
    -- ^ If True, outgoing bytes should be marked as aborted until _dcBuf is replaced
  }
  deriving (Generic, NFDataX)

-- | Computes new state from incoming data
fromPacketStreamM2S
  :: forall (dataWidth :: Nat) .
  KnownNat dataWidth
  => PacketStreamM2S dataWidth ()
  -> DownConverterState dataWidth
fromPacketStreamM2S (PacketStreamM2S vs lastIdx _ aborted) =
  DownConverterState
    { _dcBuf = vs
    , _dcSize = maybe (natToNum @dataWidth) (succ . resize) lastIdx -- lastIdx points to the last valid byte, so the buffer size is one more
    , _dcLastVec = isJust lastIdx
    , _dcAborted = aborted
    }

-- | Computes output of down converter
orNothingPacketStreamM2S
  :: forall (dataWidth :: Nat) .
  1 <= dataWidth
  => KnownNat dataWidth
  => DownConverterState dataWidth
  -> Maybe (PacketStreamM2S 1 ())
orNothingPacketStreamM2S DownConverterState {..} = orNothing (_dcSize > 0) out
  where
    out = PacketStreamM2S
      { _data = leToPlusKN @1 @dataWidth head _dcBuf :> Nil
      , _last = orNothing (_dcSize == 1 && _dcLastVec) 0
      , _meta = ()
      , _abort = _dcAborted
      }

downConverter
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => ( Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     , Signal dom PacketStreamS2M
     )
  -- ^ Input packet stream from the source and backpressure from the sink
  -> ( Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S 1 ()))
     )
  -- ^ Output backpressure to the source
  --   Output packet stream to the sink
downConverter = mealyB go s0
  where
    s0 = DownConverterState
      { _dcBuf = errorX "downConverter: undefined initial value"
      , _dcSize = 0
      , _dcLastVec = False
      , _dcAborted = False
      }
    go
      :: DownConverterState dataWidth
      -> (Maybe (PacketStreamM2S dataWidth ()), PacketStreamS2M)
      -> (DownConverterState dataWidth, (PacketStreamS2M, Maybe (PacketStreamM2S 1 ())))
    go st@(DownConverterState {..}) (fwdIn, PacketStreamS2M inReady) = (st', (bwdOut, fwdOut))
      where
        (_dcSize',_dcBuf')
          = if _dcSize > 0 && inReady
              then (_dcSize - 1, _dcBuf <<+ 0)
              else (_dcSize, _dcBuf)

        -- If the next buffer contains no valid bytes,
        -- and the final byte was acknowledged, we can
        -- acknowledge the newly received data.
        -- The || is lazy, and we need this: if the output
        -- of the downconverter is Nothing, we are not allowed to
        -- evaluate inReady.
        outReady = _dcSize == 0 || (_dcSize == 1 && inReady)
        st' = case fwdIn of
                Just inp | outReady -> fromPacketStreamM2S inp
                _ -> st  { _dcBuf = _dcBuf'
                         , _dcSize = _dcSize'
                         }

        bwdOut = PacketStreamS2M outReady
        fwdOut = orNothingPacketStreamM2S st

-- | Converts packet streams of arbitrary data widths to packet streams of single bytes.
-- Has one clock cycle of latency, but optimal throughput, i.e. a packet of n bytes is
-- sent out in n clock cycles, even if `_last` is set.
downConverterC
  :: forall (dataWidth :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom 1 ())
downConverterC = forceResetSanity |> fromSignals downConverter
