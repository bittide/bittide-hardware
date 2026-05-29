-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Tooling to negotiate a common "start word" in an otherwise unstructured stream of
words.
-}
module Bittide.Handshake where

import Clash.Prelude
import Protocols

import Bittide.ElasticBuffer (ElasticBufferData (..), fromData)
import Clash.Class.BitPackC (BitPackC, ByteOrder)
import Clash.Functor.Extra ((<<$>>))
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import Protocols.Experimental.Wishbone
import Protocols.MemoryMap (Access (..), Mm)
import Protocols.MemoryMap.Mask (fromBitVector, fromVec, toVec)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (..),
  deviceConfig,
  deviceWbI,
  registerConfig,
  registerWbI,
  registerWbI_,
 )
import Protocols.MemoryMap.TypeDescription (deriveTypeDescription)

nextLfsr :: BitVector 8 -> BitVector 8
nextLfsr 0 = 1
nextLfsr current
  | lsb current == 1 = (current `shiftR` 1) `xor` 0xB8
  | otherwise = current `shiftR` 1

{- Magic 'BitVector' used to detect whether a handshake is currently active.
-}
magicConstant :: forall n. (KnownNat n) => BitVector n
magicConstant = resize $ pack num
 where
  num = iterateI nextLfsr 0xAB :: Vec (n `DivRU` 8) (BitVector 8)

{- | Meta information send along with 'magicConstant'. Used to negotiate a common start
word. In practice, this is used to allow hardware UGN capture and ring buffer alignment.
-}
data Meta a = Meta
  { readyToReceive :: Bool
  -- ^ Ready to receive user data
  , lastMetaWord :: Bool
  -- ^ Next transceiver word will be user data, i.e., it won't contain this 'Meta' anymore
  , softwareMeta :: a
  -- ^ Additional information transmitted to the software
  }
  deriving (Generic, NFDataX, BitPack, Eq, Show, Functor)

emptyMeta :: a -> Meta a
emptyMeta softwareMeta =
  Meta
    { readyToReceive = False
    , lastMetaWord = False
    , softwareMeta
    }

{- | Given a @BitVector n@, parse the least @BitSize Meta@ bits as 'Meta', but only if the
rest of the bits correspond to 'magicConstant'.
-}
wordToMeta ::
  forall a n.
  (KnownNat n, BitPack (Meta a), BitSize (Meta a) <= n) =>
  BitVector n ->
  Maybe (Meta a)
wordToMeta word
  -- TODO: There is some worry that we'd be looking at stale data even if we do see the
  --       magic constant. Future implementations could try and detect a sequence of
  --       @magicConstant@ followed by @complement magicConstant@.
  | header == magicConstant = Just $ unpack meta
  | otherwise = Nothing
 where
  (header, meta) = split @_ @(n - BitSize (Meta a)) word

{- | Given a @Meta@, convert it to a @BitVector n@. The resulting 'BitVector' will have
its LSBs set to @Meta@, with its MSBs set to 'magicConstant'.
-}
metaToWord ::
  forall a n.
  (KnownNat n, BitPack (Meta a), BitSize (Meta a) <= n) =>
  Meta a ->
  BitVector n
metaToWord meta = magicConstant @(n - BitSize (Meta a)) ++# pack meta

data Status
  = -- | Negotiating transition to post-handshake data (or in reset)
    Negotiating
  | -- | Next word is going to be post-handshake
    Last
  | -- | Current word is post-handshake
    PostHandshake
  deriving (Generic, Show, Eq, NFDataX, BitPack, BitPackC)
deriveTypeDescription ''Status

{- | A circuit negotiating a common "first word" boundary on an unstructured stream of
words. In a bittide context, this typically sits behind the elastic buffer and before
the transmit channel of a transceiver. Also in bittide context, the purpose of a "first
word" negotiation is two-fold:

  1. Implement a mini-protocol that states that the "first word" contains the local
     counter of a node. This is used to construct a clock cycle to clock cycle mapping
     from one node to another (the main thesis of bittide).

  2. Implement hardware accelerated ring buffer alignment. For various reasons, bittide
     systems communicate over ring buffers when in asynchronous mode. In a system that
     has achieved syntony, the transmit buffer of one node will move in lockstep with
     another. This is a very nice feature as it means that when a transmit buffer is
     stable, a receive buffer will be too and read the exact same words. In this case
     a "first word" negotiation negotiates the first clock cycle at which the ring
     buffers start turning.

It works by detecting 'magicWord' in the words it receives and decoding a few bits of
'Meta' information.
-}
handshake ::
  forall a n dom.
  ( KnownNat n
  , BitPack a
  , NFDataX a
  , BitSize (Meta a) <= n
  , HiddenClock dom
  ) =>
  Reset dom ->
  -- | From neighbor
  Signal dom (ElasticBufferData (BitVector n)) ->
  -- | Receive OK?
  Signal dom Bool ->
  -- | Additional metadata to send to neighbor
  Signal dom a ->
  ( -- To neighbor
    Signal dom (BitVector n)
  , -- Receive status (neighbor to core)
    Signal dom Status
  , -- Transmit status (core to neighbor)
    Signal dom Status
  , -- Additional metadata received from neighbor
    Signal dom (Maybe a)
  )
handshake reset wordIn receiveEnable userMeta =
  ( wordOut
  , neighborToCoreStatus
  , coreToNeighborStatus
  , (.softwareMeta) <<$>> maybeNeighborMeta
  )
 where
  withResetEnable :: ((HiddenReset dom, HiddenEnable dom) => r) -> r
  withResetEnable r = withReset reset $ withEnable enableGen $ r

  prevReceiveStatus = withResetEnable $ register Negotiating neighborToCoreStatus
  neighborToCoreStatus = mkStatus <$> maybeNeighborMeta <*> prevReceiveStatus

  prevCoreToNeighborStatus = withResetEnable $ register Negotiating coreToNeighborStatus
  coreToNeighborStatus = mkStatus <$> fmap Just transmitMeta <*> prevCoreToNeighborStatus
  transmitMeta =
    mkMeta
      <$> neighborToCoreStatus
      <*> maybeNeighborMeta
      <*> userMeta
      <*> receiveEnable

  resetAsBool = unsafeToActiveHigh reset
  wordOut = metaToWord @a <$> mux resetAsBool (emptyMeta <$> userMeta) transmitMeta
  maybeNeighborMeta =
    mux
      resetAsBool
      (pure Nothing)
      (withReset reset $ elasticBufferWordToMeta @a wordIn)
{-# OPAQUE handshake #-}

{- | Convert each incoming 'ElasticBufferData' word to @'Maybe' ('Meta' a)@. The result is
registered, so that on cycles where the elastic buffer does not produce valid data (or the
word does not decode to 'Meta') the previous cycle's 'Meta' is used as a fallback via
'<|>'.
-}
elasticBufferWordToMeta ::
  forall a n dom.
  ( KnownNat n
  , BitPack a
  , NFDataX a
  , BitSize (Meta a) <= n
  , HiddenClock dom
  , HiddenReset dom
  ) =>
  Signal dom (ElasticBufferData (BitVector n)) ->
  Signal dom (Maybe (Meta a))
elasticBufferWordToMeta wordIn = current
 where
  (.<|>.) = liftA2 (<|>)
  prev = withEnable enableGen $ register Nothing current
  current = fmap decode wordIn .<|>. prev

  decode (Data w) = wordToMeta w
  decode _ = Nothing

mkStatus ::
  -- | Meta from neighbor or ourselves
  Maybe (Meta a) ->
  -- | Previous status
  Status ->
  -- | Updated status
  Status
mkStatus maybeMeta prevStatus =
  case prevStatus of
    Last -> PostHandshake
    PostHandshake -> PostHandshake
    Negotiating
      | Just meta <- maybeMeta, meta.lastMetaWord -> Last
      | otherwise -> Negotiating

mkMeta :: Status -> Maybe (Meta a) -> a -> Bool -> Meta a
mkMeta neighborToCoreStatus maybeNeighborMeta softwareMeta readyToReceive =
  Meta
    { softwareMeta
    , readyToReceive
    , -- Note: we don't send the last word until we're ready to receive a word ourselves. We
      -- do this, because if we'd send the last word we'd have no way to communicate whether
      -- we're ready anymore.
      lastMetaWord = neighborIsReadyToReceive && readyToReceive
    }
 where
  neighborIsReadyToReceive =
    case neighborToCoreStatus of
      Last -> True
      PostHandshake -> True
      _ ->
        case maybeNeighborMeta of
          Just neighborMeta -> neighborMeta.readyToReceive
          Nothing -> False

data Inputs dom n = Inputs
  { fromNeighbors :: Vec n (Signal dom (ElasticBufferData (BitVector 64)))
  , fromCores :: Vec n (Signal dom (BitVector 64))
  }

instance Protocol (Inputs dom n) where
  type Fwd (Inputs dom n) = Inputs dom n
  type Bwd (Inputs dom n) = ()

data Outputs dom n = Outputs
  { toNeighbors :: Vec n (Signal dom (BitVector 64))
  -- ^ Data to be propagated to the neighbor (typically transceiver IP)
  , toCores :: Vec n (Signal dom (BitVector 64))
  -- ^ Data to be propagated to the core (typically UGN capture)
  , toNeighborDones :: Vec n (Signal dom Bool)
  -- ^ Whether the data in 'toNeighbor' is post-handshake data
  , toCoreDones :: Vec n (Signal dom Bool)
  -- ^ Whether the data in 'toCore' is post-handshake data
  , fromNeighborDones :: Vec n (Signal dom Bool)
  -- ^ Whether the data in 'Inputs.fromNeighbor' is propagated to the core as post-handshake data
  , fromCoreDones :: Vec n (Signal dom Bool)
  -- ^ Whether the data in 'Inputs.fromCore' is propagated to the neighbor as post-handshake data
  }

instance Protocol (Outputs dom n) where
  type Fwd (Outputs dom n) = Outputs dom n
  type Bwd (Outputs dom n) = ()

data Mode
  = {- | Register data from the core to the neighbor and vice versa. While this mode is
    active, the handshake logic has its reset asserted.
    -}
    PassThrough
  | -- | Handshake module engaged
    Enabled
  deriving (Generic, Show, Eq, NFDataX, BitPack, BitPackC)
deriveTypeDescription ''Mode

data Input dom = Input
  { mode :: Signal dom Mode
  , receiveReady :: Signal dom Bool
  , softwareReady :: Signal dom Bool
  , fromNeighbor :: Signal dom (BitVector 64)
  , fromCore :: Signal dom (BitVector 64)
  }

data Output dom = Output
  { toNeighbor :: Signal dom (BitVector 64)
  , toCore :: Signal dom (BitVector 64)
  , toNeighborDone :: Signal dom Bool
  , toCoreDone :: Signal dom Bool
  , fromNeighborDone :: Signal dom Bool
  , fromCoreDone :: Signal dom Bool
  , neighborSoftwareReady :: Signal dom Bool
  , handshakeDone :: Signal dom Bool
  }

perChannel ::
  (HiddenClock dom) =>
  Signal dom Mode ->
  Signal dom Bool ->
  Signal dom Bool ->
  Signal dom (ElasticBufferData (BitVector 64)) ->
  Signal dom (BitVector 64) ->
  Output dom
perChannel mode receiveReady softwareReady fromNeighbor fromCore =
  Output
    { toNeighbor = dflipflop toNeighbor
    , toCore = dflipflop (fromData <$> fromNeighbor)
    , toNeighborDone = dflipflop coreToNeighborDone
    , toCoreDone = dflipflop neighborToCoreDone
    , -- Software only:
      neighborSoftwareReady = dflipflop neighborSoftwareReady
    , handshakeDone = dflipflop (neighborToCoreDone .&&. coreToNeighborDone)
    , -- These signals relate to input signals, so we can't test for "done", because we'd
      -- be too late if we register it. We therefore jump a cycle ahead by testing for
      -- 'Last' too.
      fromNeighborDone = dflipflop (neighborToCoreStatus .== Last .||. neighborToCoreDone)
    , fromCoreDone = dflipflop (coreToNeighborStatus .== Last .||. coreToNeighborDone)
    }
 where
  handshakeReset = unsafeFromActiveHigh (mode .== PassThrough)
  (handshakeToNeighbor, neighborToCoreStatus, coreToNeighborStatus, maybeSoftwareReady) =
    handshake
      handshakeReset
      fromNeighbor
      receiveReady
      softwareReady

  neighborToCoreDone = neighborToCoreStatus .== PostHandshake
  coreToNeighborDone = coreToNeighborStatus .== PostHandshake
  neighborSoftwareReady = fromMaybe False <$> maybeSoftwareReady
  toNeighbor = mux (mode .== PassThrough .||. coreToNeighborDone) fromCore handshakeToNeighbor

handshakesWb ::
  forall n dom addrW nBytes.
  ( HasCallStack
  , HiddenClock dom
  , HiddenReset dom
  , KnownNat n
  , KnownNat addrW
  , KnownNat nBytes
  , 1 <= nBytes
  , ?byteOrder :: ByteOrder
  ) =>
  Circuit
    ((ToConstBwd Mm, Wishbone dom 'Standard addrW nBytes), Inputs dom n)
    (Outputs dom n)
handshakesWb = circuit $ \(bus, Fwd inputs) -> do
  [ modesBus
    , receiveReadysBus
    , softwareReadysBus
    , neighborSoftwareReadysBus
    , neighborToCoreDonesBus
    , coreToNeighborDonesBus
    , handshakeDonesBus
    ] <-
    deviceWbI (deviceConfig "Handshakes") -< bus

  Fwd (modes, _) <-
    registerWbI modesConfig (repeat PassThrough) -< (modesBus, Fwd noWrite)
  Fwd (receiveReadys, _) <-
    registerWbI receiveReadysConfig zeroMask -< (receiveReadysBus, Fwd noWrite)
  Fwd (softwareReadys, _) <-
    registerWbI softwareReadysConfig zeroMask -< (softwareReadysBus, Fwd noWrite)

  registerWbI_ neighborSoftwareReadysConfig zeroMask
    -< (neighborSoftwareReadysBus, Fwd (Just <$> neighborSoftwareReadysMask))
  registerWbI_ neighborToCoreDonesConfig zeroMask
    -< (neighborToCoreDonesBus, Fwd (Just <$> neighborToCoreDonesMask))
  registerWbI_ coreToNeighborDonesConfig zeroMask
    -< (coreToNeighborDonesBus, Fwd (Just <$> coreToNeighborDonesMask))
  registerWbI_ handshakeDonesConfig zeroMask
    -< (handshakeDonesBus, Fwd (Just <$> handshakeDonesMask))

  let
    modesV = unbundle modes
    receiveReadysV = unbundle (toVec <$> receiveReadys)
    softwareReadysV = unbundle (toVec <$> softwareReadys)

    outputs =
      zipWith5
        perChannel
        modesV
        receiveReadysV
        softwareReadysV
        inputs.fromNeighbors
        inputs.fromCores

    toNeighbors = map (.toNeighbor) outputs
    toCores = map (.toCore) outputs
    toNeighborDones = map (.toNeighborDone) outputs
    toCoreDones = map (.toCoreDone) outputs
    fromNeighborDones = map (.fromNeighborDone) outputs
    fromCoreDones = map (.fromCoreDone) outputs
    neighborSoftwareReadys = map (.neighborSoftwareReady) outputs
    handshakeDones = map (.handshakeDone) outputs

    neighborSoftwareReadysMask = fromVec <$> bundle neighborSoftwareReadys
    neighborToCoreDonesMask = fromVec <$> bundle toCoreDones
    coreToNeighborDonesMask = fromVec <$> bundle toNeighborDones
    handshakeDonesMask = fromVec <$> bundle handshakeDones

  -- Note that all flip-flopping is done inside 'perChannel' and all signals routed outside
  -- come directly from it. This is on purpose to trivially show that:
  --
  --  * All outputs are registered
  --  * The delay through this component is always the same, enabled or not
  idC
    -< Fwd
      ( Outputs
          { toNeighbors
          , toCores
          , toNeighborDones
          , toCoreDones
          , fromNeighborDones
          , fromCoreDones
          }
      )
 where
  noWrite = pure Nothing
  zeroMask = fromBitVector 0

  modesConfig =
    (registerConfig "modes")
      { description =
          "Per-channel mode. Whether to engage the handshake module or not. In both modes, this device introduces a single cycle of latency. To renegotiate post handshake, set the mode to 'PassThrough' and back to 'Enabled'. Keep in mind that you should clear the other control registers when a channel is in 'PassThrough' mode again."
      }

  receiveReadysConfig = registerConfig "receive_readys"

  softwareReadysConfig =
    (registerConfig "software_readys")
      { description =
          "Per-channel extra field sent to the neighbor while negotiating to indicate whether software has reached a state where it will soon assert 'receive_readys'. In a bittide context, this means that elastic buffers aren't actively (destructively) centered anymore, making it safe to end the handshake and send over UGNs. The neighbor will send this field too, which will end up in 'neighbor_software_readys'. Once a channel's bit is asserted, it should not be deasserted anymore."
      }

  neighborSoftwareReadysConfig =
    (registerConfig "neighbor_software_readys")
      { access = ReadOnly
      , description =
          "Per-channel extra field received from the neighbor, see 'software_readys'. A channel's bit is only valid when its corresponding entry in 'modes' is set to 'Enabled'."
      }

  neighborToCoreDonesConfig =
    (registerConfig "receive_dones")
      { access = ReadOnly
      , description =
          "Per-channel state of the handshake of the receive channel. A channel's bit is only valid when its corresponding entry in 'modes' is set to 'Enabled'."
      }

  coreToNeighborDonesConfig =
    (registerConfig "transmit_dones")
      { access = ReadOnly
      , description =
          "Per-channel state of the handshake of the transmit channel. A channel's bit is only valid when its corresponding entry in 'modes' is set to 'Enabled'."
      }

  handshakeDonesConfig =
    (registerConfig "handshake_dones")
      { access = ReadOnly
      , description =
          "Per-channel indicator that both the receive and transmit channels have completed their handshake negotiation."
      }
