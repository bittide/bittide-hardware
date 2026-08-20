-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | Credit\/valid-ack link layer for the decode demo's hardware-asynchronous
variant (B): the same streaming-reduce datapath as the scheduled variant, but
transfers are initiated by flow control instead of a calendar. No CPU is
involved anywhere.

A transfer is a HEADER word (magic + sequence number) followed by
@vector_words@ payload words, sent on the ring's forward direction. The
receiver returns a CREDIT word on the reverse direction of the same physical
link after it can accept the next frame. One credit is outstanding per link;
every node starts holding one credit for its downstream neighbor.

Frame discrimination is positional: only the first word after idle is matched
against the header magic; payload words are counted, never inspected. Idle
links carry the local counter (the processing element's debug convention),
which stays below @2^48@ for weeks of uptime, so a magic in the top 16 bits
cannot alias idle traffic. The host asserts this before arming.

Two relay disciplines, selected at runtime:

* __Cut-through__ (default): a relay regenerates the header one cycle after
  detecting it and streams the payload through the one-register reduce stage —
  ~2 cycles of framing per hop. The credit round trip is off the critical path
  whenever the ring's dependency chain is longer than one hop.
* __Store-and-forward__ (@cut_through = False@): a relay buffers the complete
  frame, then requests forwarding, which blocks on the credit; the credit for
  the upstream neighbor is only granted after the relay's own forward
  completes. This exposes the per-hop handshake on the critical path, the
  discipline of fabrics that validate frames per hop.
-}
module Bittide.CreditLink (
  -- * Configuration and status
  CreditLinkKnobs (..),
  CreditLinkStatus (..),

  -- * Link layer
  creditLink,
  CreditLinkOut (..),

  -- * Wishbone device
  creditLinkConfig,

  -- * Word encodings (exposed for tests and the host driver)
  headerMagic,
  creditMagic,
  mkHeaderWord,
  mkCreditWord,
  isHeaderWord,
  isCreditWord,
  wordSeq,
) where

import Clash.Prelude
import Protocols

import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols.Experimental.Wishbone (Wishbone, WishboneMode (Standard))
import Protocols.MemoryMap (Access (ReadOnly, ReadWrite), Mm)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access),
  deviceConfig,
  deviceWbI,
  registerConfig,
  registerWbI,
  registerWbI_,
 )
import Protocols.MemoryMap.Registers.WishboneStandard.Internal (RegisterWb)

import Bittide.DecodeProcessingElement (
  DecodePeSettings (..),
  FireInfo (..),
  Lap (..),
  LapResult (..),
  PeMode (..),
  Role (..),
  StreamOut (..),
 )

import Data.Maybe (fromMaybe, isJust)

-- | Top 16 bits of a frame header word.
headerMagic :: BitVector 16
headerMagic = 0xB17D

-- | Top 16 bits of a credit word.
creditMagic :: BitVector 16
creditMagic = 0xC4ED

mkHeaderWord :: Unsigned 32 -> BitVector 64
mkHeaderWord seqNr = headerMagic ++# (0 :: BitVector 16) ++# pack seqNr

mkCreditWord :: Unsigned 32 -> BitVector 64
mkCreditWord seqNr = creditMagic ++# (0 :: BitVector 16) ++# pack seqNr

isHeaderWord :: BitVector 64 -> Bool
isHeaderWord w = slice d63 d48 w == headerMagic

isCreditWord :: BitVector 64 -> Bool
isCreditWord w = slice d63 d48 w == creditMagic

wordSeq :: BitVector 64 -> Unsigned 32
wordSeq = unpack . slice d31 d0

-- | Host-tunable knobs.
data CreditLinkKnobs = CreditLinkKnobs
  { creditTimeout :: Unsigned 32
  {- ^ Cycles a blocked sender waits for a credit before force-restoring it
  (deadlock recovery; 0 disables). Recoveries are counted.
  -}
  , creditPulseLen :: Unsigned 8
  -- ^ Cycles the credit word is repeated (loss tolerance; edge-deduplicated).
  , cutThrough :: Bool
  -- ^ 'True': cut-through relays; 'False': store-and-forward relays.
  }
  deriving (Generic, NFDataX)

-- | Hardware-maintained status, mirrored into read-only registers.
data CreditLinkStatus = CreditLinkStatus
  { creditsConsumed :: Unsigned 32
  , creditsReturned :: Unsigned 32
  , creditsGranted :: Unsigned 32
  , framesReceived :: Unsigned 32
  , headerErrors :: Unsigned 32
  , creditErrors :: Unsigned 32
  , noCreditDrops :: Unsigned 32
  , timeoutCount :: Unsigned 32
  , minCreditRtt :: Unsigned 32
  , maxCreditRtt :: Unsigned 32
  , lastCreditRtt :: Unsigned 32
  }
  deriving (Generic, NFDataX)

-- | Everything the credit link drives.
data CreditLinkOut = CreditLinkOut
  { fire :: Maybe FireInfo
  -- ^ Window starts for the reduce core
  , coreRxWord :: BitVector 64
  -- ^ The core's receive stream (live tap or store-and-forward replay)
  , forwardTx :: Maybe (BitVector 64)
  -- ^ Word for the forward (downstream) link; 'Nothing' = idle
  , creditTx :: Maybe (BitVector 64)
  -- ^ Word for the reverse (upstream) link; 'Nothing' = idle
  , status :: CreditLinkStatus
  , rttSample :: Maybe (Unsigned 32)
  -- ^ Per-transfer credit round-trip time samples
  }
  deriving (Generic, NFDataX)

data RxFsm
  = RxIdle
  | -- | Receiving payload; live roles fire the core on the first word.
    RxRecv
      { seqOut :: Unsigned 32
      , role :: Role
      , lap :: Lap
      , wordIdx :: Unsigned 16
      , buffered :: Bool
      }
  | -- | Store-and-forward: frame complete, waiting for the sender to accept.
    RxAwaitFwd {seqOut :: Unsigned 32, role :: Role, lap :: Lap}
  | -- | Store-and-forward: replaying the buffered frame into the core.
    RxReplay {seqOut :: Unsigned 32, role :: Role, lap :: Lap, wordIdx :: Unsigned 16}
  | -- | Driving the credit word toward the upstream neighbor.
    RxGrant {seqDone :: Unsigned 32, pulseIdx :: Unsigned 8}
  deriving (Generic, NFDataX)

data RxState = RxState
  { rxFsm :: RxFsm
  , rxExpectedSeq :: Unsigned 32
  , rxFramesReceived :: Unsigned 32
  , rxHeaderErrors :: Unsigned 32
  , rxCreditsGranted :: Unsigned 32
  }
  deriving (Generic, NFDataX)

data TxFsm
  = TxIdle
  | {- | Store-and-forward: wait one cycle (the receiver learns of the
    acceptance through a register), then emit the header, so the payload
    from the replaying core lands exactly one cycle after the header.
    -}
    TxHeaderPend {seqNr :: Unsigned 32, emitNow :: Bool}
  | TxStream {wordIdx :: Unsigned 16}
  deriving (Generic, NFDataX)

data TxState = TxState
  { txFsm :: TxFsm
  , txCreditHeld :: Bool
  , txSendCycle :: Unsigned 64
  , txPrevCreditIn :: Bool
  , txBlockedFor :: Unsigned 32
  , txCreditsConsumed :: Unsigned 32
  , txCreditsReturned :: Unsigned 32
  , txCreditErrors :: Unsigned 32
  , txNoCreditDrops :: Unsigned 32
  , txTimeoutCount :: Unsigned 32
  , txMinRtt :: Unsigned 32
  , txMaxRtt :: Unsigned 32
  , txLastRtt :: Unsigned 32
  }
  deriving (Generic, NFDataX)

data InjFsm
  = InjWaitLaunch
  | InjRequesting
  | InjWaitSink
  | InjGap {remaining :: Unsigned 32}
  | InjDone
  deriving (Generic, NFDataX)

data InjState = InjState
  { injFsm :: InjFsm
  , injSeqNr :: Unsigned 32
  , injLayerIdx :: Unsigned 16
  , injTokensStarted :: Unsigned 32
  }
  deriving (Generic, NFDataX)

-- Internal per-cycle output bundles.

data RxOut = RxOut
  { rxFire :: Maybe FireInfo
  , rxRelayReq :: Maybe (Unsigned 32)
  -- ^ Cut-through: forward now (header this cycle)
  , rxFwdReq :: Maybe (Unsigned 32)
  -- ^ Store-and-forward: request to forward (level)
  , rxCreditWord :: Maybe (BitVector 64)
  , rxRamWrite :: Maybe (Index 64, BitVector 64)
  , rxRamReadAddr :: Index 64
  , rxReplaying :: Bool
  , rxStat :: (Unsigned 32, Unsigned 32, Unsigned 32)
  -- ^ (frames received, header errors, credits granted)
  }
  deriving (Generic, NFDataX)

data TxCounts = TxCounts
  { tcConsumed :: Unsigned 32
  , tcReturned :: Unsigned 32
  , tcCreditErrors :: Unsigned 32
  , tcDrops :: Unsigned 32
  , tcTimeouts :: Unsigned 32
  , tcMinRtt :: Unsigned 32
  , tcMaxRtt :: Unsigned 32
  , tcLastRtt :: Unsigned 32
  }
  deriving (Generic, NFDataX)

data TxOut = TxOut
  { txForwardWord :: Maybe (BitVector 64)
  , txInjectFire :: Maybe FireInfo
  , txRttSample :: Maybe (Unsigned 32)
  , txStat :: TxCounts
  }
  deriving (Generic, NFDataX)

idleRxOut :: RxState -> RxOut
idleRxOut s =
  RxOut
    { rxFire = Nothing
    , rxRelayReq = Nothing
    , rxFwdReq = Nothing
    , rxCreditWord = Nothing
    , rxRamWrite = Nothing
    , rxRamReadAddr = 0
    , rxReplaying = False
    , rxStat = (s.rxFramesReceived, s.rxHeaderErrors, s.rxCreditsGranted)
    }

idleTxOut :: TxState -> TxOut
idleTxOut s =
  TxOut
    { txForwardWord = Nothing
    , txInjectFire = Nothing
    , txRttSample = Nothing
    , txStat =
        TxCounts
          { tcConsumed = s.txCreditsConsumed
          , tcReturned = s.txCreditsReturned
          , tcCreditErrors = s.txCreditErrors
          , tcDrops = s.txNoCreditDrops
          , tcTimeouts = s.txTimeoutCount
          , tcMinRtt = s.txMinRtt
          , tcMaxRtt = s.txMaxRtt
          , tcLastRtt = s.txLastRtt
          }
    }

{- | The credit link layer. Watches the raw receive taps, fires the reduce
core, drives the forward and reverse directions of the node's ring links and
keeps the flow-control accounting.

Timing contract with 'Bittide.DecodeProcessingElement.decodeReduceCore' (which
emits its transmit stream one cycle after consuming): a relayed frame leaves
exactly one header-regeneration cycle plus the one-register reduce stage after
it arrives — ~2 cycles of framing per cut-through hop.
-}
creditLink ::
  forall dom linkCount.
  ( HasCallStack
  , HiddenClock dom
  , KnownNat linkCount
  , 1 <= linkCount
  ) =>
  -- | Reset (business-logic reset)
  Reset dom ->
  -- | Local counter
  Signal dom (Unsigned 64) ->
  -- | Settings (links, roles, workload shape)
  Signal dom (DecodePeSettings linkCount) ->
  -- | Knobs
  Signal dom CreditLinkKnobs ->
  -- | Arm pulse: clear status, restart the injector chain
  Signal dom Bool ->
  -- | Raw receive taps (all links)
  Signal dom (Vec linkCount (BitVector 64)) ->
  -- | Reduce-core output (transmit stream + lap results)
  Signal dom StreamOut ->
  Signal dom CreditLinkOut
creditLink rst localCounter settings knobs armPulse rxs coreOut =
  CreditLinkOut
    <$> fire
    <*> coreRxWord
    <*> ((.txForwardWord) <$> txOut)
    <*> ((.rxCreditWord) <$> rxOut)
    <*> combinedStatus
    <*> ((.txRttSample) <$> txOut)
 where
  withCrst :: forall a. ((HiddenClockResetEnable dom) => a) -> a
  withCrst f = withClockResetEnable hasClock rst enableGen f

  forwardRx = liftA2 (\cfg v -> v !! fromMaybe (0 :: Index linkCount) cfg.readLink) settings rxs
  creditRxWord = liftA2 (\cfg v -> v !! fromMaybe (0 :: Index linkCount) cfg.writeLink) settings rxs

  sinkDone =
    (\so -> maybe False (\r -> r.role == RoleSink) so.lapResult) <$> coreOut

  -- Store-and-forward frame buffer; written during reception, replayed
  -- (asynchronous read) into the core.
  ramOut =
    withCrst
      $ asyncRam d64 ((.rxRamReadAddr) <$> rxOut) ((.rxRamWrite) <$> rxOut)

  rxOut =
    withCrst
      $ mealy
        goRx
        RxState
          { rxFsm = RxIdle
          , rxExpectedSeq = 0
          , rxFramesReceived = 0
          , rxHeaderErrors = 0
          , rxCreditsGranted = 0
          }
        (bundle (settings, knobs, armPulse, forwardRx, fwdAcceptedR))

  txMealyOut =
    withCrst
      $ mealy
        goTx
        TxState
          { txFsm = TxIdle
          , txCreditHeld = True
          , txSendCycle = 0
          , txPrevCreditIn = False
          , txBlockedFor = 0
          , txCreditsConsumed = 0
          , txCreditsReturned = 0
          , txCreditErrors = 0
          , txNoCreditDrops = 0
          , txTimeoutCount = 0
          , txMinRtt = maxBound
          , txMaxRtt = 0
          , txLastRtt = 0
          }
        ( bundle
            ( knobs
            , armPulse
            , creditRxWord
            , (.rxRelayReq) <$> rxOut
            , (.rxFwdReq) <$> rxOut
            , injReq
            , (.txWord) <$> coreOut
            , localCounter
            )
        )
  txOut = (\(o, _, _) -> o) <$> txMealyOut
  fwdAccepted = (\(_, a, _) -> a) <$> txMealyOut
  injAccepted = (\(_, _, a) -> a) <$> txMealyOut

  -- Registered before consumption: the receive machine and the injector
  -- controller both feed requests into the transmit machine combinationally,
  -- so reading the acceptance back in the same cycle would form an
  -- evaluation cycle.
  fwdAcceptedR = withCrst $ register False fwdAccepted
  injAcceptedR = withCrst $ register False injAccepted

  injReq =
    withCrst
      $ mealy
        goInj
        InjState{injFsm = InjWaitLaunch, injSeqNr = 0, injLayerIdx = 0, injTokensStarted = 0}
        (bundle (settings, armPulse, localCounter, injAcceptedR, sinkDone))

  fire = liftA2 (\r t -> orElse r.rxFire t.txInjectFire) rxOut txOut
  coreRxWord =
    (\r live ram -> if r.rxReplaying then ram else live) <$> rxOut <*> forwardRx <*> ramOut

  combinedStatus = liftA2 mkStatus rxOut txOut
  mkStatus :: RxOut -> TxOut -> CreditLinkStatus
  mkStatus r t =
    CreditLinkStatus
      { creditsConsumed = t.txStat.tcConsumed
      , creditsReturned = t.txStat.tcReturned
      , creditsGranted = granted
      , framesReceived = frames
      , headerErrors = hdrErrs
      , creditErrors = t.txStat.tcCreditErrors
      , noCreditDrops = t.txStat.tcDrops
      , timeoutCount = t.txStat.tcTimeouts
      , minCreditRtt = t.txStat.tcMinRtt
      , maxCreditRtt = t.txStat.tcMaxRtt
      , lastCreditRtt = t.txStat.tcLastRtt
      }
   where
    (frames, hdrErrs, granted) = r.rxStat

  orElse :: forall a. Maybe a -> Maybe a -> Maybe a
  orElse (Just x) _ = Just x
  orElse Nothing y = y

  -- The role of an incoming frame, from the receiving node's position and
  -- the sequence number's lap bit.
  frameRole :: Bool -> Unsigned 32 -> (Role, Lap)
  frameRole isInjector seqNr = case (isInjector, testBit seqNr 0) of
    (True, False) -> (RolePassRelay, Lap2) -- turnaround: lap-1 total enters lap 2
    (True, True) -> (RoleSink, Lap2)
    (False, False) -> (RoleAddRelay, Lap1)
    (False, True) -> (RolePassRelay, Lap2)

  -- Whether an incoming frame is forwarded at all (the injector's sink
  -- window terminates the token's traffic).
  forwards :: Role -> Bool
  forwards RoleSink = False
  forwards _ = True

  -- The sequence number the node forwards: the turnaround bumps the frame
  -- into lap 2; relays forward unchanged.
  forwardSeq :: Bool -> Unsigned 32 -> Unsigned 32
  forwardSeq isInjector seqNr
    | isInjector = seqNr + 1
    | otherwise = seqNr

  goRx ::
    RxState ->
    ( DecodePeSettings linkCount
    , CreditLinkKnobs
    , Bool
    , BitVector 64
    , Bool
    ) ->
    (RxState, RxOut)
  goRx s (cfg, kn, arm, rx, accepted)
    | arm =
        ( s{rxFsm = RxIdle, rxExpectedSeq = 0, rxFramesReceived = 0, rxHeaderErrors = 0, rxCreditsGranted = 0}
        , idleRxOut s
        )
    | otherwise = case s.rxFsm of
        RxIdle
          | isHeaderWord rx ->
              let
                seqNr = wordSeq rx
                seqOk = seqNr == s.rxExpectedSeq
                (role, lap) = frameRole cfg.isInjector seqNr
                -- Live streaming (cut-through, or any non-forwarding role):
                -- the core is fired next cycle, aligned with payload word 0.
                live = kn.cutThrough || not (forwards role)
               in
                ( s
                    { rxFsm =
                        RxRecv
                          { seqOut = forwardSeq cfg.isInjector seqNr
                          , role
                          , lap
                          , wordIdx = 0
                          , buffered = not live
                          }
                    , rxExpectedSeq = seqNr + 1
                    , rxHeaderErrors = if seqOk then s.rxHeaderErrors else s.rxHeaderErrors + 1
                    }
                , idleRxOut s
                )
          | otherwise -> (s, idleRxOut s)
        RxRecv{seqOut, role, lap, wordIdx, buffered} ->
          let
            firstWord = wordIdx == 0
            lastWord = wordIdx == cfg.vectorWords - 1
            out =
              (idleRxOut s)
                { rxFire =
                    if firstWord && not buffered
                      then Just FireInfo{lap, role}
                      else Nothing
                , rxRelayReq =
                    if firstWord && not buffered && forwards role
                      then Just seqOut
                      else Nothing
                , rxRamWrite =
                    if buffered
                      then Just (truncateIdx wordIdx, rx)
                      else Nothing
                }
            nextFsm
              | not lastWord = RxRecv{seqOut, role, lap, wordIdx = wordIdx + 1, buffered}
              | buffered = RxAwaitFwd{seqOut, role, lap}
              | otherwise = RxGrant{seqDone = seqOut, pulseIdx = 0}
            s' =
              s
                { rxFsm = nextFsm
                , rxFramesReceived =
                    if lastWord then s.rxFramesReceived + 1 else s.rxFramesReceived
                }
           in
            (s', out)
        RxAwaitFwd{seqOut, role, lap}
          | accepted ->
              (s{rxFsm = RxReplay{seqOut, role, lap, wordIdx = 0}}, (idleRxOut s){rxFwdReq = Just seqOut})
          | otherwise -> (s, (idleRxOut s){rxFwdReq = Just seqOut})
        RxReplay{seqOut, role, lap, wordIdx} ->
          let
            firstWord = wordIdx == 0
            lastWord = wordIdx == cfg.vectorWords - 1
            out =
              (idleRxOut s)
                { rxFire = if firstWord then Just FireInfo{lap, role} else Nothing
                , rxRamReadAddr = truncateIdx wordIdx
                , rxReplaying = True
                }
            nextFsm
              | lastWord = RxGrant{seqDone = seqOut, pulseIdx = 0}
              | otherwise = RxReplay{seqOut, role, lap, wordIdx = wordIdx + 1}
           in
            (s{rxFsm = nextFsm}, out)
        RxGrant{seqDone, pulseIdx} ->
          let
            lastPulse = pulseIdx + 1 >= kn.creditPulseLen
            out = (idleRxOut s){rxCreditWord = Just (mkCreditWord seqDone)}
            s' =
              s
                { rxFsm =
                    if lastPulse
                      then RxIdle
                      else RxGrant{seqDone, pulseIdx = pulseIdx + 1}
                , rxCreditsGranted =
                    if pulseIdx == 0 then s.rxCreditsGranted + 1 else s.rxCreditsGranted
                }
           in
            (s', out)

  truncateIdx :: Unsigned 16 -> Index 64
  truncateIdx = unpack . resize . pack

  goTx ::
    TxState ->
    ( CreditLinkKnobs
    , Bool
    , BitVector 64
    , Maybe (Unsigned 32)
    , Maybe (Unsigned 32)
    , Maybe (Unsigned 32)
    , Maybe (BitVector 64)
    , Unsigned 64
    ) ->
    (TxState, (TxOut, Bool, Bool))
  goTx s (kn, arm, creditIn, relayReq, fwdReq, injectReq, coreTx, counter)
    | arm =
        ( s
            { txFsm = TxIdle
            , txCreditHeld = True
            , txBlockedFor = 0
            , txPrevCreditIn = False
            , txCreditsConsumed = 0
            , txCreditsReturned = 0
            , txCreditErrors = 0
            , txNoCreditDrops = 0
            , txTimeoutCount = 0
            , txMinRtt = maxBound
            , txMaxRtt = 0
            , txLastRtt = 0
            }
        , (idleTxOut s, False, False)
        )
    | otherwise = (sFinal, (txOutput{txRttSample = rttSampleOut}, fwdAcc, injAcc))
   where
    -- Credit intake (edge-deduplicated), independent of the send FSM.
    creditEdge = isCreditWord creditIn && not s.txPrevCreditIn
    rtt :: Unsigned 32
    rtt = truncateB (counter - s.txSendCycle)
    creditAccepted = creditEdge && not s.txCreditHeld
    sCredit
      | creditEdge && s.txCreditHeld = s{txCreditErrors = s.txCreditErrors + 1}
      | creditAccepted =
          s
            { txCreditHeld = True
            , txCreditsReturned = s.txCreditsReturned + 1
            , txMinRtt = min s.txMinRtt rtt
            , txMaxRtt = max s.txMaxRtt rtt
            , txLastRtt = rtt
            }
      | otherwise = s
    sCredit' = sCredit{txPrevCreditIn = isCreditWord creditIn}
    rttSampleOut = if creditAccepted then Just rtt else Nothing

    canSend = sCredit'.txCreditHeld

    (sFinal, txOutput, fwdAcc, injAcc) = case sCredit'.txFsm of
      TxIdle
        -- Cut-through relay: header goes out THIS cycle.
        | Just seqNr <- relayReq
        , canSend ->
            ( consume sCredit'{txFsm = TxStream{wordIdx = 0}}
            , (idleTxOut sCredit'){txForwardWord = Just (mkHeaderWord seqNr)}
            , False
            , False
            )
        | Just _ <- relayReq ->
            ( sCredit'{txNoCreditDrops = sCredit'.txNoCreditDrops + 1}
            , idleTxOut sCredit'
            , False
            , False
            )
        -- Store-and-forward: accept; the header follows two cycles later
        -- (see 'TxHeaderPend').
        | Just seqNr <- fwdReq
        , canSend ->
            ( consume sCredit'{txFsm = TxHeaderPend{seqNr, emitNow = False}}
            , idleTxOut sCredit'
            , True
            , False
            )
        -- Injector: fire the core this cycle, header this cycle.
        | Just seqNr <- injectReq
        , canSend ->
            ( consume sCredit'{txFsm = TxStream{wordIdx = 0}}
            , (idleTxOut sCredit')
                { txForwardWord = Just (mkHeaderWord seqNr)
                , txInjectFire = Just FireInfo{lap = Lap1, role = RoleInject}
                }
            , False
            , True
            )
        -- Blocked with a pending request: run the deadlock-recovery timer.
        | isJust fwdReq || isJust injectReq ->
            let
              blocked = sCredit'.txBlockedFor + 1
              expired = kn.creditTimeout /= 0 && blocked >= kn.creditTimeout
             in
              if expired
                then
                  ( sCredit'
                      { txCreditHeld = True
                      , txBlockedFor = 0
                      , txTimeoutCount = sCredit'.txTimeoutCount + 1
                      }
                  , idleTxOut sCredit'
                  , False
                  , False
                  )
                else (sCredit'{txBlockedFor = blocked}, idleTxOut sCredit', False, False)
        | otherwise -> (sCredit'{txBlockedFor = 0}, idleTxOut sCredit', False, False)
      TxHeaderPend{seqNr, emitNow}
        | not emitNow ->
            (sCredit'{txFsm = TxHeaderPend{seqNr, emitNow = True}}, idleTxOut sCredit', False, False)
        | otherwise ->
            ( sCredit'{txFsm = TxStream{wordIdx = 0}}
            , (idleTxOut sCredit'){txForwardWord = Just (mkHeaderWord seqNr)}
            , False
            , False
            )
      -- Pull the core's transmit stream; the stream length is governed by
      -- the core's vector_words, so follow txWord validity. The first word
      -- arrives one cycle after this state is entered (fire-timing
      -- contract), so an initial Nothing must not terminate the stream.
      TxStream{wordIdx} -> case coreTx of
        Just w ->
          ( sCredit'{txFsm = TxStream{wordIdx = wordIdx + 1}}
          , (idleTxOut sCredit'){txForwardWord = Just w}
          , False
          , False
          )
        Nothing
          | wordIdx == 0 -> (sCredit', idleTxOut sCredit', False, False)
          | otherwise -> (sCredit'{txFsm = TxIdle}, idleTxOut sCredit', False, False)
     where
      consume st =
        st
          { txCreditHeld = False
          , txSendCycle = counter
          , txBlockedFor = 0
          , txCreditsConsumed = st.txCreditsConsumed + 1
          }

  goInj ::
    InjState ->
    (DecodePeSettings linkCount, Bool, Unsigned 64, Bool, Bool) ->
    (InjState, Maybe (Unsigned 32))
  goInj s (cfg, arm, counter, accepted, sinkDone_)
    | arm =
        (InjState{injFsm = InjWaitLaunch, injSeqNr = 0, injLayerIdx = 0, injTokensStarted = 0}, Nothing)
    | not cfg.isInjector || cfg.mode /= ModeCredit = (s, Nothing)
    | otherwise = case s.injFsm of
        InjWaitLaunch
          | cfg.tokenCount == 0 -> (s{injFsm = InjDone}, Nothing)
          | counter >= cfg.firstCycle -> (s{injFsm = InjRequesting}, Nothing)
          | otherwise -> (s, Nothing)
        InjRequesting
          | accepted -> (s{injFsm = InjWaitSink}, Nothing)
          | otherwise -> (s, Just s.injSeqNr)
        InjWaitSink
          | sinkDone_ ->
              let
                lastLayer = s.injLayerIdx + 1 >= cfg.layersPerToken
                tokensStarted' = if lastLayer then s.injTokensStarted + 1 else s.injTokensStarted
                allDone = lastLayer && tokensStarted' >= cfg.tokenCount
                s' =
                  s
                    { injSeqNr = s.injSeqNr + 2
                    , injLayerIdx = if lastLayer then 0 else s.injLayerIdx + 1
                    , injTokensStarted = tokensStarted'
                    , injFsm =
                        if allDone
                          then InjDone
                          else
                            if cfg.computeCycles == 0
                              then InjRequesting
                              else InjGap{remaining = cfg.computeCycles}
                    }
               in
                (s', Nothing)
          | otherwise -> (s, Nothing)
        InjGap{remaining}
          | remaining <= 1 -> (s{injFsm = InjRequesting}, Nothing)
          | otherwise -> (s{injFsm = InjGap{remaining = remaining - 1}}, Nothing)
        InjDone -> (s, Nothing)

{- | The Wishbone configuration/status device (@CreditLinkConfig@). Knobs are
read-write; status is read-only and mirrors 'CreditLinkStatus' (clearing is
done by the processing element's @arm@ pulse, which resets the hardware
counters themselves).
-}
creditLinkConfig ::
  forall dom addrW nBytes.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat nBytes
  , 1 <= nBytes
  , ?byteOrder :: ByteOrder
  ) =>
  Circuit
    ( (ToConstBwd Mm, Wishbone dom 'Standard addrW nBytes)
    , "STATUS" ::: CSignal dom CreditLinkStatus
    )
    ("KNOBS" ::: CSignal dom CreditLinkKnobs)
creditLinkConfig = circuit $ \(bus, status) -> do
  [ wbCreditTimeout
    , wbCreditPulseLen
    , wbCutThrough
    , wbCreditsConsumed
    , wbCreditsReturned
    , wbCreditsGranted
    , wbFramesReceived
    , wbHeaderErrors
    , wbCreditErrors
    , wbNoCreditDrops
    , wbTimeoutCount
    , wbMinCreditRtt
    , wbMaxCreditRtt
    , wbLastCreditRtt
    ] <-
    deviceWbI (deviceConfig "CreditLinkConfig") -< bus

  Fwd status' <- idC -< status

  (Fwd creditTimeout, _timeoutActivity) <-
    registerWbI
      ( registerConfig
          "credit_timeout"
          "Cycles a blocked sender waits before force-restoring its credit; 0 disables."
      )
        { access = ReadWrite
        }
      (1_048_576 :: Unsigned 32)
      -< (wbCreditTimeout, Fwd (pure Nothing))
  (Fwd creditPulseLen, _pulseLenActivity) <-
    registerWbI
      (registerConfig "credit_pulse_len" "Cycles the credit word is repeated."){access = ReadWrite}
      (4 :: Unsigned 8)
      -< (wbCreditPulseLen, Fwd (pure Nothing))
  (Fwd cutThrough, _cutThroughActivity) <-
    registerWbI
      (registerConfig "cut_through" "True: cut-through relays; False: store-and-forward.")
        { access = ReadWrite
        }
      True
      -< (wbCutThrough, Fwd (pure Nothing))

  ro "credits_consumed" ((.creditsConsumed) <$> status') -< wbCreditsConsumed
  ro "credits_returned" ((.creditsReturned) <$> status') -< wbCreditsReturned
  ro "credits_granted" ((.creditsGranted) <$> status') -< wbCreditsGranted
  ro "frames_received" ((.framesReceived) <$> status') -< wbFramesReceived
  ro "header_errors" ((.headerErrors) <$> status') -< wbHeaderErrors
  ro "credit_errors" ((.creditErrors) <$> status') -< wbCreditErrors
  ro "no_credit_drops" ((.noCreditDrops) <$> status') -< wbNoCreditDrops
  ro "timeout_count" ((.timeoutCount) <$> status') -< wbTimeoutCount
  ro "min_credit_rtt" ((.minCreditRtt) <$> status') -< wbMinCreditRtt
  ro "max_credit_rtt" ((.maxCreditRtt) <$> status') -< wbMaxCreditRtt
  ro "last_credit_rtt" ((.lastCreditRtt) <$> status') -< wbLastCreditRtt

  idC
    -< Fwd
      ( CreditLinkKnobs
          <$> creditTimeout
          <*> creditPulseLen
          <*> cutThrough
      )
 where
  ro ::
    String ->
    Signal dom (Unsigned 32) ->
    Circuit (RegisterWb dom addrW nBytes) ()
  ro name value = circuit $ \wb -> do
    registerWbI_ (registerConfig name ""){access = ReadOnly} (0 :: Unsigned 32)
      -< (wb, Fwd (Just <$> value))
