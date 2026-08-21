-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedRecordDot #-}

{- | User core for the decode demo: the streaming-reduce processing element
with two front-ends — a calendar (variant A, scheduled) and a credit link
(variant B, hardware-async) — selected by the @mode@ register, behind a
programmable mux that hands the links from the management unit to the
processing element once armed.

The management-unit path (ring buffers) carries variants C and A′ before the
mux is armed; the mux is one-shot, so the host driver runs those first. After
arming, variants A, B and B_sf re-run by rewriting the config registers and
pulsing @arm@ — the datapath is never reset again.

For the contention experiment (PLAN2), a traffic generator shares the ring
links with the decode flow: its bursts ride the forward direction next to
decode frames (calendar slots in scheduled mode, a work-conserving
round-robin frame arbiter in credit mode) and its credits share the reverse
direction. With @tg_mode = 0@ the generator is off and every path reduces to
the base demo's.
-}
module Bittide.Instances.Hitl.DecodeDemo.UserCore (
  UserCoreBusses,
  RingBufferDepth,
  ringBufferDepth,
  mkUserCore,
) where

import Clash.Explicit.Prelude
import Clash.Prelude (withClock, withClockResetEnable)
import Protocols

import Data.Maybe (fromMaybe, isJust)

import Bittide.CreditLink (
  CreditLinkOut (..),
  creditLink,
  creditLinkConfig,
 )
import Bittide.DecodeProcessingElement (
  DecodePeSettings (..),
  PeMode (..),
  StreamIn (..),
  StreamOut (..),
  calendarFrontEnd,
  decodePeConfig,
  decodeReduceCore,
  decodeSequencer,
 )
import Bittide.Instances.Hitl.GenericDemo.BringUp (NmuRemBusWidth, UserCoreCircuit)
import Bittide.Instances.Hitl.Setup (LinkCount)
import Bittide.ProgrammableMux (programmableMux)
import Bittide.TrafficGen (
  ArbiterOut (..),
  TgMode (..),
  TrafficGenOut (..),
  TrafficGenSettings (..),
  TrafficGenStatus (..),
  linkPortArbiter,
  rxStreamDemux,
  trafficGen,
  trafficGenConfig,
 )

type UserCoreBusses = 4

type RingBufferDepth = 512

ringBufferDepth :: SNat RingBufferDepth
ringBufferDepth = SNat

mkUserCore :: UserCoreCircuit UserCoreBusses (NmuRemBusWidth UserCoreBusses)
mkUserCore bitClk bitRst bitEna localCounter _maybeDna =
  circuit $ \(userCoreBusses, Fwd rxs2Raw, handshakeOut) -> do
    [muProgrammableMuxBus, peConfigBus, creditLinkBus, trafficGenBus] <- idC -< userCoreBusses

    -- Start business logic
    (Fwd settings, Fwd armPulse) <-
      withClockResetEnable bitClk bitRst bitEna (decodePeConfig @_ @_ @_ @LinkCount)
        -< (peConfigBus, Fwd status)
    Fwd knobs <-
      withClockResetEnable bitClk bitRst bitEna creditLinkConfig
        -< (creditLinkBus, Fwd clStatus)
    Fwd tgSettings <-
      withClockResetEnable bitClk bitRst bitEna trafficGenConfig
        -< (trafficGenBus, Fwd tgStatus)

    let
      isCredit = (\cfg -> cfg.mode == ModeCredit) <$> settings
      tgIsCredit = (\c -> c.tgMode == TgCredit) <$> tgSettings

      fwdRawRx =
        liftA2 (\cfg v -> v !! fromMaybe (0 :: Index LinkCount) cfg.readLink) settings rxs2Raw
      crdRawRx =
        liftA2 (\cfg v -> v !! fromMaybe (0 :: Index LinkCount) cfg.writeLink) settings rxs2Raw

      -- Credit-mode frame demux of the shared upstream tap.
      demuxed =
        withClock bitClk
          $ rxStreamDemux
            businessLogicReset
            armPulse
            (liftA2 (\cfg tg -> (cfg.vectorWords, tg.tgBurstWords)) settings tgSettings)
            fwdRawRx
      decodeFwdRx = fst <$> demuxed
      tgFwdRx = snd <$> demuxed

      -- Port arbiter for the shared forward direction. Word valids are
      -- registered: they close a loop through the senders and only feed the
      -- collision counter.
      arbOut =
        withClock bitClk
          $ linkPortArbiter
            businessLogicReset
            armPulse
            ((.reservePort) <$> clOut)
            ((.portRequest) <$> clOut)
            ((.tgPortReq) <$> tgOut)
            ((.txActive) <$> clOut)
            ((.tgTxActive) <$> tgOut)
            wordValidsR

      fireA =
        withClock bitClk
          $ calendarFrontEnd businessLogicReset localCounter settings armPulse
      clOut =
        withClock bitClk
          $ creditLink
            businessLogicReset
            localCounter
            settings
            knobs
            armPulse
            decodeFwdRx
            crdRawRx
            ((.arbPortFree) <$> arbOut)
            ((.arbDecodeGrant) <$> arbOut)
            coreOut
      tgOut =
        withClock bitClk
          $ trafficGen
            businessLogicReset
            localCounter
            tgSettings
            armPulse
            (mux tgIsCredit tgFwdRx fwdRawRx)
            crdRawRx
            ((.arbTgGrant) <$> arbOut)
            (isJust <$> decodeCrdM)

      fire = mux isCredit ((.fire) <$> clOut) fireA
      rxWord = mux isCredit ((.coreRxWord) <$> clOut) fwdRawRx
      streamIn = StreamIn <$> fire <*> rxWord
      (_, coreOut) =
        toSignals
          (withClock bitClk $ decodeReduceCore businessLogicReset localCounter settings)
          (streamIn, ())

      -- Relays sample per-transfer credit round-trip times in credit mode;
      -- everything else histograms token latencies.
      extSample =
        liftA2
          (\cfg r -> if cfg.isInjector || cfg.mode /= ModeCredit then Nothing else r)
          settings
          ((.rttSample) <$> clOut)
      status =
        withClock bitClk
          $ decodeSequencer
            businessLogicReset
            localCounter
            settings
            armPulse
            fire
            ((.lapResult) <$> coreOut)
            rxWord
            extSample
      clStatus = (.status) <$> clOut
      tgStatus =
        liftA2
          (\t a -> (t.tgStatus){tgCollisions = a.arbCollisions})
          tgOut
          arbOut

      idleWord = resize . pack <$> localCounter

      -- Forward direction of the write link: the decode flow's word, else
      -- the generator's, else idle. Arbitration (credit) or disjoint
      -- calendars (scheduled) guarantee at most one is valid; the collision
      -- counter proves it.
      decodeTxM = mux isCredit ((.forwardTx) <$> clOut) ((.txWord) <$> coreOut)
      tgTxM = (.tgTxWord) <$> tgOut
      fwdWord = pick <$> decodeTxM <*> tgTxM <*> idleWord
      wordValidsR =
        register
          bitClk
          businessLogicReset
          bitEna
          (False, False)
          (liftA2 (\d t -> (isJust d, isJust t)) decodeTxM tgTxM)

      -- Reverse direction of the read link: single-word credits from both
      -- flows; the decode flow wins the cycle and the generator's receiver
      -- stalls its pulse (it watches this).
      decodeCrdM = (.creditTx) <$> clOut
      tgCrdM = (.tgCreditTx) <$> tgOut
      crdWord = pick <$> decodeCrdM <*> tgCrdM <*> idleWord

      pick :: Maybe (BitVector 64) -> Maybe (BitVector 64) -> BitVector 64 -> BitVector 64
      pick d t i = fromMaybe (fromMaybe i t) d

      buildTxs cfg idleW fwdW crdW = withReverse
       where
        base = repeat idleW
        withForward = maybe base (\wl -> replace wl fwdW base) cfg.writeLink
        withReverse = maybe withForward (\rl -> replace rl crdW withForward) cfg.readLink
      peTxs = buildTxs <$> settings <*> idleWord <*> fwdWord <*> crdWord
    -- Stop business logic

    -- Start programmable mux
    (Fwd businessLogicReset, txsOut) <-
      withClockResetEnable bitClk bitRst bitEna
        $ programmableMux localCounter
        -< (muProgrammableMuxBus, handshakeOut, Fwd peTxs)
    -- Stop programmable mux

    idC -< txsOut
