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

import Data.Maybe (fromMaybe)

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

type UserCoreBusses = 3

type RingBufferDepth = 512

ringBufferDepth :: SNat RingBufferDepth
ringBufferDepth = SNat

mkUserCore :: UserCoreCircuit UserCoreBusses (NmuRemBusWidth UserCoreBusses)
mkUserCore bitClk bitRst bitEna localCounter _maybeDna =
  circuit $ \(userCoreBusses, Fwd rxs2Raw, handshakeOut) -> do
    [muProgrammableMuxBus, peConfigBus, creditLinkBus] <- idC -< userCoreBusses

    -- Start business logic
    (Fwd settings, Fwd armPulse) <-
      withClockResetEnable bitClk bitRst bitEna (decodePeConfig @_ @_ @_ @LinkCount)
        -< (peConfigBus, Fwd status)
    Fwd knobs <-
      withClockResetEnable bitClk bitRst bitEna creditLinkConfig
        -< (creditLinkBus, Fwd clStatus)

    let
      isCredit = (\cfg -> cfg.mode == ModeCredit) <$> settings

      fireA =
        withClock bitClk
          $ calendarFrontEnd businessLogicReset localCounter settings armPulse
      clOut =
        withClock bitClk
          $ creditLink businessLogicReset localCounter settings knobs armPulse rxs2Raw coreOut
      fire = mux isCredit ((.fire) <$> clOut) fireA

      rawRx =
        liftA2 (\cfg v -> v !! fromMaybe (0 :: Index LinkCount) cfg.readLink) settings rxs2Raw
      rxWord = mux isCredit ((.coreRxWord) <$> clOut) rawRx
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
            extSample
      clStatus = (.status) <$> clOut

      -- Per-link transmit: idle counter everywhere, except the write link
      -- (core stream in calendar mode, credit-link forward in credit mode)
      -- and, in credit mode, the read link's reverse direction (credits).
      idleWord = resize . pack <$> localCounter
      coreTx = fromMaybe <$> idleWord <*> ((.txWord) <$> coreOut)
      fwdTx = fromMaybe <$> idleWord <*> ((.forwardTx) <$> clOut)
      crdTx = fromMaybe <$> idleWord <*> ((.creditTx) <$> clOut)
      buildTxs cfg credit idleW coreW fwdW crdW = withCredit
       where
        base = repeat idleW
        withForward =
          maybe base (\wl -> replace wl (if credit then fwdW else coreW) base) cfg.writeLink
        withCredit
          | credit = maybe withForward (\rl -> replace rl crdW withForward) cfg.readLink
          | otherwise = withForward
      peTxs = buildTxs <$> settings <*> isCredit <*> idleWord <*> coreTx <*> fwdTx <*> crdTx
    -- Stop business logic

    -- Start programmable mux
    (Fwd businessLogicReset, txsOut) <-
      withClockResetEnable bitClk bitRst bitEna
        $ programmableMux localCounter
        -< (muProgrammableMuxBus, handshakeOut, Fwd peTxs)
    -- Stop programmable mux

    idC -< txsOut
