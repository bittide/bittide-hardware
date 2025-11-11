-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Functions to reset the transceiver subsystems during bringup
module Bittide.Transceiver.ResetManager where

import Clash.Explicit.Prelude

import Bittide.Arithmetic.Time (IndexMs)
import Clash.Class.Counter (countSucc)
import Data.Bifunctor (Bifunctor (first))

-- | See 'Config.txTimeoutMs'
type MaxTxTimeoutMs = Index 128

-- | See 'Config.rxTimeoutMs'
type MaxRxTimeoutMs = Index 128

-- | See 'Config.rxRetries'
type MaxRxRetries = Index 128

-- | Statistics exported by 'resetManager'
data Statistics = Statistics
  { txRetries :: Unsigned 32
  -- ^ How many times the transmit side was reset in order to get a stable clock
  -- signal.
  , rxRetries :: MaxRxRetries
  -- ^ How many times the receive side was reset. Note that this value in itself
  -- will reset if the receive side doesn't come up quickly enough - see
  -- 'rxFullRetries'.
  , rxFullRetries :: Unsigned 32
  -- ^ How many times 'rxRetries' overflowed. I.e., how many times 'WaitRx' moved
  -- the state machine back to 'ResetUserTx'.
  , failAfterUps :: Unsigned 32
  -- ^ How many times the link failed when in the 'Monitor' state - i.e., after
  -- detecting it fully worked. A failure can have a diverse number of root
  -- causes. Examples include 8b/10b decoding errors, disconnected cables, or
  -- instable clocks.
  }
  deriving (Generic, NFDataX)

{- | Configuration for 'resetManager'

Develop notes: the current API is a balance between the tightest possible
hardware and the most flexible API. We could make the API more flexible by
exposing 'MaxTxTimeoutMs' and friends as type parameters, but that severely
impacts verbosity and readability.
-}
data Config = Config
  { txTimeoutMs :: MaxTxTimeoutMs
  -- ^ Number of milliseconds to wait for the transmit side to be ready, before
  -- resetting the transmit subsystem.
  , rxTimeoutMs :: MaxRxTimeoutMs
  -- ^ Number of milliseconds to wait for the receive side to be ready, before
  -- resetting the receiver subsystem.
  , rxRetries :: MaxRxRetries
  -- ^ Number of times to retry the receive side before resetting the transmit
  -- side as well.
  }
  deriving (Generic, NFDataX, Show)

{- | Default configuration for 'resetManager'

XXX: Current timeout values for 'WaitTxClock' and 'WaitRx' are chosen arbitrarily.
     We should investigate what these values should be for quick bring-up.
-}
defConfig :: Config
defConfig =
  Config
    { txTimeoutMs = 1
    , rxTimeoutMs = 5
    , rxRetries = 8
    }

{- | 'resetManager' moves through the state in order of constructor definition. If
a state's documentation explicitly mentions that it resets a subcomponent, all
previous states will have that reset asserted too. Note that 'Resets.rxPllAndDatapath'
isn't currently mentioned: it is never asserted.
-}
data State dom
  = -- | Reset everything, including TX PLLs - transmit and receive side. I.e.,
    -- 'Resets.all'.
    --
    -- TODO: Transceiver is edge sensitive on its falling edge. This means that
    -- *leaving* the InReset state triggers a reset. This is counterintuitive, so
    -- maybe we should make sure that whenever a reset is asserted a falling edge
    -- is produced for the transceiver IP. But also, maybe that's not necessary
    -- because we assert all other reset signals on the transceiver?
    InReset
  | -- | Reset all PLLs and datapaths, i.e. 'Resets.txPllAndDatapath'.
    StartTxClock
  | -- | Wait for 'Input.txInitDone'
    WaitTxClock (MaxTxTimeoutMs, IndexMs dom 1)
  | -- | Assert 'Resets.txUser'. After deassertion, the user should (re)start
    -- comma generation, PRBS generation, etc. If 'Input.channelReset' is asserted,
    -- the state machine will never make it past this point.
    ResetUserTx
  | -- | Reset the receive side datapath. Note that we don't reset the PLL here,
    -- as the TX and RX PLL are assumed to be shared (CPLL / Channel-PLL).
    ResetRx
  | -- | Wait for the receive side to report it is done _and_ that it can predict
    -- the data coming from the other side. After /n/ milliseconds (see type) it
    -- times out. Depending on the value of 'ResetStat's 'rxRetries' it will
    -- either reset both the receive and the transmit side (see 'ResetUserTx'), or
    -- just the receive side. If all is well though, move on to 'Monitor'.
    WaitRx (MaxRxTimeoutMs, IndexMs dom 1)
  | -- | Wait till the end of the universe, or until a link goes down - whichever
    -- comes first. In case of the latter, the state machine moves to 'ResetUserTx'.
    --
    -- TODO: Manager should stay in the monitor state on failure and export
    --       failure reason. It should only move back upon 'Input.channelReset'
    --       assertion.
    Monitor
  deriving (Generic, NFDataX, Eq, Ord)

data Input dom = Input
  { channelReset :: Reset dom
  -- ^ Channel reset. Asserting this will cause the state machine to assert
  -- 'Resets.txUser' and renegotiate the RX subsystem.
  , txInitDone :: Signal dom Bool
  -- ^ Transmit side initialization done. Should come from the GTH core.
  , rxInitDone :: Signal dom Bool
  -- ^ Receive side initialization done. Should come from the GTH core.
  , rxDataGood :: Signal dom Bool
  -- ^ Receive side data good. Should be asserted if the PRBS checker has been
  -- content for \"long enough\".
  , errorAfterRxUser :: Signal dom Bool
  -- ^ Indicates that an error has been detected after the RX user reset has
  -- been deasserted. This is used to detect link failures while in the
  -- 'Monitor' state.
  --
  -- TODO: Accept more fine-grained error information here.
  }

data Resets dom = Resets
  { all :: Reset dom
  -- ^ Should be routed to @gtwiz_reset_all_in@.
  , txPllAndDatapath :: Reset dom
  -- ^ Should be routed to @gtwiz_reset_tx_pll_and_datapath_in@.
  , txDatapath :: Reset dom
  -- ^ Should be routed to @gtwiz_reset_tx_datapath_in@.
  , txDomain :: Reset dom
  -- ^ Reset that can be paired with the TX clock. It is deasserted once the TX
  -- clock is stable.
  , txUser :: Reset dom
  -- ^ Should be used to reset user-side logic, e.g., comma generation,
  -- PRBS generation, etc.
  , rxPllAndDatapath :: Reset dom
  -- ^ Never asserted in the current state machine. Should be routed to
  -- @gtwiz_reset_rx_pll_and_datapath_in@.
  , rxDatapath :: Reset dom
  -- ^ Should be routed to @gtwiz_reset_rx_datapath_in@.
  }

data FsmResets = FsmResets
  { all :: Bool
  , txPllAndDatapath :: Bool
  , txDatapath :: Bool
  , txDomain :: Bool
  , txUser :: Bool
  , rxPllAndDatapath :: Bool
  , rxDatapath :: Bool
  }

{- | Reset manager for transceivers: see 'State' for more information on
this state machine. See 'Statistics' for information on what debug values
are exported.

The reset manager is designed such that it can be used for systems that share a
TX clock signal. I.e., where one (channel) PLL generates the user TX clock
signal for all others. In practice this means that, once the main reset is
deasserted, the reset manager will step through a number of steps to get the PLL
up and running. This state is only ever revisited upon main reset (de)assertion.
-}
resetManager ::
  forall dom.
  (KnownDomain dom) =>
  Config ->
  Clock dom ->
  -- | Global reset. If asserted, this will keep the PLL and TX/RX datapath in
  -- reset. Note that the GTH core itself will only respond to 'Resets.all' on
  -- reset deassertion, though this difference is largely an academic difference
  -- due to the aforementioned components being held in reset. Note that this is
  -- the only reset that can reinitialize PLLs, the individual 'Input.channelReset'
  -- won't do so.
  Reset dom ->
  Input dom ->
  (Resets dom, Signal dom Statistics)
resetManager config clk rst args = (resets, statistics)
 where
  resets =
    Resets
      { all = unsafeFromActiveHigh fsmResets.all
      , txPllAndDatapath = unsafeFromActiveHigh fsmResets.txPllAndDatapath
      , txDatapath = unsafeFromActiveHigh fsmResets.txDatapath
      , txDomain = unsafeFromActiveHigh fsmResets.txDomain
      , txUser = unsafeFromActiveHigh fsmResets.txUser
      , rxPllAndDatapath = unsafeFromActiveHigh fsmResets.rxPllAndDatapath
      , rxDatapath = unsafeFromActiveHigh fsmResets.rxDatapath
      }

  (fsmResets :: Signal dom FsmResets, statistics :: Signal dom Statistics) =
    mooreB
      clk
      rst
      enableGen
      update
      (first stateToFsmResets)
      (initState, initStats)
      ( unsafeToActiveHigh args.channelReset
      , args.txInitDone
      , args.rxInitDone
      , args.rxDataGood
      , args.errorAfterRxUser
      )

  initStats :: Statistics
  initStats =
    Statistics
      { txRetries = 0
      , rxRetries = 0
      , rxFullRetries = 0
      , failAfterUps = 0
      }

  initState :: State dom
  initState = InReset

  -- I'm purposely adding empty comment lines, so that each case is visually
  -- separated from the next. Without them, it's hard to read (IMO).
  update ::
    (State dom, Statistics) ->
    (Bool, Bool, Bool, Bool, Bool) ->
    (State dom, Statistics)
  update st (channelReset, txInitDone, rxInitDone, rxDataGood, errorAfterRxUser) =
    case st of
      -- Reset everything, including TX PLLs - transmit and receive side.
      (InReset, stats) -> (StartTxClock, stats)
      --
      -- Reset all PLLs and datapaths. Note that, after the TX clock comes up,
      -- we don't reset the PLL again.
      (StartTxClock, stats) -> (WaitTxClock minBound, stats)
      --
      -- Wait for transmit side to come up. If it doesn't within the timeout,
      -- reset the transmit side again.
      (WaitTxClock cntr@(ms, _), stats@Statistics{txRetries})
        | txInitDone -> (ResetUserTx, stats)
        | ms == config.txTimeoutMs ->
            (StartTxClock, stats{txRetries = satSucc SatBound txRetries})
        | otherwise -> (WaitTxClock (countSucc cntr), stats)
      --
      -- If user requested a channel reset, go back to 'ResetUserTx' / never
      -- move past it. This makes sure that the PLL keeps running, but the
      -- channel is down otherwise.
      (_, stats) | channelReset -> (ResetUserTx, stats)
      --
      -- Assert 'Resets.txUser'. After deassertion, the user should (re)start
      -- comma generation, PRBS generation, etc.
      (ResetUserTx, stats) -> (ResetRx, stats)
      --
      -- Reset receive side datapath. We don't reset the PLL here, as it is assumed
      -- to be shared with the transmit side.
      (ResetRx, stats) -> (WaitRx minBound, stats)
      --
      -- Wait for a reliable incoming link. This can fail in multiple ways, see
      -- 'WaitRx'.
      (WaitRx cntr@(ms, _), stats@Statistics{rxRetries, rxFullRetries})
        | rxInitDone && rxDataGood ->
            (Monitor, stats)
        | ms == config.rxTimeoutMs && rxRetries >= config.rxRetries ->
            (ResetUserTx, stats{rxFullRetries = satSucc SatBound rxFullRetries})
        | ms == config.rxTimeoutMs ->
            (ResetRx, stats{rxRetries = satSucc SatBound rxRetries})
        | otherwise ->
            (WaitRx (countSucc cntr), stats)
      --
      -- Monitor link. Move all the way back to 'ResetUserTx' if the link goes down
      -- for some reason.
      --
      -- TODO: Make this more fine-grained, add error state, wait for user channel reset.
      (Monitor, stats@Statistics{failAfterUps})
        | rxInitDone && rxDataGood && not errorAfterRxUser -> (Monitor, stats)
        | otherwise -> (ResetUserTx, stats{failAfterUps = satSucc SatBound failAfterUps})

  stateToFsmResets st =
    FsmResets
      { all = st == InReset
      , txPllAndDatapath = st <= StartTxClock
      , txDatapath = st <= StartTxClock
      , txDomain = st < ResetUserTx
      , txUser = st <= ResetUserTx
      , rxPllAndDatapath = False
      , rxDatapath = st <= ResetRx
      }
