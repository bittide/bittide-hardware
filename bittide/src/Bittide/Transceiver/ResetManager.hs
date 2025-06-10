-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Functions to reset the transceiver subsystems during bringup
module Bittide.Transceiver.ResetManager where

import Clash.Explicit.Prelude

import Bittide.Arithmetic.Time (IndexMs)
import Clash.Class.Counter (countSucc)

-- | See 'Config.txTimeoutMs'
type MaxTxTimeoutMs = Index 128

-- | See 'Config.rxTimeoutMs'
type MaxRxTimeoutMs = Index 128

-- | See 'Config.rxRetries'
type MaxRxRetries = Index 128

-- | Statistics exported by 'resetManager'
data Statistics = Statistics
  { txRetries :: Unsigned 32
  -- ^ How many times the transmit side was reset
  , rxRetries :: MaxRxRetries
  -- ^ How many times the receive side was reset. Note that this value in itself
  -- will reset if the transmit side resets - see 'rxFullRetries'.
  , rxFullRetries :: Unsigned 32
  -- ^ How many times 'rxRetries' overflowed. I.e., how many times 'RxWait' moved
  -- the state machine back to 'StartTx'.
  , failAfterUps :: Unsigned 32
  -- ^ How many times the link failed when in the 'Monitor' state - i.e., after
  -- detecting it fully worked. This usually happens if the other side drops its
  -- link because it tried resetting its receive side too many times - see
  -- 'rxFullRetries'.
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

XXX: Current timeout values for 'TxWait' and 'RxWait' are chosen arbitrarily.
     We should investigate what these values should be for quick bring-up.
-}
defConfig :: Config
defConfig =
  Config
    { txTimeoutMs = 1
    , rxTimeoutMs = 5
    , rxRetries = 8
    }

{- | Bringing up the transceivers is a stochastic process - at least, that is
what Xilinx reference designs make us believe. We therefore retry a number of
times if we don't see sensible data coming in. See the individual constructors
and 'resetManager' for more information.
-}
data State dom
  = InReset
  | -- | Reset everything - transmit and receive side
    StartTx
  | -- | Reset just the receive side
    StartRx
  | -- | Wait for the transmit side to report it is done. After /n/ milliseconds
    -- (see type) it times out, moving to 'StartTx'.
    TxWait (MaxTxTimeoutMs, IndexMs dom 1)
  | -- | Wait for the receive side to report it is done _and_ that it can predict
    -- the data coming from the other side. After /n/ milliseconds (see type) it
    -- times out. Depending on the value of 'ResetStat's 'rxRetries' it will
    -- either reset both the receive and the transmit side, or just the receive
    -- side. If all is well though, move on to 'Monitor'.
    RxWait (MaxRxTimeoutMs, IndexMs dom 1)
  | -- | Wait till the end of the universe, or until a link goes down - whichever
    -- comes first. In case of the latter, the state machine moves to 'StartTx'.
    Monitor
  deriving (Generic, NFDataX, Eq)

{- | Reset manager for transceivers: see 'State' for more information on
this state machine. See 'Statistics' for information on what debug values
are exported.
-}
resetManager ::
  forall dom.
  (KnownDomain dom) =>
  Config ->
  Clock dom ->
  Reset dom ->
  "tx_init_done" ::: Signal dom Bool ->
  "rx_init_done" ::: Signal dom Bool ->
  "rx_data_good" ::: Signal dom Bool ->
  ( "reset_all_out" ::: Reset dom
  , "reset_rx" ::: Reset dom
  , "stats" ::: Signal dom Statistics
  )
resetManager config clk rst tx_init_done rx_init_done rx_data_good =
  ( unsafeFromActiveHigh reset_all_out_sig
  , unsafeFromActiveHigh reset_rx_sig
  , statistics
  )
 where
  (reset_all_out_sig, reset_rx_sig, statistics) =
    mooreB
      clk
      rst
      enableGen
      update
      extractOutput
      (initState, initStats)
      ( tx_init_done
      , rx_init_done
      , rx_data_good
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

  update :: (State dom, Statistics) -> (Bool, Bool, Bool) -> (State dom, Statistics)
  update st (tx_done, rx_done, rx_good) =
    case st of
      (InReset, stats) -> (StartTx, stats)
      -- Reset everything:
      (StartTx, stats) -> (TxWait minBound, stats)
      -- Wait for transceiver to indicate it is done
      (TxWait cntr@(ms, _), stats@Statistics{txRetries})
        | tx_done -> (RxWait minBound, stats)
        | ms == config.txTimeoutMs -> (StartTx, stats{txRetries = satSucc SatBound txRetries})
        | otherwise -> (TxWait (countSucc cntr), stats)
      -- Reset receive side logic
      (StartRx, stats) -> (RxWait minBound, stats)
      -- Wait for a reliable incoming link. This can fail in multiple ways, see
      -- 'RxWait'.
      (RxWait cntr@(ms, _), stats@Statistics{rxRetries, rxFullRetries})
        | rx_done && rx_good ->
            (Monitor, stats)
        | ms == config.rxTimeoutMs && rxRetries >= config.rxRetries ->
            (StartTx, stats{rxFullRetries = satSucc SatBound rxFullRetries})
        | ms == config.rxTimeoutMs ->
            (StartRx, stats{rxRetries = satSucc SatBound rxRetries})
        | otherwise ->
            (RxWait (countSucc cntr), stats)
      -- Monitor link. Move all the way back to 'StartTx' if the link goes down
      -- for some reason.
      (Monitor, stats@Statistics{failAfterUps})
        | rx_done && rx_good -> (Monitor, stats)
        | otherwise -> (StartTx, stats{failAfterUps = satSucc SatBound failAfterUps})

  extractOutput (st, stats) =
    ( st == StartTx
    , st == StartRx
    , stats
    )
