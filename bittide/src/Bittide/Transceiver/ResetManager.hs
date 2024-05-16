{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Bittide.Transceiver.ResetManager where

import Clash.Explicit.Prelude
import Clash.Class.Counter (countSucc)

import Bittide.Arithmetic.Time (PeriodToCycles, Milliseconds)

-- | 'Index' with its 'maxBound' corresponding to the number of cycles needed to
-- wait for /n/ milliseconds.
type IndexMs dom n = Index (PeriodToCycles dom (Milliseconds n))

-- | Number of milliseconds
type Ms = Unsigned 8

-- | Statistics exported by 'resetManager'
data Statistics = Statistics
  { txRetries :: Unsigned 32
  -- ^ How many times the transmit side was reset
  , rxRetries :: Unsigned 8
  -- ^ How many times the receive side was reset. Note that this value in itself
  -- will reset if the transmit side resets - see 'rxFullRetries'.
  , rxFullRetries :: Unsigned 32
  -- ^ How many times 'rxRetries' overflowed. I.e., how many times 'RxWait' moved
  -- the state machine back to 'StartTx'.
  , failAfterUps  :: Unsigned 32
  -- ^ How many times the link failed when in the 'Monitor' state - i.e., after
  -- detecting it fully worked. This usually happens if the other side drops its
  -- link because it tried resetting its receive side too many times - see
  -- 'rxFullRetries'.
  }
  deriving (Generic, NFDataX)

-- | Configuration for 'resetManager'
data Config = Config
  { txTimeout :: Ms
  -- ^ Number of milliseconds to wait for the transmit side to be ready, before
  -- resetting the transmit subsystem.
  , rxTimeout :: Ms
  -- ^ Number of milliseconds to wait for the receive side to be ready, before
  -- resetting the receiver subsystem.
  , rxRetries :: Unsigned 8
  -- ^ Number of times to retry the receive side before resetting the transmit
  -- side as well.
  }
  deriving (Generic, NFDataX)

-- | Default configuration for 'resetManager'
--
-- XXX: Current timeout values for 'TxWait' and 'RxWait' are chosen arbitrarily.
--      We should investigate what these values should be for quick bring-up.
defConfig :: Config
defConfig = Config
  { txTimeout = 1
  , rxTimeout = 5
  , rxRetries = 8
  }

-- | Bringing up the transceivers is a stochastic process - at least, that is
-- what Xilinx reference designs make us believe. We therefore retry a number of
-- times if we don't see sensible data coming in. See the individual constructors
-- and 'resetManager' for more information.
data State dom
  = StartTx Statistics
  -- ^ Reset everything - transmit and receive side
  | StartRx Statistics
  -- ^ Reset just the receive side
  | TxWait Statistics (Ms, IndexMs dom 1)
  -- ^ Wait for the transmit side to report it is done. After /n/ milliseconds
  -- (see type) it times out, moving to 'StartTx'.
  | RxWait Statistics (Ms, IndexMs dom 1)
  -- ^ Wait for the receive side to report it is done _and_ that it can predict
  -- the data coming from the other side. After /n/ milliseconds (see type) it
  -- times out. Depending on the value of 'ResetStat's 'rxRetries' it will
  -- either reset both the receive and the transmit side, or just the receive
  -- side. If all is well though, move on to 'Monitor'.
  | Monitor Statistics
  -- ^ Wait till the end of the universe, or until a link goes down - whichever
  -- comes first. In case of the latter, the state machine moves to 'StartTx'.
  deriving (Generic, NFDataX)

-- | Reset manager for transceivers: see 'State' for more information on
-- this state machine. See 'Statistics' for information on what debug values
-- are exported.
resetManager ::
  forall dom .
  KnownDomain dom =>
  Config ->
  Clock dom ->
  Reset dom ->
  "tx_init_done" ::: Signal dom Bool ->
  "rx_init_done" ::: Signal dom Bool ->
  "rx_data_good" ::: Signal dom Bool ->
  ( "reset_all_out" ::: Reset dom
  , "reset_rx"  ::: Reset dom
  , "init_done" ::: Signal dom Bool
  , "stats" ::: Signal dom Statistics
  )
resetManager config clk rst tx_init_done rx_init_done rx_data_good =
  ( unsafeFromActiveHigh reset_all_out_sig
  , unsafeFromActiveHigh reset_rx_sig
  , init_done
  , statistics
  )
 where
  (reset_all_out_sig, reset_rx_sig, init_done, statistics) =
    mooreB
      clk rst enableGen
      update
      extractOutput
      initSt
      ( tx_init_done
      , rx_init_done
      , rx_data_good
      )

  initSt :: State dom
  initSt = StartTx (Statistics
    { rxRetries=0
    , rxFullRetries=0
    , txRetries=0
    , failAfterUps=0
    })

  update :: State dom -> (Bool, Bool, Bool) -> State dom
  update st (tx_done, rx_done, rx_good) =
    case st of
      -- Reset everything:
      StartTx stats -> TxWait stats minBound

      -- Wait for transceiver to indicate it is done
      TxWait stats@Statistics{txRetries} cntr@(ms, _)
        | tx_done                -> RxWait stats minBound
        | ms == config.txTimeout -> StartTx stats{txRetries=satSucc SatBound txRetries}
        | otherwise              -> TxWait stats (countSucc cntr)

      -- Reset receive side logic
      StartRx stats -> RxWait stats minBound

      -- Wait for a reliable incoming link. This can fail in multiple ways, see
      -- 'RxWait'.
      RxWait stats@Statistics{rxRetries, rxFullRetries} cntr@(ms, _)
        | rx_done && rx_good ->
          Monitor stats

        | ms == config.rxTimeout && rxRetries >= config.rxRetries ->
          StartTx stats{rxFullRetries=satSucc SatBound rxFullRetries}

        | ms == config.rxTimeout ->
          StartRx stats{rxRetries=satSucc SatBound rxRetries}

        | otherwise ->
          RxWait stats (countSucc cntr)

      -- Monitor link. Move all the way back to 'StartTx' if the link goes down
      -- for some reason.
      Monitor stats@Statistics{failAfterUps}
        | rx_done && rx_good -> Monitor stats
        | otherwise -> StartTx stats{failAfterUps=satSucc SatBound failAfterUps}

  extractOutput st = case st of
         --             rst_all rst_rx done   statistics
    StartTx stats   -> (True,   False, False, stats)
    TxWait  stats _ -> (False,  False, False, stats)
    StartRx stats   -> (False,  True,  False, stats)
    RxWait  stats _ -> (False,  False, False, stats)
    Monitor stats   -> (False,  False, True,  stats)
