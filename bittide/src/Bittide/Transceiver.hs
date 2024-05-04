-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Bittide.Transceiver where

import Clash.Explicit.Prelude
import Clash.Explicit.Reset.Extra
import Clash.Cores.Xilinx.GTH
import Clash.Cores.Xilinx.Xpm.Cdc.Single
import Data.Maybe (isNothing, fromMaybe)

import Bittide.Arithmetic.Time
import Bittide.SharedTypes (Bytes, Byte)

-- | Careful: the domains for each transceiver are different, even if their
-- types say otherwise.
data TransceiverOutputs n tx rx txS freeclk = TransceiverOutputs
  { txClocks :: Vec n (Clock tx)
  -- ^ Transmit clocks. Can only be used if associated 'linkUps' is asserted.
  --
  -- TODO: Export more signals such that this clock can be used earlier.
  , rxClocks :: Vec n (Clock rx)
  -- ^ Receive clocks, recovered from the incoming data stream. Can only be used
  -- if associated 'linkUps' is asserted.
  --
  -- TODO: Export more signals such that these clocks can be used earlier.
  , txPs :: Vec n (Signal txS (BitVector 1))
  -- ^ Transmit data (and implicitly a clock), positive
  , txNs :: Vec n (Signal txS (BitVector 1))
  -- ^ Transmit data (and implicitly a clock), negative
  , linkUps :: Vec n (Signal rx Bool)
  -- ^ True if the link is considered stable. See 'linkStateTracker'.
  , stats :: Vec n (Signal freeclk GthResetStats)
  -- ^ Statistics exported by 'gthResetManager'
  }

data TransceiverOutput tx rx txS freeclk = TransceiverOutput
  { txClock :: Clock tx
  -- ^ Transmit clock. Can only be used after 'linkUp' is asserted.
  --
  -- TODO: Export more signals such that this clock can be used earlier.
  , rxClock :: Clock rx
  -- ^ Receive clock, recovered from the incoming data stream. Can only be used
  -- after 'linkUp' is asserted.
  --
  -- TODO: Export more signals such that this clock can be used earlier.
  , txP :: Signal txS (BitVector 1)
  -- ^ Transmit data (and implicitly a clock), positive
  , txN :: Signal txS (BitVector 1)
  -- ^ Transmit data (and implicitly a clock), negative
  , linkUp :: Signal rx Bool
  -- ^ True if the link is considered stable. See 'linkStateTracker'.
  , stats :: Signal freeclk GthResetStats
  -- ^ Statistics exported by 'gthResetManager'
  }

transceiverPrbsN ::
  forall tx rx refclk freeclk txS rxS chansUsed .
  ( KnownNat chansUsed
  , HasSynchronousReset tx
  , HasDefinedInitialValues tx

  , HasSynchronousReset rx
  , HasDefinedInitialValues rx

  , HasSynchronousReset freeclk
  , HasDefinedInitialValues freeclk
  ) =>
  Clock refclk ->
  Clock freeclk ->

  Reset freeclk ->

  Vec chansUsed String ->
  Vec chansUsed String ->

  Vec chansUsed (Signal rxS (BitVector 1)) ->
  Vec chansUsed (Signal rxS (BitVector 1)) ->

  TransceiverOutputs chansUsed tx rx txS freeclk
transceiverPrbsN refclk freeclk rst chanNms clkPaths rxns rxps = TransceiverOutputs
  { txClocks = map (.txClock) outputs
  , rxClocks = map (.rxClock) outputs
  , txPs     = map (.txP)     outputs
  , txNs     = map (.txN)     outputs
  , linkUps  = map (.linkUp)  outputs
  , stats    = map (.stats)   outputs
  }
 where
  outputs = zipWith4 (transceiverPrbs refclk freeclk rst) chanNms clkPaths rxns rxps

transceiverPrbs ::
  ( HasSynchronousReset tx
  , HasDefinedInitialValues tx

  , HasSynchronousReset rx
  , HasDefinedInitialValues rx

  , HasSynchronousReset freeclk
  , HasDefinedInitialValues freeclk
  ) =>

  Clock refclk ->
  Clock freeclk ->

  Reset freeclk ->  -- ^ rst all

  String -> -- ^ channel, example X0Y18
  String -> -- ^ clkPath, example clk0-2

  Signal rxS (BitVector 1) ->
  Signal rxS (BitVector 1) ->

  TransceiverOutput tx rx txS freeclk
transceiverPrbs gtrefclk freeclk rst_all_in chan clkPath rxn rxp = TransceiverOutput
  { txClock = tx_clk
  , rxClock = rx_clk
  , txP = txp
  , txN = txn
  , linkUp = link_up
  , stats = stats
  }
 where
  (txn, txp, tx_clk, rx_clk, rx_data, reset_tx_done, reset_rx_done, tx_active)
    = gthCore
        chan clkPath
        rxn
        rxp

        freeclk -- gtwiz_reset_clk_freerun_in

        (delayReset Asserted freeclk rst_all {-* filter glitches *-})
        noReset -- gtwiz_reset_tx_pll_and_datapath_in
        noReset -- gtwiz_reset_tx_datapath_in
        noReset -- gtwiz_reset_rx_pll_and_datapath_in
        (delayReset Asserted freeclk rst_rx {-* filter glitches *-}) -- gtwiz_reset_rx_datapath_in
        gtwiz_userdata_tx_in
        txctrl
        freeclk -- drpclk_in
        gtrefclk -- gtrefclk0_in

  prbsConfig = prbsConf31w64

  (commas, txctrl) = commaGen defCommaGenCycles tx_clk txStimRst
  commasDone = isNothing <$> commas
  prbs = prbsGen tx_clk (unsafeFromActiveLow commasDone) enableGen prbsConfig
  gtwiz_userdata_tx_in = fromMaybe <$> prbs <*> commas

  rxReset =
    xpmResetSynchronizer Asserted rx_clk rx_clk $
      unsafeFromActiveLow $ fmap bitCoerce reset_rx_done

  prbsErrors = prbsChecker rx_clk rxReset enableGen prbsConfig rx_data
  anyErrors = fmap (pack . reduceOr) prbsErrors
  link_up = linkStateTracker rx_clk rxReset anyErrors

  -- XPM_CDC_SINGLE config used to synchronize 'reset_tx_done' and
  -- 'reset_rx_done'. We use Xilinx's defaults, except for 'registerInput' which
  -- we set to 'False'. We do this, because the two 'done' signals are indicative
  -- of their domain's clocks stability.
  cdcConfig = XpmCdcSingleConfig
    { stages = d4 -- default
    , initialValues = True -- default
    , registerInput = False
    }

  (rst_all, rst_rx, _init_done, stats) =
    gthResetManager
      freeclk rst_all_in
      (xpmCdcSingleWith cdcConfig tx_clk freeclk $ unpack <$> reset_tx_done)
      (xpmCdcSingleWith cdcConfig rx_clk freeclk $ unpack <$> reset_rx_done)
      (xpmCdcSingle rx_clk freeclk link_up)

  txStimRst = xpmResetSynchronizer Asserted tx_clk tx_clk $
              (unsafeFromActiveLow $ fmap bitCoerce tx_active)
    `orReset` (unsafeFromActiveLow $ fmap bitCoerce reset_tx_done)
    `orReset` xpmResetSynchronizer Asserted freeclk tx_clk rst_all_in

data LinkSt
  = Down (Index 127)
  -- ^ Link is considered down. Needs 127 cycles of \"good\" input to transition
  -- to 'Up'.
  | Up
  -- ^ Link has not seen errors in at least 127 cycles.
  deriving (Eq, Show, Generic, NFDataX)

isUp :: LinkSt -> Bool
isUp Up = True
isUp _ = False

-- | Small state machine tracking whether a link is stable. A link is considered
-- stable, if no errors were detected for a number of cycles (see "LinkSt").
-- Whenever a bit error is detected, it immediately deasserts its output.
linkStateTracker ::
  (KnownDomain dom, KnownNat w) =>
  Clock dom ->
  Reset dom ->
  Signal dom (BitVector w) ->
  Signal dom Bool
linkStateTracker clk rst =
  mooreB clk rst enableGen update isUp initSt . fmap (/= 0)
 where
  initSt = Down maxBound

  update :: LinkSt -> Bool -> LinkSt
  update _  True = initSt
  update st False =
    case st of
      Down 0 -> Up
      Down n -> Down (n - 1)
      Up -> Up

-- | Number of cycles to generate commas for, pulled from Xilinx example code.
defCommaGenCycles :: SNat 10240
defCommaGenCycles = SNat

-- | Generate commas (transceiver alignment symbols) for a number of cycles
commaGen ::
  forall nCycles nBytes dom .
  ( KnownDomain dom
  , KnownNat nBytes
  , 1 <= nCycles ) =>
  SNat nCycles ->
  Clock dom ->
  Reset dom ->
  -- | (comma, tx control)
  ( Signal dom (Maybe (Bytes nBytes))
  , Signal dom (BitVector nBytes)
  )
commaGen _nCycles@SNat clk rst =
  ( mux sendCommas (pure $ Just commas) (pure Nothing)
  , mux sendCommas (pure maxBound)      (pure 0)
  )
 where
  comma :: Byte
  comma = 0xbc

  commas :: Bytes nBytes
  commas = pack (repeat comma)

  sendCommas = counter ./=. pure maxBound
  counter = register clk rst enableGen (0 :: Index nCycles) (satSucc SatBound <$> counter)

data PrbsConfig polyLength polyTap nBits where
  PrbsConfig ::
    ( KnownNat polyLength
    , KnownNat polyTap
    , KnownNat nBits

    , 1 <= nBits
    , 1 <= polyTap
    , (polyTap + 1) <= polyLength

    -- Same constraints, but written differently for type checking purposes:
    , (_n0 + 1) ~ nBits
    , (polyTap + _n1) ~ polyLength
    , polyTap ~ (_n2 + 1)
    , _n1 ~ (_n3 + 1)
    ) =>
    PrbsConfig polyLength polyTap nBits


-- | PRBS configuration where we use the full 64 data bits for the PRBS.
prbsConf31w64 :: PrbsConfig 31 28 64
prbsConf31w64 = PrbsConfig

prbsGen ::
  forall dom polyLength polyTap nBits .
  KnownDomain dom =>
  Clock dom -> Reset dom -> Enable dom ->
  PrbsConfig polyLength polyTap nBits ->
  Signal dom (BitVector nBits)
prbsGen clk rst ena PrbsConfig =
  mealy clk rst ena go (maxBound,maxBound) (pure ())
 where
  go ::
    (BitVector polyLength, BitVector nBits) ->
    () ->
    ((BitVector polyLength, BitVector nBits), BitVector nBits)
  go (prbs_reg, prbs_out_prev) _ =
    ( ( last prbs
      , pack (reverse $ map msb prbs))
    , prbs_out_prev
    )
   where
     prbs :: Vec nBits (BitVector polyLength)
     prbs = unfoldrI goPrbs prbs_reg

     goPrbs :: BitVector polyLength -> (BitVector polyLength, BitVector polyLength)
     goPrbs bv = (o,o)
      where
       o = nb +>>. bv
       tap = SNat @(polyLength - polyTap)
       nb = xor (lsb bv) (unpack $ slice tap tap bv)


prbsChecker ::
  forall dom polyLength polyTap nBits .
  KnownDomain dom =>
  Clock dom -> Reset dom -> Enable dom ->
  PrbsConfig polyLength polyTap nBits ->
  Signal dom (BitVector nBits) ->
  Signal dom (BitVector nBits)
prbsChecker clk rst ena PrbsConfig = mealy clk rst ena go (maxBound, maxBound)
 where
  go ::
    (BitVector polyLength, BitVector nBits) ->
    BitVector nBits ->
    ((BitVector polyLength, BitVector nBits), BitVector nBits)
  go (prbs_reg, prbs_out_prev) prbsIn =
    ( (prbs_state, pack $ reverse prbs_out)
    , prbs_out_prev
    )
   where
     prbs_out :: Vec nBits Bit
     prbs_state :: BitVector polyLength
     (prbs_state, prbs_out) = mapAccumL goPrbs prbs_reg (reverse $ unpack prbsIn)

     goPrbs :: BitVector polyLength -> Bit -> (BitVector polyLength, Bit)
     goPrbs bv inp = (o, bitErr)
      where
       o = inp +>>. bv
       tap = SNat @(polyLength - polyTap)
       bitErr = xor inp (xor (lsb bv) (unpack $ slice tap tap bv))

-- | 'Index' with its 'maxBound' corresponding to the number of cycles needed to
-- wait for /n/ milliseconds.
type IndexMs dom n = Index (PeriodToCycles dom (Milliseconds n))

-- | Statistics exported by 'gthResetManager'
data GthResetStats = GthResetStats
  { txRetries :: Unsigned 32
  -- ^ How many times the transmit side was reset
  , rxRetries :: Index 32
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

-- | Bringing up the transceivers is a stochastic process - at least, that is
-- what Xilinx reference designs make us believe. We therefore retry a number of
-- times if we don't see sensible data coming in. See the individual constructors
-- and 'gthResetManager' for more information.
--
-- XXX: Current timeout values for 'TxWait' and 'RxWait' are chosen arbitrarily.
--      We should investigate what these values should be for quick bring-up.
data GthResetState dom
  = StartTx GthResetStats
  -- ^ Reset everything - transmit and receive side
  | StartRx GthResetStats
  -- ^ Reset just the receive side
  | TxWait GthResetStats (IndexMs dom 3)
  -- ^ Wait for the transmit side to report it is done. After /n/ milliseconds
  -- (see type) it times out, moving to 'StartTx'.
  | RxWait GthResetStats (IndexMs dom 13)
  -- ^ Wait for the receive side to report it is done _and_ that it can predict
  -- the data coming from the other side. After /n/ milliseconds (see type) it
  -- times out. Depending on the value of 'GthResetStat's 'rxRetries' it will
  -- either reset both the receive and the transmit side, or just the receive
  -- side. If all is well though, move on to 'Monitor'.
  | Monitor GthResetStats
  -- ^ Wait till the end of the universe, or until a link goes down - whichever
  -- comes first. In case of the latter, the state machine moves to 'StartTx'.
  deriving (Generic, NFDataX)

-- | Reset manager for transceivers: see 'GthResetState' for more information on
-- this state machine. See 'GthResetStats' for information on what debug values
-- are exported.
gthResetManager ::
  forall dom .
  KnownDomain dom =>
  Clock dom ->
  Reset dom ->
  "tx_init_done" ::: Signal dom Bool ->
  "rx_init_done" ::: Signal dom Bool ->
  "rx_data_good" ::: Signal dom Bool ->
  ( "reset_all_out" ::: Reset dom
  , "reset_rx"  ::: Reset dom
  , "init_done" ::: Signal dom Bool
  , "stats" ::: Signal dom GthResetStats
  )
gthResetManager clk rst tx_init_done rx_init_done rx_data_good =
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

  initSt :: GthResetState dom
  initSt = StartTx (GthResetStats
    { rxRetries=0
    , rxFullRetries=0
    , txRetries=0
    , failAfterUps=0
    })

  update :: GthResetState dom -> (Bool, Bool, Bool) -> GthResetState dom
  update st (tx_done, rx_done, rx_good) =
    case st of
      -- Reset everything:
      StartTx stats -> TxWait stats 0

      -- Wait for transceiver to indicate it is done
      TxWait stats@GthResetStats{txRetries} cntr
        | tx_done          -> RxWait stats 0
        | cntr == maxBound -> StartTx stats{txRetries=satSucc SatBound txRetries}
        | otherwise        -> TxWait stats (succ cntr)

      -- Reset receive side logic
      StartRx stats -> RxWait stats 0

      -- Wait for a reliable incoming link. This can fail in multiple ways, see
      -- 'RxWait'.
      RxWait stats@GthResetStats{rxRetries, rxFullRetries} cntr
        | rx_done && rx_good ->
          Monitor stats

        | cntr == maxBound && rxRetries == maxBound ->
          StartTx stats{rxFullRetries=satSucc SatBound rxFullRetries}

        | cntr == maxBound ->
          StartRx stats{rxRetries=satSucc SatBound rxRetries}

        | otherwise ->
          RxWait stats (succ cntr)

      -- Monitor link. Move all the way back to 'StartTx' if the link goes down
      -- for some reason.
      Monitor stats@GthResetStats{failAfterUps}
        | rx_done && rx_good -> Monitor stats
        | otherwise -> StartTx stats{failAfterUps=satSucc SatBound failAfterUps}

  extractOutput st = case st of
         --             rst_all rst_rx done   statistics
    StartTx stats   -> (True,   False, False, stats)
    TxWait  stats _ -> (False,  False, False, stats)
    StartRx stats   -> (False,  True,  False, stats)
    RxWait  stats _ -> (False,  False, False, stats)
    Monitor stats   -> (False,  False, True,  stats)
