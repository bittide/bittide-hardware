-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

-- | Transceiver module for the Bittide project. This module is a wrapper around
-- the 'Clash.Cores.Xilinx.GTH.gthCore' function, adding additional functionality
-- such as PRBS generation and checking, comma insertion, word alignment, and
-- user data handshaking.
--
-- __CAUTION__: When instantiating multiple transceivers you might want to use
-- 'transceiverPrbsN'. Make sure to read its documentation before proceeding.
--
-- = Internals
-- This section will cover the internals of the transceiver module. Feel free to
-- skip reading this if you just want to use the transceiver.
--
-- __Commas__
-- We've configured the Xilinx transceiver IP to use 8b/10b encoding. In order
-- for the decoding to work properly, the transceivers need to byte-align. This
-- is done by detecting comma symbols in the incoming data stream. We start out
-- by sending them for a number of cycles (see @Comma.@'Comma.defCycles').
--
-- __Word alignment__
-- After sending commas, we assume the receiver receives our words in a byte-aligned
-- fashion. We can use this fact by reserving the MSB of each byte for an alignment
-- symbol - see "Bittide.Transceiver.WordAlign".
--
-- __Meta data__
-- We send along meta data with each word. This meta data is used to signal to the
-- neighbor that we're ready to receive user data, or that the next word will be
-- user data. The meta data also contains the FPGA and transceiver index, which
-- can be used for debugging.
--
-- __Reset manager__
-- A reset manager is used as a sort of \"watchdog\" while booting the
-- transceivers. It will reset the receive side of the transceiver if it doesn't
-- receive sensible (PRBS) data for a certain amount of time. After resetting the
-- receive side for a number of times, it will reset the transmit side as well if
-- the received data is still considered gibberish. Note that this means that if
-- you receive *good* data for @'Bittide.Transceiver.ResetManager.rxRetries' *
-- 'Bittide.Transceiver.ResetManager.rxTimeoutMs'@ milliseconds, you can be sure
-- the neighbor won't reset its transceiver anymore.
--
-- __Word format__
-- An (aligned) 64 bit word is formatted as follows:
--
-- > +----------+----------+----------+----------+----------+----------+----------+----------+
-- > | 1mpppppp | 0mpppppp | 0mpppppp | 0mpppppp | 0mpppppp | 0mpppppp | 0mpppppp | 0mpppppp |
-- > +----------+----------+----------+----------+----------+----------+----------+----------+
--
--  * 1/0: alignment symbol
--  * m: meta data
--  * p: PRBS data
--
-- __Protocol__
-- The protocol is as follows:
--
-- Transmit:
--
--  1. Send commas for a number of cycles
--  2. Send PRBS data with meta data
--  3. Wait for receiver to signal it has successfully decoded PRBS data for a long time
--  4. Send meta data with 'ready' set to 'True'
--  5. Wait for 'Input.txReady'
--  6. Send meta data with 'lastPrbsWord' set to 'True'
--  7. Send user data
--
-- Note that the reset manager might decide to reset in steps (2) and (3).
--
-- Receive:
--
--  1. Detect alignment symbol and shift data accordingly
--  2. Check PRBS data
--  3. Signal that PRBS data is OK after observing it for some time (see section
--    __Reset manager__).
--  4. Wait for 'Meta.lastPrbsWord'
--  5. Freeze alignment logic
--
module Bittide.Transceiver where

import Clash.Explicit.Prelude

import Bittide.Arithmetic.Time (Milliseconds, trueForSteps)
import Bittide.ElasticBuffer (sticky)
import Clash.Cores.Xilinx.GTH (GthCore)
import Clash.Cores.Xilinx.Ila (IlaConfig(advancedTriggers, depth, stages), ilaConfig, ila, Depth(D1024))
import Clash.Cores.Xilinx.Xpm.Cdc.ArraySingle (xpmCdcArraySingle)
import Clash.Cores.Xilinx.Xpm.Cdc.Single (xpmCdcSingle)
import Clash.Explicit.Reset.Extra (Asserted(Asserted), delayReset, xpmResetSynchronizer)
import Clash.Prelude (withClock)
import Clash.Sized.Vector.Extra (zipWith8)
import Control.Monad (when)
import Data.Maybe (isNothing, fromMaybe)
import Data.Proxy (Proxy(Proxy))

import qualified Bittide.Transceiver.Cdc as Cdc
import qualified Bittide.Transceiver.Comma as Comma
import qualified Bittide.Transceiver.Prbs as Prbs
import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Bittide.Transceiver.WordAlign as WordAlign
import qualified Clash.Cores.Xilinx.GTH as Gth

-- | Meta information send along with the PRBS and alignment symbols. See module
-- documentation for more information.
data Meta = Meta
  { ready :: Bool
  -- ^ Ready to receive user data
  , lastPrbsWord :: Bool
  -- ^ Next word will be user data
  , fpgaIndex :: Unsigned 3
  -- ^ FPGA index to use (debug only, logic does not rely on this)
  , transceiverIndex :: Unsigned 3
  -- ^ Transceiver index to use (debug only, logic does not rely on this)
  }
  deriving (Generic, NFDataX, BitPack)

-- | Insert zeroes such that each of the following are encoded in 4 bits, making
-- them easier to read when formatted as a hex value:
--
--   * prbsOk, lastPrbsWord
--   * fpgaIndex
--   * transceiverIndex
--
-- This is useful for when we don't control formatting (such as when looking at
-- ILA traces).
prettifyMetaBits :: BitVector 8 -> BitVector 12
prettifyMetaBits bv = pack $
  let meta = unpack @Meta bv in
  ( low, low, meta.ready
  , meta.lastPrbsWord
  , low, meta.fpgaIndex
  , low, meta.transceiverIndex
  )

data Config dom = Config
  { debugIla :: Bool
  -- ^ Instantiate a debug ILAs
  , debugFpgaIndex :: Signal dom (Unsigned 3)
  -- ^ FPGA index to use for debug signals
  , resetManagerConfig :: ResetManager.Config
  -- ^ Configuration for 'ResetManager.resetManager'
  }

defConfig :: Config dom
defConfig = Config
  { debugIla = False
  , debugFpgaIndex = pure 0
  , resetManagerConfig = ResetManager.defConfig
  }

-- | Careful: the domains for each transceiver are different, even if their
-- types say otherwise.
data Outputs n tx rx txS free = Outputs
  { txClocks :: Vec n (Clock tx)
  -- ^ See 'Output.txClock'
  , txResets :: Vec n (Reset tx)
  -- ^ See 'Output.txReset'
  , txReadys :: Vec n (Signal tx Bool)
  -- ^ See 'Output.txReady'
  , txSamplings :: Vec n (Signal tx Bool)
  -- ^ See 'Output.txSampling'

  , txPs :: Signal txS (BitVector n)
  -- ^ See 'Output.txP'
  , txNs :: Signal txS (BitVector n)
  -- ^ See 'Output.txN'

  , rxClocks :: Vec n (Clock rx)
  -- ^ See 'Output.rxClock'
  , rxResets :: Vec n (Reset rx)
  -- ^ See 'Output.rxReset'
  , rxDatas :: Vec n (Signal rx (Maybe (BitVector 64)))
  -- ^ See 'Output.rxData'

  , linkUps :: Vec n (Signal free Bool)
  -- ^ See 'Output.linkUp'
  , linkReadys :: Vec n (Signal free Bool)
  -- ^ See 'Output.linkReady'
  , stats :: Vec n (Signal free ResetManager.Statistics)
  -- ^ See 'Output.stats'
  }

data Output tx rx txS free serializedData = Output
  { txClock :: Clock tx
  -- ^ Transmit clock. See 'txReset'.
  , txReset :: Reset tx
  -- ^ Reset signal for the transmit side. Clock can be unstable until this reset
  -- is deasserted.
  , txReady :: Signal tx Bool
  -- ^ Ready to signal to neigbor that next word will be user data. Waiting for
  -- 'Input.txReady' to be asserted before starting to send 'txData'.
  , txSampling :: Signal tx Bool
  -- ^ Data is sampled from 'Input.txSampling'

  , txP :: Signal txS serializedData
  -- ^ Transmit data (and implicitly a clock), positive
  , txN :: Signal txS serializedData
  -- ^ Transmit data (and implicitly a clock), negative

  , rxClock :: Clock rx
  -- ^ Receive clock, recovered from the incoming data stream. See 'rxReset'.
  , rxReset :: Reset rx
  -- ^ Reset signal for the receive side. Clock can be unstable until this reset
  -- is deasserted.
  , rxData :: Signal rx (Maybe (BitVector 64))
  -- ^ User data received from the neighbor

  , linkUp :: Signal free Bool
  -- ^ True if both the transmit and receive side are either handling user data
  , linkReady :: Signal free Bool
  -- ^ True if both the transmit and receive side ready to handle user data or
  -- doing so. I.e., 'linkUp' implies 'linkReady'. Note that this
  , stats :: Signal free ResetManager.Statistics
  -- ^ Statistics exported by 'ResetManager.resetManager'. Useful for debugging.
  }

data Input tx rx ref free rxS serializedData = Input
  { clock :: Clock free
  -- ^ Any "always on" clock
  , reset :: Reset free
  -- ^ Reset signal for the entire transceiver
  , refClock :: Clock ref
  -- ^ Reference clock. Used to synthesize transmit clock.

  , transceiverIndex :: Unsigned 3
  -- ^ Index of this transceiver, used for debugging. Can be set to 0 if not used.
  , channelName :: String
  -- ^ Channel name, example \"X0Y18\"
  , clockPath :: String
  -- ^ Clock path, example \"clk0-2\"

  , rxN :: Signal rxS serializedData
  , rxP :: Signal rxS serializedData

  , txData :: Signal tx (BitVector 64)
  -- ^ Data to transmit to the neighbor. Is sampled on sample after
  -- 'Output.txSamplingOnNext' is asserted. Is sampled when
  -- 'Output.txData' is asserted.
  , txReady :: Signal tx Bool
  -- ^ When asserted, signal to neighbor that next word will be user data. This
  -- signal is ignored until 'Output.txReady' is asserted. Can be tied
  -- to 'True'.
  , rxReady :: Signal rx Bool
  -- ^ When asserted, allow signalling to the neighbor that we are ready to
  -- receive user data. Once asserted, it should stay asserted. Note that the
  -- neighbor might decide to not send user data for a long time, even if this
  -- is asserted.
  }

data Inputs tx rx ref free rxS n = Inputs
  { clock :: Clock free
  -- ^ See 'Input.clock'
  , reset :: Reset free
  -- ^ See 'Input.reset'
  , refClock :: Clock ref
  -- ^ See 'Input.refClock'
  , channelNames :: Vec n String
  -- ^ See 'Input.channel'
  , clockPaths :: Vec n String
  -- ^ See 'Input.clockPath'
  , rxNs :: Signal rxS (BitVector n)
  -- ^ See 'Input.rxN'
  , rxPs :: Signal rxS (BitVector n)
  -- ^ See 'Input.rxP'
  , txDatas :: Vec n (Signal tx (BitVector 64))
  -- ^ See 'Input.txData'
  , txReadys :: Vec n (Signal tx Bool)
  -- ^ See 'Input.txReady'
  , rxReadys :: Vec n (Signal rx Bool)
  -- ^ See 'Input.rxReady'
  }


transceiverPrbsN ::
  forall tx rx ref free txS rxS n .
  ( KnownNat n
  , HasSynchronousReset tx
  , HasDefinedInitialValues tx

  , HasSynchronousReset rx
  , HasDefinedInitialValues rx

  , HasSynchronousReset free
  , HasDefinedInitialValues free

  , KnownDomain rxS
  , KnownDomain txS
  , KnownDomain ref
  , KnownDomain free
  ) =>
  Config free ->
  Inputs tx rx ref free rxS n ->
  Outputs n tx rx txS free
transceiverPrbsN opts inputs@Inputs{clock, reset, refClock} = Outputs
  -- tx
  { txClocks          = map (.txClock) outputs
  , txResets          = map (.txReset) outputs
  , txReadys          = map (.txReady) outputs
  , txSamplings       = map (.txSampling) outputs

  -- rx
  , rxClocks = map (.rxClock) outputs
  , rxResets = map (.rxReset) outputs
  , rxDatas  = map (.rxData) outputs

  -- transceiver
  , txPs = pack <$> bundle (map (.txP) outputs)
  , txNs = pack <$> bundle (map (.txN) outputs)

  -- free
  , linkUps    = map (.linkUp)  outputs
  , linkReadys = map (.linkReady)  outputs
  , stats      = map (.stats)   outputs
  }
 where
  -- XXX: Replacing 'zipWithN' with '<$>' and '<*>' triggers a combination of:
  --
  --       * https://github.com/clash-lang/clash-compiler/issues/2723
  --       * https://github.com/clash-lang/clash-compiler/issues/2722
  --
  -- Note that these bugs break the instantiation of multiple ILAs.
  outputs = zipWith8 go
    (iterateI (+1) 0) -- Note that the target type is only 3 bits, so this will
                      -- wrap around after 8 transceivers. This is fine, as we
                      -- only use this for debugging.
    inputs.channelNames
    inputs.clockPaths
    (unbundle (unpack <$> inputs.rxNs))
    (unbundle (unpack <$> inputs.rxPs))
    inputs.txDatas
    inputs.txReadys
    inputs.rxReadys

  go transceiverIndex channelName clockPath rxN rxP txData txReady rxReady =
    transceiverPrbs opts Input
      { channelName, clockPath, rxN, rxP, txData, txReady, rxReady, transceiverIndex
      , clock, reset, refClock
      }

transceiverPrbs ::
  forall tx rx ref free txS rxS .
  ( HasSynchronousReset tx
  , HasDefinedInitialValues tx

  , HasSynchronousReset rx
  , HasDefinedInitialValues rx

  , HasSynchronousReset free
  , HasDefinedInitialValues free

  , KnownDomain rxS
  , KnownDomain txS
  , KnownDomain ref
  , KnownDomain free
  ) =>
  Config free ->
  Input tx rx ref free rxS (BitVector 1) ->
  Output tx rx txS free (BitVector 1)
transceiverPrbs = transceiverPrbsWith Gth.gthCore

transceiverPrbsWith ::
  forall tx rx ref free txS rxS serializedData .
  ( HasSynchronousReset tx
  , HasDefinedInitialValues tx

  , HasSynchronousReset rx
  , HasDefinedInitialValues rx

  , HasSynchronousReset free
  , HasDefinedInitialValues free

  , KnownDomain rxS
  , KnownDomain txS
  , KnownDomain ref
  , KnownDomain free
  ) =>
  GthCore tx rx ref free txS rxS serializedData ->
  Config free ->
  Input tx rx ref free rxS serializedData ->
  Output tx rx txS free serializedData
transceiverPrbsWith gthCore opts args@Input{clock, reset} =
  when opts.debugIla debugIla `hwSeqX` result
 where

  debugIla :: Signal free ()
  debugIla = ila
    ((ilaConfig $
         "ila_probe_fpgaIndex"
      :> "ila_probe_transIndex"
      :> "ila_probe_txRetries"
      :> "ila_probe_rxRetries"
      :> "ila_probe_rxFullRetries"
      :> "ila_probe_failAfterUps"
      :> "ila_probe_rx_data0"
      :> "ila_probe_alignedRxData0"
      :> "ila_probe_gtwiz_userdata_tx_in"
      :> "ila_probe_reset_rx_done"
      :> "ila_probe_reset_tx_done"
      :> "ila_probe_reset"
      :> "ila_probe_alignError"
      :> "ila_probe_prbsErrors"
      :> "ila_probe_alignedAlignBits"
      :> "ila_probe_alignedMetaBits"
      :> "ila_probe_rxCtrl0"
      :> "ila_probe_rxCtrl1"
      :> "ila_probe_rxCtrl2"
      :> "ila_probe_rxCtrl3"
      :> "ila_probe_prbsOk"
      :> "ila_probe_prbsOkDelayed"
      :> "ila_probe_rst_all"
      :> "ila_probe_rst_rx"
      :> "ila_probe_rxReset"
      :> "ila_probe_txReset"
      :> "ila_probe_metaTx"
      :> "ila_probe_linkUp"
      :> "ila_probe_txLastFree"
      :> "capture"
      :> "trigger"
      :> Nil) { advancedTriggers = True, stages = 1, depth = D1024 })
    clock
    opts.debugFpgaIndex
    (pure args.transceiverIndex :: Signal free (Unsigned 3))
    ((.txRetries) <$> stats)
    ((.rxRetries) <$> stats)
    ((.rxFullRetries) <$> stats)
    ((.failAfterUps) <$> stats)
    (xpmCdcArraySingle rxClock clock rx_data0)
    (xpmCdcArraySingle rxClock clock alignedRxData0)
    (xpmCdcArraySingle txClock clock gtwiz_userdata_tx_in)
    (xpmCdcArraySingle rxClock clock reset_rx_done)
    (xpmCdcArraySingle txClock clock reset_tx_done)
    (unsafeToActiveHigh reset)
    (xpmCdcArraySingle rxClock clock alignError)
    (xpmCdcArraySingle rxClock clock prbsErrors)
    (xpmCdcArraySingle rxClock clock alignedAlignBits)
    (xpmCdcArraySingle rxClock clock (prettifyMetaBits <$> alignedMetaBits))
    (xpmCdcArraySingle rxClock clock rxCtrl0)
    (xpmCdcArraySingle rxClock clock rxCtrl1)
    (xpmCdcArraySingle rxClock clock rxCtrl2)
    (xpmCdcArraySingle rxClock clock rxCtrl3)
    (xpmCdcSingle rxClock clock prbsOk)
    (xpmCdcSingle rxClock clock prbsOkDelayed)
    (unsafeToActiveHigh rst_all)
    (unsafeToActiveHigh rst_rx)
    (xpmCdcSingle rxClock clock $ unsafeToActiveHigh rxReset)
    (xpmCdcSingle txClock clock $ unsafeToActiveHigh txReset)
    (xpmCdcArraySingle txClock clock (prettifyMetaBits . pack <$> metaTx))
    linkUp
    txLastFree
    (pure True :: Signal free Bool) -- capture
    txLastFree -- trigger

  result = Output
    { txSampling = txUserData
    , rxData = mux rxUserData (Just <$> alignedRxData0) (pure Nothing)
    , txReady
    , txN, txP
    , txClock
    , txReset
    , rxClock
    , rxReset
    , linkUp
    , linkReady
    , stats
    }

  linkUp =
         withLockTxFree txUserData
    .&&. withLockRxFree rxUserData

  linkReady = linkUp .||. withLockRxFree rxReadySticky

  ( txN, txP, txClock, rxClock, rx_data0, reset_tx_done, reset_rx_done, tx_active
   , rxCtrl0, rxCtrl1, rxCtrl2, rxCtrl3 )
    = gthCore
        args.channelName args.clockPath
        args.rxN
        args.rxP

        clock -- gtwiz_reset_clk_freerun_in

        (delayReset Asserted clock rst_all {-* filter glitches *-})
        (delayReset Asserted clock rst_rx {-* filter glitches *-}) -- gtwiz_reset_rx_datapath_in
        gtwiz_userdata_tx_in
        txctrl
        args.refClock -- gtrefclk0_in

  prbsConfig = Prbs.conf31 @48

  (commas, txctrl) = Comma.generator Comma.defTimeout txClock txReset
  commasDone = isNothing <$> commas
  prbs = Prbs.generator txClock (unsafeFromActiveLow commasDone) enableGen prbsConfig
  prbsWithMeta = WordAlign.joinMsbs @8 <$> fmap pack metaTx <*> prbs
  prbsWithMetaAndAlign = WordAlign.joinMsbs @8 WordAlign.alignSymbol <$> prbsWithMeta
  gtwiz_userdata_tx_in =
    mux
      txUserData
      args.txData
      (fromMaybe <$> prbsWithMetaAndAlign <*> commas)

  rxReset =
    xpmResetSynchronizer Asserted rxClock rxClock $
                unsafeFromActiveLow (bitCoerce <$> reset_rx_done)
      `orReset` xpmResetSynchronizer Asserted clock rxClock reset

  alignedRxData0 :: Signal rx (BitVector 64)
  alignedRxData0 = withClock rxClock $
    WordAlign.alignBytesFromMsbs @8 WordAlign.alignLsbFirst (rxUserData .||. rxLast) rx_data0

  (alignedAlignBits, alignedRxData1) = unbundle $
    WordAlign.splitMsbs @8 @8 <$> alignedRxData0

  (alignedMetaBits, alignedRxData2) = unbundle $
    WordAlign.splitMsbs @8 @7 <$> alignedRxData1

  prbsErrors = Prbs.checker rxClock rxReset enableGen prbsConfig alignedRxData2
  anyPrbsErrors = prbsErrors ./=. pure 0
  alignError = alignedAlignBits ./=. pure WordAlign.alignSymbol

  -- We consider the control symbols as errors, as they should not be present in
  -- the user data stream. 8b/10b encoding errors naturally count as errors too.
  -- Note that the upper bits of rxCtrl0 and rxCtrl1 are unused.
  --
  -- TODO: Truncate rxCtrl0 and rxCtrl1 in GTH primitive.
  rxCtrlOrError =
         fmap (truncateB @_ @8) rxCtrl0 ./=. pure 0
    .||. fmap (truncateB @_ @8) rxCtrl1 ./=. pure 0
    .||.                        rxCtrl2 ./=. pure 0
    .||.                        rxCtrl3 ./=. pure 0

  prbsOk =
         rxUserData
    .||. Prbs.tracker rxClock rxReset (anyPrbsErrors .||. alignError .||. rxCtrlOrError)

  -- 'prbsWaitMs' is the number of milliseconds representing the worst case time
  -- it takes for the PRBS to stabilize. I.e., after this time we can be sure the
  -- neighbor doesn't reset its transceiver anymore. We add a single retry to
  -- account for clock speed variations.
  prbsWaitMs =
          ((1 :: Index 2) `add` opts.resetManagerConfig.rxRetries)
    `mul` opts.resetManagerConfig.rxTimeoutMs
  prbsOkDelayed = trueForSteps (Proxy @(Milliseconds 1)) prbsWaitMs rxClock rxReset prbsOk
  validMeta = mux rxUserData (pure False) prbsOkDelayed

  rxMeta = mux validMeta (Just . unpack @Meta <$> alignedMetaBits) (pure Nothing)
  rxLast = maybe False (.lastPrbsWord) <$> rxMeta
  rxReady = maybe False (.ready) <$> rxMeta

  rxUserData = sticky rxClock rxReset rxLast
  txUserData = sticky txClock txReset txLast

  txReady = txLast .||. withLockRxTx (prbsOkDelayed .&&. sticky rxClock rxReset args.rxReady)

  rxReadySticky = sticky rxClock rxReset rxReady
  txLast = args.txReady .&&. withLockRxTx rxReadySticky
  txLastFree = xpmCdcSingle txClock clock txLast

  metaTx :: Signal tx Meta
  metaTx = Meta
    <$> txReady
    <*> txLast
    -- We shouldn't sync with 'xpmCdcArraySingle' here, as the individual bits in
    -- 'fpgaIndex' are related to each other. Still, we know fpgaIndex is basically
    -- a constant so :shrug:.
    <*> xpmCdcArraySingle clock txClock opts.debugFpgaIndex
    <*> pure args.transceiverIndex

  (rst_all, rst_rx, stats) =
    ResetManager.resetManager
      opts.resetManagerConfig
      clock reset
      (withLockTxFree (pure True))
      (withLockRxFree (pure True))
      (withLockRxFree (prbsOk .||. rxUserData))

  txReset = xpmResetSynchronizer Asserted txClock txClock $
              unsafeFromActiveLow (bitCoerce <$> tx_active)
    `orReset` unsafeFromActiveLow (bitCoerce <$> reset_tx_done)
    `orReset` xpmResetSynchronizer Asserted clock txClock reset

  withLockTxFree = Cdc.withLock txClock (unpack <$> reset_tx_done) clock reset
  withLockRxFree = Cdc.withLock rxClock (unpack <$> reset_rx_done) clock reset
  withLockRxTx   = Cdc.withLock rxClock (unpack <$> reset_rx_done) txClock txReset
