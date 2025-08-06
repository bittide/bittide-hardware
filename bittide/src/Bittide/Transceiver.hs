-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Transceiver module for the Bittide project. This module is a wrapper around
the 'Clash.Cores.Xilinx.GTH.gthCore' function, adding additional functionality
such as PRBS generation and checking, comma insertion, word alignment, and
user data handshaking.

__CAUTION__: When instantiating multiple transceivers you might want to use
'transceiverPrbsN'. Make sure to read its documentation before proceeding.

= Internals
This section will cover the internals of the transceiver module. Feel free to
skip reading this if you just want to use the transceiver.

__Commas__
We've configured the Xilinx transceiver IP to use 8b/10b encoding. In order
for the decoding to work properly, the transceivers need to byte-align. This
is done by detecting comma symbols in the incoming data stream. We start out
by sending them for a number of cycles (see @Comma.@'Comma.defCycles').

__Word alignment__
After sending commas, we assume the receiver receives our words in a byte-aligned
fashion. We can use this fact by reserving the MSB of each byte for an alignment
symbol - see "Bittide.Transceiver.WordAlign".

__Meta data__
We send along meta data with each word. This meta data is used to signal to the
neighbor that we're ready to receive user data, or that the next word will be
user data. The meta data also contains the FPGA and transceiver index, which
can be used for debugging.

__Reset manager__
A reset manager is used as a sort of \"watchdog\" while booting the
transceivers. It will reset the receive side of the transceiver if it doesn't
receive sensible (PRBS) data for a certain amount of time. After resetting the
receive side for a number of times, it will reset the transmit side as well if
the received data is still considered gibberish. Note that this means that if
you receive *good* data for @'Bittide.Transceiver.ResetManager.rxRetries' *
'Bittide.Transceiver.ResetManager.rxTimeoutMs'@ milliseconds, you can be sure
the neighbor won't reset its transceiver anymore.

__Word format__
An (aligned) 64 bit word is formatted as follows:

> +----------+----------+----------+----------+----------+----------+----------+----------+
> | 1mpppppp | 0mpppppp | 0mpppppp | 0mpppppp | 0mpppppp | 0mpppppp | 0mpppppp | 0mpppppp |
> +----------+----------+----------+----------+----------+----------+----------+----------+

 * 1/0: alignment symbol
 * m: meta data
 * p: PRBS data

__Protocol__
The protocol is as follows:

Transmit:

 1. Send commas for a number of cycles
 2. Send PRBS data with meta data
 3. Wait for receiver to signal it has successfully decoded PRBS data for a long time
 4. Send meta data with 'ready' set to 'True'
 5. Wait for 'Input.txStart'
 6. Send meta data with 'lastPrbsWord' set to 'True'
 7. Send user data

Note that the reset manager might decide to reset in steps (2) and (3).

Receive:

 1. Detect alignment symbol and shift data accordingly
 2. Check PRBS data
 3. Signal that PRBS data is OK after observing it for some time (see section
   __Reset manager__).
 4. Wait for 'Meta.lastPrbsWord'
 5. Freeze alignment logic
-}
module Bittide.Transceiver where

import Clash.Explicit.Prelude

import Bittide.Arithmetic.Time (trueForSteps)
import Bittide.ElasticBuffer (sticky)
import Clash.Cores.Xilinx.Ila (
  Depth (D1024),
  IlaConfig (advancedTriggers, depth, stages),
  ila,
  ilaConfig,
 )
import Clash.Cores.Xilinx.Xpm.Cdc.ArraySingle (xpmCdcArraySingle)
import Clash.Cores.Xilinx.Xpm.Cdc.Single (xpmCdcSingle)
import Clash.Explicit.Reset.Extra (Asserted (Asserted), delayReset, xpmResetSynchronizer)
import Clash.Prelude (withClock)
import Control.Monad (when)
import Data.Maybe (fromMaybe, isNothing)
import Data.Proxy (Proxy (Proxy))
import GHC.Stack (HasCallStack)

import qualified Bittide.Transceiver.Cdc as Cdc
import qualified Bittide.Transceiver.Comma as Comma
import qualified Bittide.Transceiver.Prbs as Prbs
import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Bittide.Transceiver.WordAlign as WordAlign
import qualified Clash.Cores.Xilinx.GTH as Gth

{- | Meta information send along with the PRBS and alignment symbols. See module
documentation for more information.
-}
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

{- | Insert zeroes such that each of the following are encoded in 4 bits, making
them easier to read when formatted as a hex value:

  * prbsOk, lastPrbsWord
  * fpgaIndex
  * transceiverIndex

This is useful for when we don't control formatting (such as when looking at
ILA traces).
-}
prettifyMetaBits :: BitVector 8 -> BitVector 12
prettifyMetaBits bv =
  pack
    $ let meta = unpack @Meta bv
       in ( low
          , low
          , meta.ready
          , meta.lastPrbsWord
          , low
          , meta.fpgaIndex
          , low
          , meta.transceiverIndex
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
defConfig =
  Config
    { debugIla = False
    , debugFpgaIndex = pure 0
    , resetManagerConfig = ResetManager.defConfig
    }

{- | Careful: the domains for the rx side of each transceiver are different, even if their
types say otherwise.
-}
data Outputs n tx rx txS free = Outputs
  { txClock :: Clock tx
  -- ^ Single transmit clock, shared by all links
  , txReset :: Reset tx
  -- ^ Reset associated with 'txClock'. Once deasserted, the clock is stable. In
  -- the current implementation, this also means the first link has completed a
  -- (low level) handshake. In the future we want to decouple this: the TX clock
  -- should be able to come up independently of any link.
  , txReadys :: Vec n (Signal tx Bool)
  -- ^ See 'Output.txReady'
  , txSamplings :: Vec n (Signal tx Bool)
  -- ^ See 'Output.txSampling'
  , handshakesDoneTx :: Vec n (Signal tx Bool)
  -- ^ See 'Output.handshakeDoneTx'
  , txPs :: Gth.Wires txS n
  -- ^ See 'Output.txP'
  , txNs :: Gth.Wires txS n
  -- ^ See 'Output.txN'
  , txSims :: Gth.SimWires tx n
  -- ^ See 'Output.txSim'
  , rxClocks :: Vec n (Clock rx)
  -- ^ See 'Output.rxClock'
  , rxResets :: Vec n (Reset rx)
  -- ^ See 'Output.rxReset'
  , rxDatas :: Vec n (Signal rx (Maybe (BitVector 64)))
  -- ^ See 'Output.rxData'
  , handshakesDone :: Vec n (Signal rx Bool)
  -- ^ See 'Output.handshakeDone'
  , linkUps :: Vec n (Signal free Bool)
  -- ^ See 'Output.linkUp'
  , linkReadys :: Vec n (Signal free Bool)
  -- ^ See 'Output.linkReady'
  , handshakesDoneFree :: Vec n (Signal free Bool)
  -- ^ See 'Output.handshakeDoneFree'
  , stats :: Vec n (Signal free ResetManager.Statistics)
  -- ^ See 'Output.stats'
  }

data Output tx rx tx1 rx1 txS free = Output
  { txOutClock :: Clock tx1
  -- ^ Must be routed through xilinxGthUserClockNetworkTx or equivalent to get usable clocks
  , txReset :: Reset tx
  -- ^ Reset signal for the transmit side. Clock can be unstable until this reset
  -- is deasserted.
  , txReady :: Signal tx Bool
  -- ^ Ready to signal to neighbor that next word will be user data. Waiting for
  -- 'Input.txStart' to be asserted before starting to send 'txData'.
  , txSampling :: Signal tx Bool
  -- ^ Data is sampled from 'Input.txData'
  , handshakeDoneTx :: Signal tx Bool
  -- ^ Asserted when link has been established, but not necessarily handling user data.
  -- This signal is native to the 'rx' domain. If you need that, use 'handshakeDone'.
  , txP :: Gth.Wire txS
  -- ^ Transmit data (and implicitly a clock), positive
  , txN :: Gth.Wire txS
  -- ^ Transmit data (and implicitly a clock), negative
  , txSim :: Gth.SimWire tx
  -- ^ Simulation only construct. Data for the transmit side. Used for testing.
  , rxOutClock :: Clock rx1
  -- ^ Must be routed through xilinxGthUserClockNetworkRx or equivalent to get usable clocks
  , rxReset :: Reset rx
  -- ^ Reset signal for the receive side. Clock can be unstable until this reset
  -- is deasserted.
  , rxData :: Signal rx (Maybe (BitVector 64))
  -- ^ User data received from the neighbor
  , handshakeDone :: Signal rx Bool
  -- ^ Asserted when link has been established, but not necessarily handling user data.
  , linkUp :: Signal free Bool
  -- ^ True if both the transmit and receive side are either handling user data
  , linkReady :: Signal free Bool
  -- ^ True if both the transmit and receive side ready to handle user data or
  -- doing so. I.e., 'linkUp' implies 'linkReady'. Note that this
  , handshakeDoneFree :: Signal free Bool
  -- ^ Asserted when link has been established, but not necessarily handling user data.
  -- This signal is native to the 'rx' domain. If you need that, use 'handshakeDone'.
  , stats :: Signal free ResetManager.Statistics
  -- ^ Statistics exported by 'ResetManager.resetManager'. Useful for debugging.
  }

data Input tx rx tx1 rx1 ref free rxS = Input
  { clock :: Clock free
  -- ^ Any "always on" clock
  , reset :: Reset free
  -- ^ Reset signal for the entire transceiver
  , refClock :: Clock ref
  -- ^ Reference clock. Used to synthesize transmit clock.
  , clockTx1 :: Clock tx1
  , clockTx2 :: Clock tx
  , txActive :: Signal tx (BitVector 1)
  , clockRx1 :: Clock rx1
  , clockRx2 :: Clock rx
  , rxActive :: Signal rx (BitVector 1)
  , transceiverIndex :: Unsigned 3
  -- ^ Index of this transceiver, used for debugging. Can be set to 0 if not used.
  , channelName :: String
  -- ^ Channel name, example \"X0Y18\"
  , clockPath :: String
  -- ^ Clock path, example \"clk0-2\"
  , rxSim :: Gth.SimWire rx
  -- ^ Simulation only construct. Data for the receive side. Used for testing.
  , rxN :: Gth.Wire rxS
  , rxP :: Gth.Wire rxS
  , txData :: Signal tx (BitVector 64)
  -- ^ Data to transmit to the neighbor. Is first sampled one cycle after both
  -- 'Input.txStart' and 'Output.txReady' are asserted. Is continuously sampled
  -- afterwards.
  , txStart :: Signal tx Bool
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
  -- ^ See 'Input.channelName'
  , clockPaths :: Vec n String
  -- ^ See 'Input.clockPath'
  , rxNs :: Gth.Wires rxS n
  -- ^ See 'Input.rxN'
  , rxPs :: Gth.Wires rxS n
  -- ^ See 'Input.rxP'
  , rxSims :: Gth.SimWires rx n
  -- ^ See 'Input.rxSim'
  , txDatas :: Vec n (Signal tx (BitVector 64))
  -- ^ See 'Input.txData'
  , txStarts :: Vec n (Signal tx Bool)
  -- ^ See 'Input.txStart'
  , rxReadys :: Vec n (Signal rx Bool)
  -- ^ See 'Input.rxReady'
  }

{-
[NOTE: duplicate tx/rx domain]
'gthCore' and the inside of 'transceiverPrbsN' have two extra domains, tx1 and rx1,
that aren't visible outside of transceiverPrbsN.
To do this completely clean/safe transceiverPrbsN should have two extra
forall arguments, two extra KnownDomain constraints.
And either some Proxy arguments or we would have to enable AllowAmbiguousTypes.

Because the tx1/rx1 domains aren't visible outside 'transceiverPrbsN',
I choose to sidestep the extra complication and pretend tx1/rx1 and tx/rx are the same.
This disables the typechecking safety we'd normally get from clash,
but vivado should call us out when we make a mistake.
-}

{- | Clash has a very hard time translating maps with 'SimOnly' constructs,
making compilation take ~10 times longer. The reasons are not clear to me,
but replacing entire structures with 'deepErrorX' seems to work around the
issue.
-}
simOnlyHdlWorkaround :: (HasCallStack, NFDataX a) => a -> a
simOnlyHdlWorkaround a
  | clashSimulation = a
  | otherwise = deepErrorX "simOnlyHdlWorkaround: not in simulation"

transceiverPrbsN ::
  forall tx rx ref free txS rxS n m.
  ( KnownNat n
  , n ~ m + 1
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
transceiverPrbsN opts inputs@Inputs{clock, reset, refClock} =
  Outputs
    { -- tx
      txClock = txClock
    , txReset = unsafeFromActiveLow (head outputs).handshakeDoneTx
    , txReadys = map (.txReady) outputs
    , txSamplings = map (.txSampling) outputs
    , handshakesDoneTx = map (.handshakeDoneTx) outputs
    , txSims = simOnlyHdlWorkaround (SimOnly (map (Gth.unSimOnly . (.txSim)) outputs))
    , -- rx
      rxClocks = rxClocks
    , rxResets = map (.rxReset) outputs
    , rxDatas = map (.rxData) outputs
    , handshakesDone = map (.handshakeDone) outputs
    , -- transceiver
      txPs = pack <$> bundle (map (.txP) outputs)
    , txNs = pack <$> bundle (map (.txN) outputs)
    , -- free
      linkUps = map (.linkUp) outputs
    , linkReadys = map (.linkReady) outputs
    , handshakesDoneFree = map (.handshakeDoneFree) outputs
    , stats = map (.stats) outputs
    }
 where
  -- XXX: 'outputs' used to be written with zipWithN, to workaround bugs:
  --       * https://github.com/clash-lang/clash-compiler/issues/2723
  --       * https://github.com/clash-lang/clash-compiler/issues/2722
  -- That breaks the instantiation of the the debug ILAs inside 'transceiverPrbs'.
  --
  -- But when the GTHs were changed to use external "user clock networks",
  -- this zipWithN became unusably slow when using more then ~4 transceivers.
  -- Unfortunately this means debugIla is broken now, when using more then 1 transceiver.
  outputs =
    go txClockNw
      <$> iterateI (+ 1) 0 -- Note that the target type is only 3 bits, so this will
      -- wrap around after 8 transceivers. This is fine, as we
      -- only use this for debugging.
      <*> inputs.channelNames
      <*> inputs.clockPaths
      <*> simOnlyHdlWorkaround (map SimOnly (Gth.unSimOnly inputs.rxSims))
      <*> unbundle (unpack <$> inputs.rxNs)
      <*> unbundle (unpack <$> inputs.rxPs)
      <*> inputs.txDatas
      <*> inputs.txStarts
      <*> inputs.rxReadys
      <*> rxClockNws

  -- NOTE: The example project generated by gtwizard_ultrascale suggests tying tx/rxUsrClkRst
  -- to tx/rxpmaresetdone, but that doesn't seem to work.
  -- And also it's not what the gtwizard_ultrascale does when configured with internal
  -- "user clock network".
  txUsrClkRst = noReset @tx
  rxUsrClkRst = noReset @rx

  txOutClk = (head outputs).txOutClock
  -- see [NOTE: duplicate tx/rx domain]
  txClockNw = Gth.xilinxGthUserClockNetworkTx @tx @tx txOutClk txUsrClkRst
  (_txClk1s, txClock, _txClkActives) = txClockNw

  rxOutClks = map (.rxOutClock) outputs
  -- see [NOTE: duplicate tx/rx domain]
  rxClockNws = map (flip (Gth.xilinxGthUserClockNetworkRx @rx @rx) rxUsrClkRst) rxOutClks
  (_rxClk1s, rxClocks, _rxClkActives) = unzip3 rxClockNws

  go (clockTx1, clockTx2, txActive) transceiverIndex channelName clockPath rxSim rxN rxP txData txStart rxReady (clockRx1, clockRx2, rxActive) =
    transceiverPrbs
      opts
      Input
        { channelName
        , clockPath
        , rxSim
        , rxN
        , rxP
        , txData
        , txStart
        , rxReady
        , transceiverIndex
        , clock
        , reset
        , refClock
        , clockTx1
        , clockTx2
        , txActive
        , clockRx1
        , clockRx2
        , rxActive
        }

transceiverPrbs ::
  forall tx rx tx1 rx1 ref free txS rxS.
  ( HasSynchronousReset tx
  , HasDefinedInitialValues tx
  , HasSynchronousReset rx
  , HasDefinedInitialValues rx
  , HasSynchronousReset free
  , HasDefinedInitialValues free
  , KnownDomain rx1
  , KnownDomain tx1
  , KnownDomain rxS
  , KnownDomain txS
  , KnownDomain ref
  , KnownDomain free
  ) =>
  Config free ->
  Input tx rx tx1 rx1 ref free rxS ->
  Output tx rx tx1 rx1 txS free
transceiverPrbs = transceiverPrbsWith Gth.gthCore

transceiverPrbsWith ::
  forall tx rx tx1 rx1 ref free txS rxS.
  ( HasSynchronousReset tx
  , HasDefinedInitialValues tx
  , HasSynchronousReset rx
  , HasDefinedInitialValues rx
  , HasSynchronousReset free
  , HasDefinedInitialValues free
  , KnownDomain rx1
  , KnownDomain tx1
  , KnownDomain rxS
  , KnownDomain txS
  , KnownDomain ref
  , KnownDomain free
  ) =>
  Gth.GthCore tx1 tx rx1 rx ref free txS rxS ->
  Config free ->
  Input tx rx tx1 rx1 ref free rxS ->
  Output tx rx tx1 rx1 txS free
transceiverPrbsWith gthCore opts args@Input{clock, reset} =
  when opts.debugIla debugIla `hwSeqX` result
 where
  debugIla :: Signal free ()
  debugIla =
    setName @"transceiverDebugIla"
      $ ila
        ( ( ilaConfig
              $ "ila_probe_fpgaIndex"
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
              :> "capture_ila"
              :> "trigger_ila"
              :> Nil
          )
            { advancedTriggers = True
            , stages = 1
            , depth = D1024
            }
        )
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
  tx_active = args.txActive

  result =
    Output
      { txSampling = txUserData
      , rxData = mux rxUserData (Just <$> alignedRxData0) (pure Nothing)
      , txReady = withLockRxTx rxReadyNeighborSticky
      , handshakeDoneTx = withLockRxTx prbsOkDelayed
      , handshakeDone = prbsOkDelayed
      , handshakeDoneFree = withLockRxFree prbsOkDelayed
      , txSim
      , txN = txN
      , txP = txP
      , txOutClock
      , txReset
      , rxOutClock
      , rxReset
      , linkUp
      , linkReady
      , stats
      }

  linkUp =
    withLockTxFree txUserData
      .&&. withLockRxFree rxUserData

  linkReady = linkUp .||. withLockRxFree rxReadyNeighborSticky

  ( txSim
    , txN
    , txP
    , txOutClock
    , rxOutClock
    , rx_data0
    , reset_tx_done
    , reset_rx_done
    , _txpmaresetdone_out
    , _rxpmaresetdone_out
    , rxCtrl0 :: Signal rx (BitVector 16)
    , rxCtrl1 :: Signal rx (BitVector 16)
    , rxCtrl2 :: Signal rx (BitVector 8)
    , rxCtrl3 :: Signal rx (BitVector 8)
    ) =
      gthCore
        args.channelName
        args.clockPath
        args.rxSim
        args.rxN
        args.rxP
        clock -- gtwiz_reset_clk_freerun_in
        (delayReset Asserted clock rst_all)
        -- \* filter glitches *
        (delayReset Asserted clock rst_rx)
        -- \* filter glitches *
        -- gtwiz_reset_rx_datapath_in
        gtwiz_userdata_tx_in
        txctrl
        args.refClock -- gtrefclk0_in
        args.clockTx1
        args.clockTx2
        args.txActive
        args.clockRx1
        args.clockRx2
        args.rxActive

  prbsConfig = Prbs.conf31 @48

  txClock = args.clockTx2
  rxClock = args.clockRx2

  (commas, txctrl) = Comma.generator d1 txClock txReset
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
    xpmResetSynchronizer Asserted rxClock rxClock
      $ unsafeFromActiveLow (bitCoerce <$> reset_rx_done)
      `orReset` xpmResetSynchronizer Asserted clock rxClock reset

  alignedRxData0 :: Signal rx (BitVector 64)
  alignedRxData0 =
    withClock rxClock
      $ WordAlign.alignBytesFromMsbs @8 WordAlign.alignLsbFirst (rxUserData .||. rxLast) rx_data0

  (alignedAlignBits, alignedRxData1) =
    unbundle (WordAlign.splitMsbs @8 @8 <$> alignedRxData0)

  (alignedMetaBits, alignedRxData2) =
    unbundle (WordAlign.splitMsbs @8 @7 <$> alignedRxData1)

  prbsErrors = Prbs.checker rxClock rxReset enableGen prbsConfig alignedRxData2
  anyPrbsErrors = prbsErrors ./=. pure 0
  alignError = alignedAlignBits ./=. pure WordAlign.alignSymbol

  -- We consider the control symbols as errors, as they should not be present in
  -- the user data stream. 8b/10b encoding errors naturally count as errors too.
  -- Note that the upper bits of rxCtrl0 and rxCtrl1 are unused.
  --
  -- TODO: Truncate rxCtrl0 and rxCtrl1 in GTH primitive.
  rxCtrlOrError =
    (fmap (truncateB @_ @8) rxCtrl0 ./=. pure 0)
      .||. (fmap (truncateB @_ @8) rxCtrl1 ./=. pure 0)
      .||. (rxCtrl2 ./=. pure 0)
      .||. (rxCtrl3 ./=. pure 0)

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
  rxReadyNeighbor = maybe False (.ready) <$> rxMeta

  rxUserData = sticky rxClock rxReset rxLast
  txUserData = sticky txClock txReset txLast

  -- Investigate: should `txLast` be mentioned here?
  indicateRxReady = txLast .||. withLockRxTx (prbsOkDelayed .&&. sticky rxClock rxReset args.rxReady)

  rxReadyNeighborSticky = sticky rxClock rxReset rxReadyNeighbor
  txLast = args.txStart .&&. withLockRxTx rxReadyNeighborSticky
  txLastFree = xpmCdcSingle txClock clock txLast

  metaTx :: Signal tx Meta
  metaTx =
    Meta
      <$> indicateRxReady
      <*> txLast
      -- We shouldn't sync with 'xpmCdcArraySingle' here, as the individual bits in
      -- 'fpgaIndex' are related to each other. Still, we know fpgaIndex is basically
      -- a constant so :shrug:.
      <*> xpmCdcArraySingle clock txClock opts.debugFpgaIndex
      <*> pure args.transceiverIndex

  (rst_all, rst_rx, stats) =
    ResetManager.resetManager
      opts.resetManagerConfig
      clock
      reset
      (withLockTxFree (pure True))
      (withLockRxFree (pure True))
      (withLockRxFree (prbsOk .||. rxUserData))

  txReset =
    xpmResetSynchronizer Asserted txClock txClock
      $ unsafeFromActiveLow (bitCoerce <$> tx_active)
      `orReset` unsafeFromActiveLow (bitCoerce <$> reset_tx_done)
      `orReset` xpmResetSynchronizer Asserted clock txClock reset

  withLockTxFree = Cdc.withLock txClock (unpack <$> reset_tx_done) clock reset
  withLockRxFree = Cdc.withLock rxClock (unpack <$> reset_rx_done) clock reset
  withLockRxTx = Cdc.withLock rxClock (unpack <$> reset_rx_done) txClock txReset
