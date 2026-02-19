-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Transceiver module for the Bittide project. This module is a wrapper around
the 'Clash.Cores.Xilinx.Gth.gthCore' function, adding additional functionality
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
neighbor that we're ready to receive user data, ready to transmit user data, or
that the next word will be user data.

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
 4. Send meta data with user supplied 'Input.rxReady' and 'Input.txStart'
 5. Wait for 'Input.rxReady', 'Input.txStart' and its neighbor to signal it is ready to
    receive user data.
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
import Protocols

import Bittide.Arithmetic.Time (trueForSteps)
import Bittide.ElasticBuffer (sticky)
import Clash.Explicit.Reset.Extra (Asserted (Asserted), delayReset, xpmResetSynchronizer)
import Clash.Prelude (withClock)
import Data.Maybe (fromMaybe, isNothing)
import Data.Proxy (Proxy (Proxy))
import GHC.Stack (HasCallStack)

import qualified Bittide.Transceiver.Cdc as Cdc
import qualified Bittide.Transceiver.Comma as Comma
import qualified Bittide.Transceiver.Prbs as Prbs
import qualified Bittide.Transceiver.ResetManager as ResetManager
import qualified Bittide.Transceiver.WordAlign as WordAlign
import qualified Clash.Cores.Xilinx.Gth as Gth

{- | Meta information send along with the PRBS and alignment symbols. See module
documentation for more information.
-}
data Meta = Meta
  { readyToReceive :: Bool
  -- ^ Ready to receive user data
  , readyToTransmit :: Bool
  -- ^ Ready to transmit user data
  , lastPrbsWord :: Bool
  -- ^ Next word will be user data
  , padding :: Unsigned 5
  -- ^ Padding up to 1 byte
  }
  deriving (Generic, NFDataX, BitPack)

data Config dom = Config
  { resetManagerConfig :: ResetManager.Config
  -- ^ Configuration for 'ResetManager.resetManager'
  }

defConfig :: Config dom
defConfig =
  Config
    { resetManagerConfig = ResetManager.defConfig
    }

data Input tx rx tx1 rx1 ref free rxS = Input
  { clock :: Clock free
  -- ^ Any "always on" clock
  , reset :: Reset free
  -- ^ Reset signal for the entire transceiver
  , channelReset :: Reset free
  -- ^ Reset from user logic. The transceiver will not attempt to establish a link
  -- while this reset is asserted. If this is asserted while a link is established,
  -- the link will be torn down.
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
  , channelResets :: Vec n (Reset free)
  -- ^ Resets for each individual link
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

data CInputs tx rx free n = CInputs
  { channelResets :: Vec n (Reset free)
  -- ^ Resets for each individual link
  , txDatas :: Vec n (Signal tx (BitVector 64))
  -- ^ See 'Input.txData'
  , txStarts :: Vec n (Signal tx Bool)
  -- ^ See 'Input.txStart'
  , rxReadys :: Vec n (Signal rx Bool)
  -- ^ See 'Input.rxReady'
  }

instance Protocol (CInputs tx rx free n) where
  type Fwd (CInputs tx rx free n) = CInputs tx rx free n
  type Bwd (CInputs tx rx free n) = ()

data Output tx rx tx1 rx1 txS free = Output
  { txOutClock :: Clock tx1
  -- ^ Must be routed through xilinxGthUserClockNetworkTx or equivalent to get usable clocks
  , txReset :: Reset tx
  -- ^ Reset for 'Input.clockTx2'. Clock can be unstable until this reset is
  -- deasserted. See documentation of 'transceiverPrbsN' for more information.
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
  , debugLinkUp :: Signal free Bool
  -- ^ Legacy field, not yet removed because it is used by a HITL test. True if both the transmit
  --  and receive side are either handling user data.
  , debugLinkReady :: Signal free Bool
  -- ^ Legacy field, not yet removed because it is used by a HITL test. True if both the transmit
  --  and receive side ready to handle user data or doing so. I.e., 'debugLinkUp' implies
  -- 'debugLinkReady'.
  , handshakeDoneFree :: Signal free Bool
  -- ^ Asserted when link has been established, but not necessarily handling user data.
  -- This signal is native to the 'rx' domain. If you need that, use 'handshakeDone'.
  , neighborReceiveReady :: Signal free Bool
  -- ^ Asserted when the neighbor has signalled it is ready to receive user data.
  , neighborTransmitReady :: Signal free Bool
  -- ^ Asserted when the neighbor has signalled it is ready to transmit user data.
  , stats :: Signal free ResetManager.Statistics
  -- ^ Statistics exported by 'ResetManager.resetManager'. Useful for debugging.
  }

{- | Careful: the domains for the rx side of each transceiver are different, even if their
types say otherwise.
-}
data Outputs n tx rx txS free = Outputs
  { txClock :: Clock tx
  -- ^ Single transmit clock, shared by all links
  , txReset :: Reset tx
  -- ^ Reset associated with 'txClock'. Once deasserted, the clock is stable.
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
  , debugLinkUps :: Vec n (Signal free Bool)
  -- ^ See 'Output.debugLinkUp'
  , debugLinkReadys :: Vec n (Signal free Bool)
  -- ^ See 'Output.debugLinkReady'
  , handshakesDoneFree :: Vec n (Signal free Bool)
  -- ^ See 'Output.handshakeDoneFree'
  , neighborReceiveReadys :: Vec n (Signal free Bool)
  -- ^ See 'Output.neighborReceiveReady'
  , neighborTransmitReadys :: Vec n (Signal free Bool)
  -- ^ See 'Output.neighborTransmitReady'
  , stats :: Vec n (Signal free ResetManager.Statistics)
  -- ^ See 'Output.stats'
  }

{- | Careful: the domains for the rx side of each transceiver are different, even if their
types say otherwise.
-}
data COutputs n tx rx free = COutputs
  { txClock :: Clock tx
  -- ^ See 'Output.txClock'
  , txReset :: Reset tx
  -- ^ See 'Output.txReset'
  , txReadys :: Vec n (Signal tx Bool)
  -- ^ See 'Output.txReady'
  , txSamplings :: Vec n (Signal tx Bool)
  -- ^ See 'Output.txSampling'
  , handshakesDoneTx :: Vec n (Signal tx Bool)
  -- ^ See 'Output.handshakeDoneTx'
  , rxClocks :: Vec n (Clock rx)
  -- ^ See 'Output.rxClock'
  , rxResets :: Vec n (Reset rx)
  -- ^ See 'Output.rxReset'
  , rxDatas :: Vec n (Signal rx (Maybe (BitVector 64)))
  -- ^ See 'Output.rxData'
  , handshakesDone :: Vec n (Signal rx Bool)
  -- ^ See 'Output.handshakeDone'
  , debugLinkUps :: Vec n (Signal free Bool)
  -- ^ See 'Output.debugLinkUp'
  , debugLinkReadys :: Vec n (Signal free Bool)
  -- ^ See 'Output.debugLinkReady'
  , handshakesDoneFree :: Vec n (Signal free Bool)
  -- ^ See 'Output.handshakeDoneFree'
  , neighborReceiveReadys :: Vec n (Signal free Bool)
  -- ^ See 'Output.neighborReceiveReady'
  , neighborTransmitReadys :: Vec n (Signal free Bool)
  -- ^ See 'Output.neighborTransmitReady'
  , stats :: Vec n (Signal free ResetManager.Statistics)
  -- ^ See 'Output.stats'
  }

instance Protocol (COutputs n tx rx free) where
  type Fwd (COutputs n tx rx free) = COutputs n tx rx free
  type Bwd (COutputs n tx rx free) = ()

data
  Transceivers
    (tx :: Domain)
    (rx :: Domain)
    (ref :: Domain)
    (free :: Domain)
    (txS :: Domain)
    (rxS :: Domain)
    (n :: Nat)

instance Protocol (Transceivers tx rx ref free txS rxS n) where
  type Fwd (Transceivers tx rx ref free txS rxS n) = Inputs tx rx ref free rxS n
  type Bwd (Transceivers tx rx ref free txS rxS n) = Outputs n tx rx txS free

instance Protocol (Inputs tx rx ref free rxS n) where
  type Fwd (Inputs tx rx ref free rxS n) = Inputs tx rx ref free rxS n
  type Bwd (Inputs tx rx ref free rxS n) = ()

instance Protocol (Outputs n tx rx txS free) where
  type Fwd (Outputs n tx rx txS free) = Outputs n tx rx txS free
  type Bwd (Outputs n tx rx txS free) = ()

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

outputsToCOutputs ::
  Outputs n tx rx txS free ->
  COutputs n tx rx free
outputsToCOutputs outputs =
  COutputs
    { txClock = outputs.txClock
    , txReset = outputs.txReset
    , txReadys = outputs.txReadys
    , txSamplings = outputs.txSamplings
    , handshakesDoneTx = outputs.handshakesDoneTx
    , rxClocks = outputs.rxClocks
    , rxResets = outputs.rxResets
    , rxDatas = outputs.rxDatas
    , handshakesDone = outputs.handshakesDone
    , debugLinkUps = outputs.debugLinkUps
    , debugLinkReadys = outputs.debugLinkReadys
    , handshakesDoneFree = outputs.handshakesDoneFree
    , neighborReceiveReadys = outputs.neighborReceiveReadys
    , neighborTransmitReadys = outputs.neighborTransmitReadys
    , stats = outputs.stats
    }

transceiverPrbsNC ::
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
  Clock free ->
  Reset free ->
  Config free ->
  Circuit
    ( CInputs tx rx free n
    , Gth.Gths rx rxS tx txS ref n
    )
    ( COutputs n tx rx free
    )
transceiverPrbsNC clock reset config = Circuit go
 where
  go ::
    ( ( CInputs tx rx free n
      , ( Clock ref
        , Gth.SimWires rx n
        , Gth.Wires rxS n
        , Gth.Wires rxS n
        , Vec n String
        , Vec n String
        )
      )
    , ()
    ) ->
    ( ((), (Gth.SimWires tx n, Gth.Wires txS n, Gth.Wires txS n))
    , COutputs n tx rx free
    )
  go ((cInputs, (refClock, rxSims, rxNs, rxPs, channelNames, clockPaths)), _) =
    (((), (txSims, txNs, txPs)), cOutputs)
   where
    cOutputs = outputsToCOutputs outputs

    outputs@Outputs{txPs, txNs, txSims} =
      transceiverPrbsN
        config
        Inputs
          { clock
          , reset
          , channelResets = cInputs.channelResets
          , refClock
          , channelNames
          , clockPaths
          , rxSims
          , rxNs
          , rxPs
          , txDatas = cInputs.txDatas
          , txStarts = cInputs.txStarts
          , rxReadys = cInputs.rxReadys
          }

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
      txClock
    , txReset
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
      debugLinkUps = map (.debugLinkUp) outputs
    , debugLinkReadys = map (.debugLinkReady) outputs
    , handshakesDoneFree = map (.handshakeDoneFree) outputs
    , neighborReceiveReadys = map (.neighborReceiveReady) outputs
    , neighborTransmitReadys = map (.neighborTransmitReady) outputs
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
      <*> inputs.channelResets
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
  txReset = (head outputs).txReset

  -- see [NOTE: duplicate tx/rx domain]
  txClockNw = Gth.xilinxGthUserClockNetworkTx @tx @tx txOutClk txUsrClkRst
  (_txClk1s, txClock, _txClkActives) = txClockNw

  rxOutClks = map (.rxOutClock) outputs
  -- see [NOTE: duplicate tx/rx domain]
  rxClockNws = map (flip (Gth.xilinxGthUserClockNetworkRx @rx @rx) rxUsrClkRst) rxOutClks
  (_rxClk1s, rxClocks, _rxClkActives) = unzip3 rxClockNws

  go (clockTx1, clockTx2, txActive) transceiverIndex channelName clockPath rxSim rxN rxP channelReset txData txStart rxReady (clockRx1, clockRx2, rxActive) =
    transceiverPrbs
      opts
      Input
        { channelName
        , clockPath
        , rxSim
        , rxN
        , rxP
        , channelReset
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
    , txReset = txDomainReset
    , txOutClock
    , rxOutClock
    , rxReset
    , debugLinkUp
    , debugLinkReady
    , neighborReceiveReady = withLockRxFree rxReadyNeighborSticky
    , neighborTransmitReady = withLockRxFree txReadyNeighborSticky
    , stats
    }
 where
  debugLinkUp =
    withLockTxFree txUserData
      .&&. withLockRxFree rxUserData

  debugLinkReady = debugLinkUp .||. withLockRxFree rxReadyNeighborSticky

  Gth.CoreOutput
    { gthtxOut = txSim
    , gthtxnOut = txN
    , gthtxpOut = txP
    , txoutclkOut = txOutClock
    , rxoutclkOut = rxOutClock
    , gtwizUserdataRxOut = rx_data0
    , gtwizResetTxDoneOut = reset_tx_done
    , gtwizResetRxDoneOut = reset_rx_done
    , txpmaresetdoneOut = _txpmaresetdone_out
    , rxpmaresetdoneOut = _rxpmaresetdone_out
    , rxctrl0Out = rxCtrl0 :: Signal rx (BitVector 16)
    , rxctrl1Out = rxCtrl1 :: Signal rx (BitVector 16)
    , rxctrl2Out = rxCtrl2 :: Signal rx (BitVector 8)
    , rxctrl3Out = rxCtrl3 :: Signal rx (BitVector 8)
    } =
      gthCore
        Gth.CoreInput
          { channel = args.channelName
          , refClkSpec = args.clockPath
          , gthrxIn = args.rxSim
          , gthrxnIn = args.rxN
          , gthrxpIn = args.rxP
          , gtwizResetClkFreerunIn = clock -- gtwiz_reset_clk_freerun_in
          -- We insert 'delayReset' to filter out glitches in the reset signals. That
          -- is, a synchronous reset may glitch/stabilize outside of the setup and
          -- hold window. An asynchronous reset (the ones on the GTH core) cannot
          -- do so and should therefore come straight from a flipflop.
          , gtwizResetAllIn = delayReset Asserted clock resets.all
          , gtwizResetTxPllAndDatapathIn = delayReset Asserted clock resets.txPllAndDatapath
          , gtwizResetTxDatapathIn = delayReset Asserted clock resets.txDatapath
          , gtwizResetRxPllAndDatapathIn = delayReset Asserted clock resets.rxPllAndDatapath
          , gtwizResetRxDatapathIn = delayReset Asserted clock resets.rxDatapath
          , gtwizUserdataTxIn = gtwiz_userdata_tx_in
          , txctrl2In = txctrl
          , gtrefclk0In = args.refClock -- gtrefclk0_in
          , txusrclkIn = args.clockTx1
          , txusrclk2In = args.clockTx2
          , gtwizUserclkTxActiveIn = args.txActive
          , rxusrclkIn = args.clockRx1
          , rxusrclk2In = args.clockRx2
          , gtwizUserclkRxActiveIn = args.rxActive
          }

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
  rxReadyNeighbor = maybe False (.readyToReceive) <$> rxMeta
  txReadyNeighbor = maybe False (.readyToTransmit) <$> rxMeta

  rxUserData = sticky rxClock rxReset rxLast
  txUserData = sticky txClock txReset txLast

  indicateRxReady = withLockRxTx (prbsOkDelayed .&&. sticky rxClock rxReset args.rxReady)

  rxReadyNeighborSticky = sticky rxClock rxReset rxReadyNeighbor
  txReadyNeighborSticky = sticky rxClock rxReset txReadyNeighbor
  txLast = indicateRxReady .&&. args.txStart .&&. withLockRxTx rxReadyNeighborSticky

  metaTx :: Signal tx Meta
  metaTx =
    Meta
      <$> indicateRxReady
      <*> (withLockRxTx prbsOkDelayed .&&. args.txStart)
      <*> txLast
      -- Padding
      <*> pure 0

  errorAfterRxUserData :: Signal rx Bool
  errorAfterRxUserData = mux rxUserData rxCtrlOrError (pure False)

  (resets, stats) =
    ResetManager.resetManager
      opts.resetManagerConfig
      clock
      reset
      ResetManager.Input
        { channelReset = args.channelReset
        , txInitDone = withLockTxFree (pure True) -- See note @ withLockRxFree
        , rxInitDone = withLockRxFree (pure True) -- See note @ withLockRxFree
        , rxDataGood = withLockRxFree (prbsOk .||. rxUserData)
        , errorAfterRxUser = withLockRxFree errorAfterRxUserData
        }

  -- Synchronized version of 'resets.txUser' and 'resets.txDomain'. We use
  -- 'holdReset', because 'txUser' and 'txDomain' can be asserted for a single
  -- clock cycle, while 'xpmResetSynchronizer' requires that the target domain
  -- samples at least 2 clock cycles. A value of 32 means that the target domain
  -- (TX) can therefore be 16 times slower than the source domain (free).
  txReset =
    xpmResetSynchronizer Asserted clock txClock
      $ holdReset clock enableGen d32 resets.txUser

  txDomainReset =
    xpmResetSynchronizer Asserted clock txClock
      $ holdReset clock enableGen d32 resets.txDomain

  -- Both the TX and RX domain may sometimes be disabled entirely. To prevent
  -- seeing stale signals, 'withLock' is employed. This means that constructs
  -- such as @withLockRxFree (pure True)@ are actually doing something!
  withLockRxFree = Cdc.withLock rxClock (unpack <$> reset_rx_done) clock reset
  withLockRxTx = Cdc.withLock rxClock (unpack <$> reset_rx_done) txClock txReset
  withLockTxFree = Cdc.withLock txClock (unpack <$> reset_tx_done) clock reset
