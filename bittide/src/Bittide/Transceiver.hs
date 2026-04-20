-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Transceiver module for the Bittide project. This module is a wrapper around
the 'Clash.Cores.Xilinx.Gth.gthCore' function, adding additional functionality
such as PRBS generation and checking, comma insertion, and word alignment.

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
> | 1ppppppp | 0ppppppp | 0ppppppp | 0ppppppp | 0ppppppp | 0ppppppp | 0ppppppp | 0ppppppp |
> +----------+----------+----------+----------+----------+----------+----------+----------+

 * 1/0: alignment symbol
 * p: PRBS data

__Protocol__
The protocol is as follows:

Transmit:

 1. Send commas for a number of cycles
 2. Send PRBS data with word alignment bits
 3. Wait for receive side to signal it has successfully decoded PRBS data for such a long
    time that we don't expect the other side to reset anymore.
 4. Wait for that same amount of time (+a little bit) to make sure the other side can reach
    the same conclusion.
 5. Send user data

Note that the reset manager might decide to reset in steps (2) and (3).

Receive:

 1. Detect alignment symbol and shift data accordingly
 2. Check PRBS data
 3. Signal that PRBS data is OK after observing it for some time (see section __Reset manager__).
 4. Freeze alignment logic
-}
module Bittide.Transceiver where

import Clash.Explicit.Prelude
import Protocols

import Bittide.Arithmetic.Time (trueForSteps)
import Bittide.ElasticBuffer (stickyE)
import Clash.Cores.Xilinx.Xpm.Cdc.Single (xpmCdcSingle)
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
  {- ^ Reset from user logic. The transceiver will not attempt to establish a link
  while this reset is asserted. If this is asserted while a link is established,
  the link will be torn down.
  -}
  , refClock :: Clock ref
  -- ^ Reference clock. Used to synthesize transmit clock.
  , clockTx1 :: Clock tx1
  , clockTx2 :: Clock tx
  , txActive :: Signal tx (BitVector 1)
  , clockRx1 :: Clock rx1
  , clockRx2 :: Clock rx
  , rxActive :: Signal rx (BitVector 1)
  , channelName :: String
  -- ^ Channel name, example \"X0Y18\"
  , clockPath :: String
  -- ^ Clock path, example \"clk0-2\"
  , rxSim :: Gth.SimWire rx
  -- ^ Simulation only construct. Data for the receive side. Used for testing.
  , rxN :: Gth.Wire rxS
  , rxP :: Gth.Wire rxS
  , txData :: Signal tx (BitVector 64)
  {- ^ Data to transmit to the neighbor. This data is sampled if 'Output.txDataInitDone'
  is asserted.
  -}
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
  }

data CInputs tx rx free n = CInputs
  { channelResets :: Vec n (Reset free)
  -- ^ Resets for each individual link
  , txDatas :: Vec n (Signal tx (BitVector 64))
  -- ^ See 'Input.txData'
  }

instance Protocol (CInputs tx rx free n) where
  type Fwd (CInputs tx rx free n) = CInputs tx rx free n
  type Bwd (CInputs tx rx free n) = ()

data Output tx rx tx1 rx1 txS free = Output
  { txOutClock :: Clock tx1
  -- ^ Must be routed through xilinxGthUserClockNetworkTx or equivalent to get usable clocks
  , txReset :: Reset tx
  {- ^ Reset for 'Input.clockTx2'. Clock can be unstable until this reset is
  deasserted. See documentation of 'transceiverPrbsN' for more information.
  -}
  , txP :: Gth.Wire txS
  -- ^ Transmit data (and implicitly a clock), positive
  , txN :: Gth.Wire txS
  -- ^ Transmit data (and implicitly a clock), negative
  , txSim :: Gth.SimWire tx
  -- ^ Simulation only construct. Data for the transmit side. Used for testing.
  , rxOutClock :: Clock rx1
  -- ^ Must be routed through xilinxGthUserClockNetworkRx or equivalent to get usable clocks
  , rxReset :: Reset rx
  {- ^ Reset signal for the receive side. Clock can be unstable until this reset
  is deasserted.
  -}
  , rxData :: Signal rx (BitVector 64)
  {- ^ User data received from the neighbor. Valid (but not necessary a user word) when
  'Output.rxReset' is deasserted and 'Output.rxDataInitDone' is asserted.
  -}
  , rxDataInitDone :: Signal rx Bool
  {- ^ Receive data initialization procedure done. This means that the data presented on
  'rxData' is word aligned and coming from the neighbor. Note that you might still see
  PRBS / initialization data coming from the other side. If you need first-word boundaries
  you'll need to connect a handshake component.
  -}
  , rxDataInitDoneFree :: Signal free Bool
  -- ^ 'rxDataInitDone', but synchronized to @free@ domain
  , txDataInitDone :: Signal tx Bool
  {- ^ Transmit data initialization procedure done. This mean that the data presented on
  'Input.txData' is sampled and sent to the neighbor.
  -}
  , txDataInitDoneFree :: Signal free Bool
  -- ^ 'Output.txDataInitDone' but in the free domain.
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
  , rxDatas :: Vec n (Signal rx (BitVector 64))
  -- ^ See 'Output.rxData'
  , rxDataInitDones :: Vec n (Signal rx Bool)
  -- ^ See 'Output.rxDataInitDone'
  , rxDataInitDonesFree :: Vec n (Signal free Bool)
  -- ^ See 'Output.rxDataInitDoneFree'
  , txDataInitDones :: Vec n (Signal tx Bool)
  -- ^ See 'Output.txDataInitDone'
  , txDataInitDonesFree :: Vec n (Signal free Bool)
  -- ^ See 'Output.txDataInitDoneFree'
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
  , rxClocks :: Vec n (Clock rx)
  -- ^ See 'Output.rxClock'
  , rxResets :: Vec n (Reset rx)
  -- ^ See 'Output.rxReset'
  , rxDatas :: Vec n (Signal rx (BitVector 64))
  -- ^ See 'Output.rxData'
  , rxDataInitDones :: Vec n (Signal rx Bool)
  -- ^ See 'Output.rxDataInitDone'
  , rxDataInitDonesFree :: Vec n (Signal free Bool)
  -- ^ See 'Output.rxDataInitDoneFree'
  , txDataInitDones :: Vec n (Signal tx Bool)
  -- ^ See 'Output.txDataInitDone'
  , txDataInitDonesFree :: Vec n (Signal free Bool)
  -- ^ See 'Output.txDataInitDoneFree'
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
that aren't visible outside of transceiverPrbsN. To do this completely clean/safe
transceiverPrbsN should have two extra forall arguments, two extra KnownDomain constraints.
And either some Proxy arguments or we would have to enable AllowAmbiguousTypes.

Because the tx1/rx1 domains aren't visible outside 'transceiverPrbsN', I chose to sidestep
the extra complication and pretend tx1/rx1 and tx/rx are the same. This disables the type
checking safety we'd normally get from Clash, but Vivado should call us out when we make a
mistake.
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
    , rxClocks = outputs.rxClocks
    , rxResets = outputs.rxResets
    , rxDatas = outputs.rxDatas
    , rxDataInitDones = outputs.rxDataInitDones
    , rxDataInitDonesFree = outputs.rxDataInitDonesFree
    , txDataInitDones = outputs.txDataInitDones
    , txDataInitDonesFree = outputs.txDataInitDonesFree
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
    (COutputs n tx rx free)
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
    , txSims = simOnlyHdlWorkaround (SimOnly (map (Gth.unSimOnly . (.txSim)) outputs))
    , txDataInitDones = map (.txDataInitDone) outputs
    , -- rx
      rxClocks = rxClocks
    , rxResets = map (.rxReset) outputs
    , rxDatas = map (.rxData) outputs
    , rxDataInitDones = map (.rxDataInitDone) outputs
    , -- transceiver
      txPs = pack <$> bundle (map (.txP) outputs)
    , txNs = pack <$> bundle (map (.txN) outputs)
    , -- free
      txDataInitDonesFree = map (.txDataInitDoneFree) outputs
    , rxDataInitDonesFree = map (.rxDataInitDoneFree) outputs
    , stats = map (.stats) outputs
    }
 where
  -- XXX: 'outputs' used to be written with zipWithN, to workaround bugs:
  --       * https://github.com/clash-lang/clash-compiler/issues/2723
  --       * https://github.com/clash-lang/clash-compiler/issues/2722
  -- That breaks the instantiation of the the debug ILAs inside 'transceiverPrbs'.
  --
  -- But when the GTHs were changed to use external "user clock networks",
  -- this zipWithN became unusably slow when using more than ~4 transceivers.
  -- Unfortunately this means debugIla is broken now, when using more than 1 transceiver.
  outputs =
    go txClockNw
      <$> inputs.channelNames
      <*> inputs.clockPaths
      <*> simOnlyHdlWorkaround (map SimOnly (Gth.unSimOnly inputs.rxSims))
      <*> unbundle (unpack <$> inputs.rxNs)
      <*> unbundle (unpack <$> inputs.rxPs)
      <*> inputs.channelResets
      <*> inputs.txDatas
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

  go (clockTx1, clockTx2, txActive) channelName clockPath rxSim rxN rxP channelReset txData (clockRx1, clockRx2, rxActive) =
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
transceiverPrbsWith gthCore opts input = output
 where
  output =
    Output
      { txSim
      , txN
      , txP
      , txReset = txDomainReset
      , txOutClock
      , rxOutClock
      , rxReset
      , rxData = alignedRxData0
      , stats
      , rxDataInitDone
      , rxDataInitDoneFree
      , txDataInitDone = txDataInitDoneTx
      , txDataInitDoneFree
      }

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
          { channel = input.channelName
          , refClkSpec = input.clockPath
          , gthrxIn = input.rxSim
          , gthrxnIn = input.rxN
          , gthrxpIn = input.rxP
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
          , gtrefclk0In = input.refClock -- gtrefclk0_in
          , txusrclkIn = input.clockTx1
          , txusrclk2In = input.clockTx2
          , gtwizUserclkTxActiveIn = input.txActive
          , rxusrclkIn = input.clockRx1
          , rxusrclk2In = input.clockRx2
          , gtwizUserclkRxActiveIn = input.rxActive
          }

  prbsConfig = Prbs.conf31 @56

  clock = input.clock
  reset = input.reset
  txClock = input.clockTx2
  rxClock = input.clockRx2

  (commas, txctrl) = Comma.generator d1 txClock txReset
  commasDone = isNothing <$> commas
  prbs = Prbs.generator txClock (unsafeFromActiveLow commasDone) enableGen prbsConfig
  prbsWithAlign = WordAlign.joinMsbs @8 WordAlign.alignSymbol <$> prbs
  gtwiz_userdata_tx_in =
    mux
      txDataInitDoneTx
      input.txData
      (fromMaybe <$> prbsWithAlign <*> commas)

  rxReset =
    xpmResetSynchronizer Asserted rxClock rxClock
      $ unsafeFromActiveLow (bitCoerce <$> reset_rx_done)
      `orReset` xpmResetSynchronizer Asserted clock rxClock reset

  alignedRxData0 :: Signal rx (BitVector 64)
  alignedRxData0 =
    withClock rxClock
      $ WordAlign.alignBytesFromMsbs @8 WordAlign.alignLsbFirst prbsOkDelayedSticky rx_data0

  (alignedAlignBits, alignedRxData1) =
    unbundle (WordAlign.splitMsbs @8 @8 <$> alignedRxData0)

  prbsErrors = Prbs.checker rxClock rxReset enableGen prbsConfig alignedRxData1
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

  prbsOk = Prbs.tracker rxClock rxReset (anyPrbsErrors .||. alignError .||. rxCtrlOrError)

  -- 'prbsWaitMs' is the number of milliseconds representing the worst case time it takes
  -- for the PRBS to stabilize. I.e., after this time we can be sure the neighbor doesn't
  -- reset its transceiver anymore. We add a single retry to account for clock speed
  -- variations.
  prbsWaitMs =
    ((1 :: Index 2) `add` opts.resetManagerConfig.rxRetries)
      `mul` opts.resetManagerConfig.rxTimeoutMs

  -- We assume that the link is established after waiting for 'prbsWaitMs' and won't switch
  -- out off it. Recovering for errors is handled by the 'ResetManager' after observing
  -- encoding errors, or by the application layer.
  prbsOkDelayed = trueForSteps (Proxy @(Milliseconds 1)) prbsWaitMs rxClock rxReset prbsOk
  prbsOkDelayedSticky = stickyE rxClock rxReset prbsOkDelayed
  rxDataInitDone = prbsOkDelayedSticky
  rxDataInitDoneFree = withLockRxFree rxDataInitDone

  -- After we assume the RX side is stable, we cannot be sure the other side has concluded
  -- the same yet. We therefore wait the worst case time it takes for the PRBS to stabilize
  -- (see 'prbsWaitMs') and add another retry to account for clock speed variations.
  prbsWaitMsPessimistic =
    ((2 :: Index 3) `add` opts.resetManagerConfig.rxRetries)
      `mul` opts.resetManagerConfig.rxTimeoutMs
  txGraceDoneSticky =
    -- Note that this variable is sticky because 'prbsOkDelayedSticky' is
    trueForSteps (Proxy @(Milliseconds 1)) prbsWaitMsPessimistic rxClock rxReset prbsOkDelayedSticky
  txDataInitDone = txGraceDoneSticky
  txDataInitDoneFree = withLockRxFree txGraceDoneSticky
  txDataInitDoneTx = xpmCdcSingle rxClock txClock txDataInitDone

  errorAfterRxInitDone :: Signal rx Bool
  errorAfterRxInitDone = mux rxDataInitDone rxCtrlOrError (pure False)

  (resets, stats) =
    ResetManager.resetManager
      opts.resetManagerConfig
      clock
      reset
      ResetManager.Input
        { channelReset = input.channelReset
        , txInitDone = withLockTxFree (pure True) -- See note @ withLockRxFree
        , rxInitDone = withLockRxFree (pure True) -- See note @ withLockRxFree
        , rxDataGood = withLockRxFree (prbsOk .||. prbsOkDelayedSticky)
        , errorAfterRxUser = withLockRxFree errorAfterRxInitDone
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
  withLockTxFree = Cdc.withLock txClock (unpack <$> reset_tx_done) clock reset
{-# INLINE transceiverPrbsWith #-}
