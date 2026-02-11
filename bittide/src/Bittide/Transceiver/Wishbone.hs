-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Transceiver.Wishbone where

import Clash.Explicit.Prelude
import Protocols

import Bittide.SharedTypes (BitboneMm)
import Bittide.Transceiver (CInputs (..), COutputs (..), Config, transceiverPrbsNC)

import Bittide.Transceiver.ResetManager (emptyStatistics)
import Clash.Class.BitPackC (ByteOrder)
import Clash.Cores.Xilinx.Xpm.Cdc (xpmCdcArraySingle, xpmCdcSingle)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Access (ReadOnly))
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access, description),
  deviceWb,
  registerConfig,
  registerWb,
  registerWb_,
 )

import qualified Clash.Cores.Xilinx.Gth as Gth

{- | Wishbone wrapper around 'transceiverPrbsNC', with registers to control and
monitor the transceiver block.
-}
transceiverPrbsNWb ::
  forall tx rx ref free txS rxS n m aw.
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
  , KnownNat aw
  , HasCallStack
  , 4 <= aw
  , n <= 1024
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  Clock free ->
  Reset free ->
  Config free ->
  Circuit
    ( (BitboneMm free aw)
    , Gth.Gths rx rxS tx txS ref n
    , CSignal tx (Vec n (BitVector 64))
    )
    ( COutputs n tx rx free
    )
transceiverPrbsNWb clk rst config = circuit $ \(wb, gths, Fwd txDatas) -> do
  Fwd tOutputs <- transceiverPrbsNC clk tReset config -< (Fwd tInputs, gths)
  [wbc0, wbc1, wbc2, wbc3, wbs0, wbs1] <- deviceWb "Transceivers" -< wb

  -- Configuration registers
  (Fwd tEnable, _c0) <-
    registerWb clk rst transceiverEnableConfig False -< (wbc0, Fwd noWrite)
  (Fwd channelEnables, _c1) <-
    registerWb clk tReset channelEnablesConfig (repeat False) -< (wbc1, Fwd noWrite)
  (Fwd rxReadys, _c2) <-
    registerWb clk tReset rxReadysConfig (repeat False) -< (wbc2, Fwd noWrite)
  (Fwd txStarts, _c3) <-
    registerWb clk tReset txStartsConfig (repeat False) -< (wbc3, Fwd noWrite)

  -- Status registers
  registerWb_ clk tReset statsConfig (repeat emptyStatistics)
    -< (wbs0, Fwd (Just <$> bundle tOutputs.stats))
  registerWb_ clk tReset handshakesDoneConfig (repeat False)
    -< (wbs1, Fwd (Just <$> bundle tOutputs.handshakesDoneFree))

  let
    noWrite = pure Nothing

    tReset = unsafeFromActiveLow tEnable

    tInputs =
      CInputs
        { channelResets = map unsafeFromActiveLow (unbundle channelEnables)
        , txDatas = unbundle txDatas
        , txStarts = unbundle (xpmCdcArraySingle clk tOutputs.txClock txStarts)
        , rxReadys = xpmCdcSingle clk <$> tOutputs.rxClocks <*> unbundle rxReadys
        }

  idC -< Fwd tOutputs
 where
  transceiverEnableConfig =
    (registerConfig "transceiver_enable")
      { description =
          "Enable transceiver block. Disabling this resets all channels and registers. Enabling the transceiver will bring up the bittide domain. Conversely, disabling it will bring down the bittide domain."
      }

  channelEnablesConfig =
    (registerConfig "channel_enables")
      { description =
          "Enable individual channels. Enabling a channel means a link will be established, provided the other side also enables it. It does not mean (user) data will be sent or received on that channel, see 'receive_readys' and 'transmit_starts'. Conversely, disabling a channel will immediately drop the link if it was up, and no data will be sent or received on that channel."
      }

  rxReadysConfig =
    (registerConfig "receive_readys")
      { description =
          "Indicate ready to receive (non-link negotiation) data. You should only set this when you are actually ready to receive data, as setting it when not ready will lead to data loss. This typically means that the elastic buffer on the receiver side has space to store incoming data. Though this can be set independently of 'channel_enables', this setting has no effect unless the corresponding channel is also enabled. Once set, there is no way to retract readiness until the next time the channel is disabled."
      }

  txStartsConfig =
    (registerConfig "transmit_starts")
      { description =
          "Indicate ready to transmit (non-link negotiation) data. Note that the transceiver block will not transition to sending data until you indicate the link is also ready to receive data (see 'receive_readys'). Though this can be set independently of 'channel_enables', this setting has no effect unless the corresponding channel is also enabled. Once set, there is no way to retract readiness until the next time the channel is disabled."
      }

  statsConfig =
    (registerConfig "statistics")
      { access = ReadOnly
      , description = "Various statistics from the transceiver reset manager."
      }

  handshakesDoneConfig =
    (registerConfig "handshakes_done")
      { access = ReadOnly
      , description =
          "Indicates whether link negotiation has completed for each channel. This means that, bar catastrophic failure, the link will be able to transfer user data."
      }
