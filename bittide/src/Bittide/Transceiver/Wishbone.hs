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
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Access (ReadOnly))
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access, description),
  deviceConfig,
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
  , ?byteOrder :: ByteOrder
  ) =>
  Clock free ->
  Reset free ->
  Config free ->
  Circuit
    ( BitboneMm free aw
    , Gth.Gths rx rxS tx txS ref n
    , CSignal tx (Vec n (BitVector 64))
    )
    (COutputs n tx rx free)
transceiverPrbsNWb clk rst config = circuit $ \(wb, gths, Fwd txDatas) -> do
  Fwd tOutputs <- transceiverPrbsNC clk tReset config -< (Fwd tInputs, gths)
  [wbc0, wbc1, wbs0, wbs1, wbs2] <-
    deviceWb clk rst (deviceConfig "Transceivers") -< wb

  -- Configuration registers
  (Fwd tEnable, _c0) <-
    registerWb clk rst transceiverEnableConfig False -< (wbc0, Fwd noWrite)
  (Fwd channelEnables, _c1) <-
    registerWb clk tReset channelEnablesConfig (repeat False) -< (wbc1, Fwd noWrite)

  -- Status registers
  registerWb_ clk tReset statsConfig (repeat emptyStatistics)
    -< (wbs0, Fwd (Just <$> bundle tOutputs.stats))
  registerWb_ clk tReset rxDataInitDoneConfig (repeat False)
    -< (wbs1, Fwd (Just <$> bundle tOutputs.rxDataInitDonesFree))
  registerWb_ clk tReset txDataInitDoneConfig (repeat False)
    -< (wbs2, Fwd (Just <$> bundle tOutputs.txDataInitDonesFree))

  let
    noWrite = pure Nothing

    tReset = unsafeFromActiveLow tEnable

    tInputs =
      CInputs
        { channelResets = map unsafeFromActiveLow (unbundle channelEnables)
        , txDatas = unbundle txDatas
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
          "Enable individual channels. Enabling a channel means a link will be established, provided the other side also enables it. Conversely, disabling a channel will immediately drop the link if it was up, and no data will be sent or received on that channel."
      }

  rxDataInitDoneConfig =
    (registerConfig "rx_data_init_dones")
      { description =
          "Receive data initialization procedure done. This means that the data presented on 'rxData' is word aligned and coming from the neighbor."
      }

  txDataInitDoneConfig =
    (registerConfig "tx_data_init_dones")
      { description =
          "Transmit data initialization procedure done. This mean that the data presented on the block's input is sampled and sent to the neighbor."
      }

  statsConfig =
    (registerConfig "statistics")
      { access = ReadOnly
      , description = "Various statistics from the transceiver reset manager."
      }
