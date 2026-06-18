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
import Data.Maybe (fromMaybe, isJust)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Access (ReadOnly))
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access),
  busActivityWrite,
  deviceConfig,
  deviceWb,
  registerConfig,
  registerWb,
  registerWb_,
 )

import qualified Bittide.Transceiver.Drp as Drp
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
  [wbc0, wbc1, wbs0, wbs1, wbs2, wbDrpReq, wbDrpStatus] <-
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

  -- DRP (Dynamic Reconfiguration Port) access. Note these use the device 'rst'
  -- (not 'tReset'), so software can read/write GTH attributes even while the
  -- transceiver/channels are disabled.
  (_drpReqVal, Fwd drpReqActivity) <-
    registerWb clk rst drpRequestConfig defDrpRequest -< (wbDrpReq, Fwd noWrite)
  registerWb_ clk rst drpStatusConfig defDrpStatus
    -< (wbDrpStatus, Fwd (Just <$> drpStatus))

  let
    noWrite = pure Nothing

    tReset = unsafeFromActiveLow tEnable

    -- A software write to 'drp_request' starts a single DRP transaction.
    drpStart = busActivityWrite <$> drpReqActivity

    (drpAddrs, drpDis, drpEns, drpWes, drpResp, drpBusy) =
      Drp.drpMaster clk rst enableGen drpStart tOutputs.drpDos tOutputs.drpRdys

    -- Latch the result of the last completed transaction for software to read.
    drpRespReg =
      regEn
        clk
        rst
        enableGen
        (Drp.Response 0 False)
        (isJust <$> drpResp)
        (fromMaybe (Drp.Response 0 False) <$> drpResp)

    drpStatus =
      ( \busy resp ->
          Drp.DrpStatus{busy, timedOut = resp.timedOut, readData = resp.readData}
      )
        <$> drpBusy
        <*> drpRespReg

    tInputs =
      CInputs
        { channelResets = map unsafeFromActiveLow (unbundle channelEnables)
        , txDatas = unbundle txDatas
        , drps = zip4 drpAddrs drpDis drpEns drpWes
        }

  idC -< Fwd tOutputs
 where
  transceiverEnableConfig =
    registerConfig
      "transceiver_enable"
      "Enable transceiver block. Disabling this resets all channels and registers. Enabling the transceiver will bring up the bittide domain. Conversely, disabling it will bring down the bittide domain."

  channelEnablesConfig =
    registerConfig
      "channel_enables"
      "Enable individual channels. Enabling a channel means a link will be established, provided the other side also enables it. Conversely, disabling a channel will immediately drop the link if it was up, and no data will be sent or received on that channel."

  rxDataInitDoneConfig =
    registerConfig
      "rx_data_init_dones"
      "Receive data initialization procedure done. This means that the data presented on 'rxData' is word aligned and coming from the neighbor."

  txDataInitDoneConfig =
    registerConfig
      "tx_data_init_dones"
      "Transmit data initialization procedure done. This mean that the data presented on the block's input is sampled and sent to the neighbor."

  statsConfig =
    (registerConfig "statistics" "Various statistics from the transceiver reset manager.")
      { access = ReadOnly
      }

  defDrpRequest =
    Drp.DrpRequest{channel = 0, address = 0, writeData = 0, isWrite = False}

  defDrpStatus =
    Drp.DrpStatus{busy = False, timedOut = False, readData = 0}

  drpRequestConfig =
    registerConfig
      "drp_request"
      "Issue a GTH DRP (Dynamic Reconfiguration Port) transaction. Writing this register starts a single read or write of the addressed channel's DRP register. Fields: 'channel' (which GTH channel), 'address' (9-bit DRP address), 'write_data' (16-bit, used for writes), 'is_write'. Poll 'drp_status' for completion."

  drpStatusConfig =
    ( registerConfig
        "drp_status"
        "Status/result of the last GTH DRP transaction. 'busy' is high while a transaction is in flight; once low, 'read_data' holds the result of the last read and 'timed_out' indicates the channel failed to respond."
    )
      { access = ReadOnly
      }
