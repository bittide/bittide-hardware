-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Houses DDR4 controller hardcoded to talk to `MT40A256M16GE-083E`, the memory
chip on the KCU105 development board.
-}
module Clash.Cores.Xilinx.Ddr4 where

import Clash.Prelude

import Control.DeepSeq (NFData, rnf)
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe, isJust)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal

import Protocols.Axi4.Common
import Protocols.Axi4.ReadAddress
import Protocols.Axi4.ReadData
import Protocols.Axi4.WriteAddress
import Protocols.Axi4.WriteData
import Protocols.Axi4.WriteResponse

import Clash.Protocols.Axi4.Extra

import qualified Data.Map as Map

-- | Width of all master and slave ID signals
type C_S_AXI_ID_WIDTH = 4

{- | Width of S_AXI_AWADDR, S_AXI_ARADDR, M_AXI_AWADDR and M_AXI_ARADDR for all
SI/MI slots. Note that these addresses are true byte adresses.
-}
type C_S_AXI_ADDR_WIDTH = 31

-- | Width of WDATA and RDATA on SI slot
type C_S_AXI_DATA_WIDTH = 32

-- | Number of bytes in S_AXI_AWADDR
type C_S_AXI_N_BYTES = DivRU C_S_AXI_DATA_WIDTH 8

{- ORMOLU_DISABLE -}
type ConfAW = 'Axi4WriteAddressConfig 'False 'False C_S_AXI_ID_WIDTH C_S_AXI_ADDR_WIDTH 'False 'False 'True 'True 'True 'False
type ConfW = 'Axi4WriteDataConfig 'True C_S_AXI_N_BYTES
type ConfB = 'Axi4WriteResponseConfig 'True C_S_AXI_ID_WIDTH
type ConfAR = 'Axi4ReadAddressConfig 'False 'False C_S_AXI_ID_WIDTH C_S_AXI_ADDR_WIDTH 'False 'False 'True 'True 'True 'False
type ConfR = 'Axi4ReadDataConfig 'True C_S_AXI_ID_WIDTH
{- ORMOLU_ENABLE -}

-- | Collection of signals which should be routed to the memory directly.
data Ddr4MemorySignals = Ddr4MemorySignals
  { c0_ddr4_adr :: BitVector 17
  , c0_ddr4_ba :: BitVector 2
  , c0_ddr4_bg :: Bit
  , c0_ddr4_cke :: Bit
  , c0_ddr4_odt :: Bit
  , c0_ddr4_cs_n :: Bit
  , c0_ddr4_ck_t :: Bit
  , c0_ddr4_ck_c :: Bit
  , c0_ddr4_act_n :: Bit
  , c0_ddr4_reset_n :: Bit
  }

-- Since these domains are _generated_ by the DDR4 primitive it makes sense to declare
-- them here.
createDomain vXilinxSystem{vName = "Ddr800", vPeriod = 1250}
createDomain vXilinxSystem{vName = "Ddr200", vPeriod = hzToPeriod 200e6}

-- | 'Map', but with a grimy 'NFDataX' instance :sparkles:
newtype MapX k v = MapX {unMapX :: Map k v}

instance (NFDataX k, NFDataX v, NFData k, NFData v) => NFDataX (MapX k v) where
  rnfX = rnf . unMapX
  ensureSpine = id
  hasUndefined = hasUndefined . Map.toList . unMapX
  deepErrorX = errorX

data Ddr4AxiState = Ddr4AxiState
  { memory :: MapX (BitVector C_S_AXI_ADDR_WIDTH) (BitVector C_S_AXI_DATA_WIDTH)
  , fsmState :: Ddr4AxiFsm
  }
  deriving (Generic, NFDataX)

data Ddr4AxiFsm
  = -- | Do absolutely nothing
    Idle
  | -- | Only accept incoming requests on the @read_address@ channel
    WaitForRead
  | -- | Only accept incoming requests on the @write_address@ channel
    WaitForWrite
  | -- | Respond on @read_data@ channel
    Read (BitVector C_S_AXI_ADDR_WIDTH)
  | -- | We got a request on the @write_address@ channel, wait for a request on
    -- the @write_data@ channel.
    WaitWriteData (BitVector C_S_AXI_ADDR_WIDTH)
  | -- | Confirm write request
    ConfirmWrite
  deriving (Generic, NFDataX)

data AxiM2S = AxiM2S
  { wa :: M2S_WriteAddress ConfAW ()
  , wd :: M2S_WriteData ConfW ()
  , wr :: M2S_WriteResponse
  , ra :: M2S_ReadAddress ConfAR ()
  , rd :: M2S_ReadData
  }

data AxiS2M = AxiS2M
  { wa :: S2M_WriteAddress
  , wd :: S2M_WriteData
  , wr :: S2M_WriteResponse ConfB ()
  , ra :: S2M_ReadAddress
  , rd :: S2M_ReadData ConfR () (BitVector C_S_AXI_DATA_WIDTH)
  }

defaultAxiS2M :: AxiS2M
defaultAxiS2M =
  AxiS2M
    { wa = S2M_WriteAddress{_awready = False}
    , wd = S2M_WriteData{_wready = False}
    , wr = S2M_NoWriteResponse
    , ra = S2M_ReadAddress{_arready = False}
    , rd = S2M_NoReadData
    }

ddr4AxiTf ::
  Ddr4AxiState ->
  AxiM2S ->
  ( Ddr4AxiState
  , AxiS2M
  )
ddr4AxiTf s0@Ddr4AxiState{fsmState = Idle} _ =
  ( s0{fsmState = WaitForRead}
  , defaultAxiS2M
  )
ddr4AxiTf s0@Ddr4AxiState{fsmState = WaitForRead} m2s@AxiM2S{ra = M2S_ReadAddress{}} =
  ( s0{fsmState = Read m2s.ra._araddr}
  , defaultAxiS2M{ra = S2M_ReadAddress{_arready = True}}
  )
ddr4AxiTf s0@Ddr4AxiState{fsmState = WaitForWrite} m2s@AxiM2S{wa = M2S_WriteAddress{}} =
  ( s0{fsmState = WaitWriteData m2s.wa._awaddr}
  , defaultAxiS2M{wa = S2M_WriteAddress{_awready = True}}
  )
ddr4AxiTf s0@Ddr4AxiState{fsmState = Read addr} AxiM2S{rd = m2s} =
  ( s0{fsmState = nextState}
  , defaultAxiS2M{rd = rd0}
  )
 where
  nextState
    | m2s._rready = WaitForWrite
    | otherwise = s0.fsmState

  rd0 =
    S2M_ReadData
      { _rid = 0
      , _rdata = fromMaybe 0 $ unMapX s0.memory Map.!? addr
      , _rresp = pure ROkay
      , _rlast = True
      , _ruser = ()
      }
ddr4AxiTf s0@Ddr4AxiState{fsmState = WaitWriteData addr} m2s@AxiM2S{wd = M2S_WriteData{}} =
  ( s0
      { memory = MapX $ Map.adjust (\_ -> dat) addr (unMapX s0.memory)
      , fsmState = ConfirmWrite
      }
  , defaultAxiS2M{wd = S2M_WriteData{_wready = True}}
  )
 where
  dat = pack $ map (fromJust . fromStrobeDataType) m2s.wd._wdata
ddr4AxiTf s0@Ddr4AxiState{fsmState = ConfirmWrite} AxiM2S{wr = M2S_WriteResponse{_bready = True}} =
  ( s0{fsmState = WaitForRead}
  , defaultAxiS2M{wr = S2M_WriteResponse{_bid = 0, _bresp = pure ROkay, _buser = ()}}
  )
ddr4AxiTf s0 _ =
  ( s0{fsmState = flipWaitStates s0.fsmState}
  , defaultAxiS2M
  )

{- | Flip between waiting on read and write requests to prevent starvation on one
of the channels.
-}
flipWaitStates :: Ddr4AxiFsm -> Ddr4AxiFsm
flipWaitStates WaitForRead = WaitForWrite
flipWaitStates WaitForWrite = WaitForRead
flipWaitStates s = s

{- BiSignals don't work in record types, which is why we have to write out all inputs and
outputs instead.
The input clock _must_ be `sys_clock_300`. We cannot constrain this exactly due to the
approximation in `DomainToHz`.
-}
ddr4Axi ::
  forall refDom.
  ( KnownDomain refDom
  , 299_000_000 <= DomainToHz refDom
  , DomainToHz refDom <= 301_000_000
  ) =>
  -- | Must be sysclk_300
  "sys_clock_300" ::: DiffClock refDom ->
  -- | Resets the entire memory design. The reset is asynchronous and must be asserted
  -- for a minimum pulse width of 5 ns. It is internally synchronized to @ui_clk_sync_rst@
  -- (or @c0_ddr4_ui_clk_sync_rst@ in the primitive HDL).
  "sys_rst" ::: Reset refDom ->
  -- Routed from our own designs
  "axi_shim_reset" ::: Reset Ddr200 ->
  "axi4_m2s_write_address" ::: Signal Ddr200 (M2S_WriteAddress ConfAW ()) ->
  "axi4_m2s_write_data" ::: Signal Ddr200 (M2S_WriteData ConfW ()) ->
  "axi4_m2s_write_response" ::: Signal Ddr200 M2S_WriteResponse ->
  "axi4_m2s_read_address" ::: Signal Ddr200 (M2S_ReadAddress ConfAR ()) ->
  "axi4_m2s_read_data" ::: Signal Ddr200 M2S_ReadData ->
  -- Routed directly from the memory
  "c0_ddr4_dm_dbi_n" ::: BiSignalIn 'Floating Ddr800 8 ->
  "c0_ddr4_dq" ::: BiSignalIn 'Floating Ddr800 64 ->
  "c0_ddr4_dqs_t" ::: BiSignalIn 'Floating Ddr800 8 ->
  "c0_ddr4_dqs_c" ::: BiSignalIn 'Floating Ddr800 8 ->
  ( -- Routed to our own designs
    "c0_ddr4_ui_clk" ::: Clock Ddr200
  , -- \^ Output clock from the user interface. Must be a quarter of the frequency of
    -- the clock going out to the external SDRAM.
    "c0_ddr4_ui_clk_sync_rst" ::: Reset Ddr200
  , -- \^ @sys_rst@ is internally synchronized to @ui_clk@
    "c0_init_calib_complete" ::: Signal Ddr200 Bool
  , "axi4_s2m_write_address" ::: Signal Ddr200 S2M_WriteAddress
  , "axi4_s2m_write_data" ::: Signal Ddr200 S2M_WriteData
  , "axi4_s2m_write_response" ::: Signal Ddr200 (S2M_WriteResponse ConfB ())
  , "axi4_s2m_read_address" ::: Signal Ddr200 S2M_ReadAddress
  , "axi4_s2m_read_data"
      ::: Signal Ddr200 (S2M_ReadData ConfR () (BitVector C_S_AXI_DATA_WIDTH))
  , -- Routed directly to the memory
    "ddr4_to_memory" ::: Signal Ddr800 Ddr4MemorySignals
  , "c0_ddr4_dm_dbi_n_o" ::: BiSignalOut 'Floating Ddr800 8
  , "c0_ddr4_dq_o" ::: BiSignalOut 'Floating Ddr800 64
  , "c0_ddr4_dqs_t_o" ::: BiSignalOut 'Floating Ddr800 8
  , "c0_ddr4_dqs_c_o" ::: BiSignalOut 'Floating Ddr800 8
  )
ddr4Axi
  refClkDiff
  sysRst
  axi_rst
  m2s_wa
  m2s_wd
  m2s_wr
  m2s_ra
  m2s_rd
  c0_ddr4_dm_dbi_n
  c0_ddr4_dq
  c0_ddr4_dqs_t
  c0_ddr4_dqs_c
    | clashSimulation = sim
    | otherwise = synth
   where
    sim =
      ( clk200
      , rst200
      , calibrationComplete
      , s2m_wa
      , s2m_wd
      , s2m_wr
      , s2m_ra
      , s2m_rd
      , ddr4_sigs
      , c0_ddr4_dm_dbi_n_o
      , c0_ddr4_dq_o
      , c0_ddr4_dqs_t_o
      , c0_ddr4_dqs_c_o
      )
     where
      clk200 = clockGen
      rst200 = resetGen
      calibrationComplete = pure False

      ddr4_sigs :: Signal Ddr800 Ddr4MemorySignals
      ddr4_sigs = pure (Ddr4MemorySignals 0 0 0 0 0 0 0 0 0 0)

      c0_ddr4_dm_dbi_n_o = writeToBiSignal @(BitVector 8) c0_ddr4_dm_dbi_n (pure Nothing)
      c0_ddr4_dq_o = writeToBiSignal @(BitVector 64) c0_ddr4_dq (pure Nothing)
      c0_ddr4_dqs_t_o = writeToBiSignal @(BitVector 8) c0_ddr4_dqs_t (pure Nothing)
      c0_ddr4_dqs_c_o = writeToBiSignal @(BitVector 8) c0_ddr4_dqs_c (pure Nothing)

      s2m = withClockResetEnable clk200 rst200 enableGen $ mealy ddr4AxiTf s0 m2s
       where
        s0 = Ddr4AxiState{memory = MapX $ Map.empty, fsmState = Idle}
        m2s = AxiM2S <$> m2s_wa <*> m2s_wd <*> m2s_wr <*> m2s_ra <*> m2s_rd

      s2m_wa = (.wa) <$> s2m
      s2m_wd = (.wd) <$> s2m
      s2m_wr = (.wr) <$> s2m
      s2m_ra = (.ra) <$> s2m
      s2m_rd = (.rd) <$> s2m

    synth =
      ( ui_clk
      , ui_clk_sync_rst
      , c0_init_calib_complete
      , s2m_wa
      , s2m_wd
      , s2m_wr
      , s2m_ra
      , s2m_rd
      , ddr4_sigs
      , errorX "No simulation: c0_ddr4_dm_dbi_n_o"
      , errorX "No simulation: c0_ddr4_dq_o"
      , errorX "No simulation: c0_ddr4_dqs_t_o"
      , errorX "No simulation: c0_ddr4_dqs_c_o"
      )
     where
      s2m_wa = S2M_WriteAddress <$> s_axi_awready
      s2m_wd = S2M_WriteData <$> s_axi_wready
      s2m_wr =
        mux
          s_axi_bvalid
          (S2M_WriteResponse <$> s_axi_bid <*> (unpack <$> s_axi_bresp) <*> pure ())
          (pure S2M_NoWriteResponse)
      s2m_ra = S2M_ReadAddress <$> s_axi_arready
      s2m_rd =
        mux
          s_axi_rvalid
          ( S2M_ReadData
              <$> s_axi_rid
              <*> s_axi_rdata
              <*> (unpack <$> s_axi_rresp)
              <*> s_axi_rlast
              <*> pure ()
          )
          (pure S2M_NoReadData)

      ddr4_sigs =
        Ddr4MemorySignals
          <$> c0_ddr4_adr
          <*> c0_ddr4_ba
          <*> c0_ddr4_bg
          <*> c0_ddr4_cke
          <*> c0_ddr4_odt
          <*> c0_ddr4_cs_n
          <*> c0_ddr4_ck_t
          <*> c0_ddr4_ck_c
          <*> c0_ddr4_act_n
          <*> c0_ddr4_reset_n

      ( -- DDR4 signals
        unPort -> c0_ddr4_adr
        , unPort -> c0_ddr4_ba
        , unPort -> c0_ddr4_bg
        , unPort -> c0_ddr4_cke
        , unPort -> c0_ddr4_odt
        , unPort -> c0_ddr4_cs_n
        , unPort -> c0_ddr4_ck_t
        , unPort -> c0_ddr4_ck_c
        , unPort -> c0_ddr4_act_n
        , unPort -> c0_ddr4_reset_n
        , -- General ports
          unPort -> ui_clk
        , unPort -> ui_clk_sync_rst
        , unPort -> c0_init_calib_complete
        , -- AXI ports
          unPort -> s_axi_awready
        , unPort -> s_axi_wready
        , unPort -> s_axi_bid
        , unPort -> s_axi_bresp
        , unPort -> s_axi_bvalid
        , unPort -> s_axi_arready
        , unPort -> s_axi_rid
        , unPort -> s_axi_rdata
        , unPort -> s_axi_rresp
        , unPort -> s_axi_rlast
        , unPort -> s_axi_rvalid
        , -- Debug ports
          unPort -> _dbg_clk
        , unPort -> _dbg_bus
        ) = go
{- ORMOLU_DISABLE -}
    go ::
      ( -- DDR4
        Port "c0_ddr4_adr"     Ddr800 (BitVector 17)
      , Port "c0_ddr4_ba"      Ddr800 (BitVector 2)
      , Port "c0_ddr4_bg"      Ddr800 Bit
      , Port "c0_ddr4_cke"     Ddr800 Bit
      , Port "c0_ddr4_odt"     Ddr800 Bit
      , Port "c0_ddr4_cs_n"    Ddr800 Bit
      , Port "c0_ddr4_ck_t"    Ddr800 Bit
      , Port "c0_ddr4_ck_c"    Ddr800 Bit
      , Port "c0_ddr4_act_n"   Ddr800 Bit
      , Port "c0_ddr4_reset_n" Ddr800 Bit

        -- DDR4 BiSignal parts.
        -- These are currently not used since BiSignalOutPort causes a mismatch
        -- in number of output ports. See:
        -- https://github.com/clash-lang/clash-cores/issues/35
        -- , BiSignalOutPort "c0_ddr4_dm_dbi_n_o" 'Floating Ddr800 8
        -- , BiSignalOutPort "c0_ddr4_dq_o"       'Floating Ddr800 64
        -- , BiSignalOutPort "c0_ddr4_dqs_t_o"    'Floating Ddr800 8
        -- , BiSignalOutPort "c0_ddr4_dqs_c_o"    'Floating Ddr800 8

        -- General ports
      , ClockPort "c0_ddr4_ui_clk"    Ddr200
      , ResetPort "c0_ddr4_ui_clk_sync_rst" ActiveHigh Ddr200
      , -- | Indicates that initialization and calibration are complete (active-high)
        Port "c0_init_calib_complete" Ddr200 Bool

        -- AXI Write Address ports
      , Port "c0_ddr4_s_axi_awready"  Ddr200 Bool
        -- AXI Write Data ports
      , Port "c0_ddr4_s_axi_wready"   Ddr200 Bool
        -- AXI Write Response ports
      , Port "c0_ddr4_s_axi_bid"      Ddr200 (BitVector C_S_AXI_ID_WIDTH)
      , Port "c0_ddr4_s_axi_bresp"    Ddr200 (BitVector 2)
      , Port "c0_ddr4_s_axi_bvalid"   Ddr200 Bool
        -- AXI Read Address ports
      , Port "c0_ddr4_s_axi_arready"  Ddr200 Bool
        -- AXI Read Data ports
      , Port "c0_ddr4_s_axi_rid"      Ddr200 (BitVector C_S_AXI_ID_WIDTH)
      , Port "c0_ddr4_s_axi_rdata"    Ddr200 (BitVector C_S_AXI_DATA_WIDTH)
      , Port "c0_ddr4_s_axi_rresp"    Ddr200 (BitVector 2)
      , Port "c0_ddr4_s_axi_rlast"    Ddr200 Bool
      , Port "c0_ddr4_s_axi_rvalid"   Ddr200 Bool

        -- Debug ports
      , -- | Debug Clock. Do not connect any signals to dbg_clk and keep the port
        -- open during instantiation. Automatically connects to the debug hub logic.
        ClockPort "dbg_clk"           Ddr200
      , -- | Reserved. Do not connect any signals to dbg_bus and keep the port
        -- open during instantiation.
        Port "dbg_bus"                Ddr200 (BitVector 512)
      )
    go =
      instWithXilinxWizard
        (instConfig "ddr4")
        XilinxWizard
          { wiz_name = "ddr4"
          , wiz_vendor = "xilinx.com"
          , wiz_library = "ip"
          , wiz_version = "2.2"
          , wiz_options =
                 ("CONFIG.C0_CLOCK_BOARD_INTERFACE", StrOpt "default_sysclk_300")
              :> ("CONFIG.C0.DDR4_TimePeriod",       IntegerOpt 1250)
              :> ("CONFIG.C0.DDR4_InputClockPeriod", IntegerOpt 3333)
              :> ("CONFIG.C0.DDR4_CLKOUT0_DIVIDE",   IntegerOpt 7)
              :> ("CONFIG.C0.DDR4_MemoryPart",       StrOpt "MT40A256M16GE-083E")
              :> ("CONFIG.C0.DDR4_DataWidth",        IntegerOpt 64)
              :> ("CONFIG.C0.DDR4_DataMask",         StrOpt "DM_NO_DBI")
              :> ("CONFIG.C0.DDR4_Ecc",              BoolOpt False)
              :> ("CONFIG.C0.DDR4_AxiSelection",     BoolOpt True)
              :> ("CONFIG.C0.DDR4_CasLatency",       IntegerOpt 11)
              :> ("CONFIG.C0.DDR4_CasWriteLatency",  IntegerOpt 11)
              :> ("CONFIG.C0.DDR4_AxiDataWidth",     IntegerOpt (natToNum @C_S_AXI_DATA_WIDTH))
              :> ("CONFIG.C0.DDR4_AxiAddressWidth",  IntegerOpt (natToNum @C_S_AXI_ADDR_WIDTH))
              :> ("CONFIG.C0.BANK_GROUP_WIDTH",      IntegerOpt 1)
              :> ("CONFIG.System_Clock",             StrOpt "Differential")
              :> Nil
          }
        (DiffClockPort @"c0_sys_clk" refClkDiff)
        (ResetPort @"sys_rst" @ActiveHigh sysRst)

        -- DDR4 ports
        (BiSignalInPort @"c0_ddr4_dm_dbi_n" c0_ddr4_dm_dbi_n)
        (BiSignalInPort @"c0_ddr4_dq"       c0_ddr4_dq)
        (BiSignalInPort @"c0_ddr4_dqs_t"    c0_ddr4_dqs_t)
        (BiSignalInPort @"c0_ddr4_dqs_c"    c0_ddr4_dqs_c)

        -- AXI Write Address Ports
        (ResetPort @"c0_ddr4_aresetn" @ActiveLow axi_rst)
        (Port @"c0_ddr4_s_axi_awid"    ((._awid) <$> m2s_wa))
        (Port @"c0_ddr4_s_axi_awaddr"  ((._awaddr) <$> m2s_wa))
        (Port @"c0_ddr4_s_axi_awlen"   (pure 0 :: Signal Ddr200 (BitVector 8)))
        (Port @"c0_ddr4_s_axi_awsize"  (pure (pack Bs1) :: Signal Ddr200 (BitVector 3)))
        (Port @"c0_ddr4_s_axi_awburst" (pure (pack BmFixed) :: Signal Ddr200 (BitVector 2)))
        (Port @"c0_ddr4_s_axi_awlock"  (pack . (._awlock) <$> m2s_wa)) -- Not used in the current implementation according to pg150.
        (Port @"c0_ddr4_s_axi_awcache" (pack . (._awcache) <$> m2s_wa)) -- Not used in the current implementation according to pg150.
        (Port @"c0_ddr4_s_axi_awprot"  (pack . (._awprot) <$> m2s_wa)) -- Not used in the current implementation according to pg150.
        (Port @"c0_ddr4_s_axi_awqos"   (pure 0 :: Signal Ddr200 (BitVector 4))) -- Not used in the current implementation according to pg150.
        (Port @"c0_ddr4_s_axi_awvalid" (isWriteAddress <$> m2s_wa))

        -- AXI Write Data Ports
        (Port @"c0_ddr4_s_axi_wdata"   (pack . map (fromJust . fromStrobeDataType) . (._wdata) <$> m2s_wd))
        (Port @"c0_ddr4_s_axi_wstrb"   (pack . map (isJust . fromStrobeDataType) . (._wdata) <$> m2s_wd))
        (Port @"c0_ddr4_s_axi_wlast"   ((._wlast) <$> m2s_wd))
        (Port @"c0_ddr4_s_axi_wvalid"  (isWriteData <$> m2s_wd))

        -- AXI Write Response Ports
        (Port @"c0_ddr4_s_axi_bready"  ((._bready) <$> m2s_wr))

        -- AXI Read Address Ports
        (Port @"c0_ddr4_s_axi_arid"    ((._arid) <$> m2s_ra))
        (Port @"c0_ddr4_s_axi_araddr"  ((._araddr) <$> m2s_ra))
        (Port @"c0_ddr4_s_axi_arlen"   (pure 0 :: Signal Ddr200 (BitVector 8)))
        (Port @"c0_ddr4_s_axi_arsize"  (pure (pack Bs1) :: Signal Ddr200 (BitVector 3)))
        (Port @"c0_ddr4_s_axi_arburst" (pure (pack BmFixed) :: Signal Ddr200 (BitVector 2)))
        (Port @"c0_ddr4_s_axi_arlock"  (pack . (._arlock) <$> m2s_ra)) -- Not used in the current implementation according to pg150.
        (Port @"c0_ddr4_s_axi_arcache" (pack . (._arcache) <$> m2s_ra)) -- Not used in the current implementation according to pg150.
        (Port @"c0_ddr4_s_axi_arprot"  (pack . (._arprot) <$> m2s_ra)) -- Not used in the current implementation according to pg150.
        (Port @"c0_ddr4_s_axi_arqos"   (pure 0 :: Signal Ddr200 (BitVector 4))) -- Not used in the current implementation according to pg150.
        (Port @"c0_ddr4_s_axi_arvalid" (isReadAddress <$> m2s_ra))

        -- AXI Read Data Ports
        (Port @"c0_ddr4_s_axi_rready"  ((._rready) <$> m2s_rd))
{- ORMOLU_ENABLE -}
