-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Test DDR4 controller by writing values to a specific \"chunk\" of memory. After
writing, read from the same addresses and confirm that the values are as expected.

This test is currently not pure as previous writes to the memory are not
cleared before running the test. Reprogramming the FPGA also clears the data in
the memory.
-}
module Bittide.Instances.Hitl.Ddr4 where

import Clash.Explicit.Prelude

import Data.Data (Proxy (..))
import Data.Functor.Identity (Identity (Identity))
import Data.Maybe (fromJust, isJust)

import Clash.Class.Counter (Counter (countSuccOverflow))
import Clash.Cores.Xilinx.Ila (Depth (..), IlaConfig (depth), ila, ilaConfig)
import Protocols
import Protocols.Axi4.Common
import Protocols.Axi4.ReadAddress
import Protocols.Axi4.ReadData
import Protocols.Axi4.WriteAddress
import Protocols.Axi4.WriteData
import Protocols.Axi4.WriteResponse

import Bittide.ElasticBuffer (sticky)
import Bittide.Hitl (
  HitlTestGroup (..),
  HwTargetRef (HwTargetByIndex),
  hitlVio,
  testCasesFromEnum,
 )
import Clash.Cores.Xilinx.Ddr4
import Clash.Protocols.Axi4.Extra

import Bittide.Instances.Domains

data TestState = Busy | Fail | Success

type Done dom = CSignal dom Bool

-- | The number of addresses per chunk
type AddressesPerChunk = 512 :: Nat

{- | The steps of addresses. Should be divisible by 4 to account for byte addressing
and 32-bit AXI bus width.
-}
type StepSize = 1024 :: Nat

data TestChunk
  = Bottom
  | Top
  deriving (Enum, Generic, NFDataX, Bounded, BitPack, ShowX, Show)

{- | Generate an address based on the current test chunk. The goal is to check
the bottom range of the memory and the top range.
-}
mkAddress ::
  forall n nAddresses stepSize.
  ( KnownNat n
  , KnownNat nAddresses
  , 1 <= nAddresses
  , nAddresses <= 2 ^ n
  ) =>
  SNat stepSize ->
  TestChunk ->
  Index nAddresses ->
  BitVector n
mkAddress SNat Bottom i = numConvert i * natToNum @stepSize
mkAddress SNat Top i = maxBound - (numConvert i * natToNum @stepSize)

writeAddressFsm ::
  forall nAddresses stepSize dom.
  ( KnownDomain dom
  , 1 <= nAddresses
  , nAddresses <= 2 ^ AWAddrWidth ConfAW
  ) =>
  SNat nAddresses ->
  SNat stepSize ->
  Clock dom ->
  Reset dom ->
  Signal dom TestChunk ->
  Circuit () (Axi4WriteAddress dom ConfAW ())
writeAddressFsm SNat SNat clk rst chunk = Circuit go
 where
  -- See `mkAddress` for which addresses are generated
  go ::
    ((), (Signal dom S2M_WriteAddress)) ->
    ((), Signal dom (M2S_WriteAddress ConfAW ()))
  go (_, s2m) = ((), m2s)
   where
    busy = fmap not done .&&. unsafeToActiveLow rst
    continue = fmap (._awready) s2m .&&. fmap not done
    i0 = register clk rst (toEnable continue) (0 :: Index nAddresses) i1
    (done, i1) = unbundle $ countSuccOverflow <$> i0
    m2s = mux busy (mkWriteAddress <$> addr) (pure M2S_NoWriteAddress)

    addr = mkAddress (SNat @stepSize) <$> chunk <*> i0

    mkWriteAddress :: BitVector (AWAddrWidth ConfAW) -> M2S_WriteAddress ConfAW ()
    mkWriteAddress a =
      M2S_WriteAddress
        { _awid = 0
        , _awaddr = a
        , _awregion = Proxy
        , _awlen = Proxy
        , _awsize = Proxy
        , _awburst = Proxy
        , _awlock = pure NonExclusiveAccess
        , _awcache = pure (NonBufferable, NonModifiable, OtherNoLookupCache, NoLookupCache)
        , _awprot = pure (NotPrivileged, NonSecure, Data)
        , _awqos = Proxy
        , _awuser = ()
        }

writeDataFsm ::
  forall nAddresses dom.
  (KnownDomain dom, 1 <= nAddresses) =>
  SNat nAddresses ->
  Clock dom ->
  Reset dom ->
  Circuit () (Axi4WriteData dom ConfW ())
writeDataFsm SNat clk rst = Circuit go
 where
  -- Generate 0, 1, 2, ...
  go ::
    ((), (Signal dom S2M_WriteData)) ->
    ((), (Signal dom (M2S_WriteData ConfW ())))
  go (_, s2m) = ((), m2s)
   where
    busy = fmap not done .&&. unsafeToActiveLow rst
    continue = fmap (._wready) s2m .&&. fmap not done
    i0 = register clk rst (toEnable continue) (0 :: Index nAddresses) i1
    (done, i1) = unbundle $ countSuccOverflow <$> i0
    m2s = mux busy (mkWriteData <$> i0) (pure M2S_NoWriteData)

    idxToVec :: Index nAddresses -> Vec 4 (BitVector 8)
    idxToVec = unpack . (resize @_ @_ @(4 * 8)) . pack

    mkWriteData :: Index nAddresses -> M2S_WriteData ConfW ()
    mkWriteData d =
      M2S_WriteData
        { _wdata = map (toStrobeDataType True) (idxToVec d)
        , _wlast = True
        , _wuser = ()
        }

writeResponseFsm ::
  forall nAddresses dom.
  (KnownDomain dom, 1 <= nAddresses) =>
  SNat nAddresses ->
  Clock dom ->
  Reset dom ->
  Circuit (Axi4WriteResponse dom ConfB ()) (Done dom)
writeResponseFsm SNat clk rst = Circuit go
 where
  -- ACK continuously
  go ::
    (Signal dom (S2M_WriteResponse ConfB ()), ()) ->
    (Signal dom M2S_WriteResponse, Signal dom Bool)
  go (s2m, _) = (m2s, done)
   where
    busy = fmap not done .&&. unsafeToActiveLow rst
    continue = fmap isWriteResponse s2m .&&. fmap not done
    i0 = register clk rst (toEnable continue) (0 :: Index nAddresses) i1
    (done, i1) = unbundle $ countSuccOverflow <$> i0
    m2s = M2S_WriteResponse <$> (busy .&&. continue)

readAddressFsm ::
  forall nAddresses stepSize dom.
  ( KnownDomain dom
  , 1 <= nAddresses
  , nAddresses <= 2 ^ ARAddrWidth ConfAR
  ) =>
  SNat nAddresses ->
  SNat stepSize ->
  Clock dom ->
  Reset dom ->
  Signal dom TestChunk ->
  Circuit (Done dom) (Axi4ReadAddress dom ConfAR ())
readAddressFsm SNat SNat clk rst chunk = Circuit go
 where
  -- See `mkAddress` for which addresses are generated
  go ::
    (Signal dom Bool, Signal dom S2M_ReadAddress) ->
    ((), Signal dom (M2S_ReadAddress ConfAR ()))
  go (start, s2m) = ((), m2s)
   where
    busy = fmap not done .&&. unsafeToActiveLow rst .&&. start
    continue = fmap (._arready) s2m .&&. busy
    i0 = register clk rst (toEnable continue) (0 :: Index nAddresses) i1
    (done, i1) = unbundle $ countSuccOverflow <$> i0
    m2s = mux busy (mkReadAddress <$> addr) (pure M2S_NoReadAddress)

    addr = mkAddress (SNat @stepSize) <$> chunk <*> i0

    mkReadAddress :: BitVector (ARAddrWidth ConfAR) -> M2S_ReadAddress ConfAR ()
    mkReadAddress a =
      M2S_ReadAddress
        { _arid = 0
        , _araddr = a
        , _arregion = Proxy
        , _arlen = Proxy
        , _arsize = Proxy
        , _arburst = Proxy
        , _arlock = pure NonExclusiveAccess
        , _arcache = pure (NonBufferable, NonModifiable, NoLookupCache, OtherNoLookupCache)
        , _arprot = pure (NotPrivileged, NonSecure, Data)
        , _arqos = Proxy
        , _aruser = ()
        }

readDataFsm ::
  forall nAddresses dom.
  (KnownDomain dom, 1 <= nAddresses) =>
  SNat nAddresses ->
  Clock dom ->
  Reset dom ->
  Circuit
    (Axi4ReadData dom ConfR () (BitVector C_S_AXI_DATA_WIDTH))
    ( CSignal dom TestState
    , CSignal dom DebugInfo
    )
readDataFsm SNat clk rst = Circuit go
 where
  -- Check 0, 1, 2, ...
  go ::
    ( Signal dom (S2M_ReadData ConfR () (BitVector C_S_AXI_DATA_WIDTH))
    , ((), ())
    ) ->
    ( Signal dom M2S_ReadData
    , (Signal dom TestState, Signal dom DebugInfo)
    )
  go (s2m, (_, _)) = (m2s, (testState, debugInfo))
   where
    busy = fmap not done .&&. unsafeToActiveLow rst
    continue = fmap isReadData s2m .&&. fmap not done
    i0 = register clk rst (toEnable continue) (0 :: Index nAddresses) i1
    (done, i1) = unbundle $ countSuccOverflow <$> i0
    m2s = M2S_ReadData <$> busy

    testState = toTestState <$> busy <*> failed
    toTestState b f =
      case (b, f) of
        (False, True) -> Fail
        (False, False) -> Success
        (True, _) -> Busy

    failed = sticky clk rst $ wrong .||. fmap isError s2m
    wrong = fmap isError s2m .||. fmap not (dataOkay <$> s2m <*> i0)

    dataOkay S2M_NoReadData _ = True
    dataOkay S2M_ReadData{_rdata} expected = _rdata == resize (pack expected)

    isError :: S2M_ReadData ConfR () (BitVector C_S_AXI_DATA_WIDTH) -> Bool
    isError S2M_ReadData{_rresp = Identity RSlaveError} = True
    isError S2M_ReadData{_rresp = Identity RDecodeError} = True
    isError _ = False

    getData S2M_ReadData{_rdata} = _rdata
    getData _ = maxBound

    debugInfo =
      DebugInfo
        <$> (dataOkay <$> s2m <*> i0)
        <*> fmap isError s2m
        <*> fmap isReadData s2m
        <*> fmap getData s2m
        <*> (resize . pack <$> i0)

data DebugInfo = DebugInfo
  { dataOkay :: Bool
  , isError :: Bool
  , hasData :: Bool
  , readData :: BitVector C_S_AXI_DATA_WIDTH
  , expectedData :: BitVector C_S_AXI_DATA_WIDTH
  }

-- TODO: Change dut so we check in chunks. That makes it possible to check intermediate
-- states when using large `nAddresses`.
dut ::
  ( KnownDomain dom
  , 1 <= nAddresses
  , nAddresses <= 2 ^ AWAddrWidth ConfAW
  , AWAddrWidth ConfAW ~ ARAddrWidth ConfAR
  ) =>
  SNat nAddresses ->
  SNat stepSize ->
  Clock dom ->
  Reset dom ->
  Signal dom TestChunk ->
  Circuit
    ( Axi4WriteResponse dom ConfB ()
    , Axi4ReadData dom ConfR () (BitVector C_S_AXI_DATA_WIDTH)
    )
    ( Axi4WriteAddress dom ConfAW ()
    , Axi4WriteData dom ConfW ()
    , Axi4ReadAddress dom ConfAR ()
    , CSignal dom TestState
    , CSignal dom DebugInfo
    )
dut nAddresses stepSize clk rst chunk = circuit $ \(wr, rd) -> do
  wa <- writeAddressFsm nAddresses stepSize clk rst chunk
  wd <- writeDataFsm nAddresses clk rst
  Fwd wrDone <- writeResponseFsm nAddresses clk rst -< wr
  ra <- readAddressFsm nAddresses stepSize clk rst chunk -< Fwd wrDone
  (testState, debugInfo) <- readDataFsm nAddresses clk rst -< rd
  idC -< (wa, wd, ra, testState, debugInfo)

{- | The DDR4 wizard generates contraints for the reference clock, so it _must_
be named @c0_sys_clk@. We also do not need a pin constraint for it as it will
be mapped to @sysclk_300_p@ (and @_n@).
The synthesis annotation below is needed because the BiSignals are not rendered
properly with 'makeTopentity'.
-}
ddr4Test ::
  "c0_sys_clk" ::: DiffClock Ext300 ->
  "c0_ddr4_dm_dbi_n" ::: BiSignalIn 'Floating Ddr800 8 ->
  "c0_ddr4_dq" ::: BiSignalIn 'Floating Ddr800 64 ->
  "c0_ddr4_dqs_t" ::: BiSignalIn 'Floating Ddr800 8 ->
  "c0_ddr4_dqs_c" ::: BiSignalIn 'Floating Ddr800 8 ->
  ""
    ::: ( "c0_ddr4_adr" ::: Signal Ddr800 (BitVector 17)
        , "c0_ddr4_ba" ::: Signal Ddr800 (BitVector 2)
        , "c0_ddr4_bg" ::: Signal Ddr800 Bit
        , "c0_ddr4_cke" ::: Signal Ddr800 Bit
        , "c0_ddr4_odt" ::: Signal Ddr800 Bit
        , "c0_ddr4_cs_n" ::: Signal Ddr800 Bit
        , "c0_ddr4_ck_t" ::: Signal Ddr800 Bit
        , "c0_ddr4_ck_c" ::: Signal Ddr800 Bit
        , "c0_ddr4_act_n" ::: Signal Ddr800 Bit
        , "c0_ddr4_reset_n" ::: Signal Ddr800 Bit
        , "c0_ddr4_dm_dbi_n_o" ::: BiSignalOut 'Floating Ddr800 8
        , "c0_ddr4_dq_o" ::: BiSignalOut 'Floating Ddr800 64
        , "c0_ddr4_dqs_t_o" ::: BiSignalOut 'Floating Ddr800 8
        , "c0_ddr4_dqs_c_o" ::: BiSignalOut 'Floating Ddr800 8
        )
ddr4Test refClkDiff c0_ddr4_dm_dbi_n c0_ddr4_dq c0_ddr4_dqs_t c0_ddr4_dqs_c =
  hwSeqX debugIla
    $ ( (.c0_ddr4_adr) <$> ddr4Signals
      , (.c0_ddr4_ba) <$> ddr4Signals
      , (.c0_ddr4_bg) <$> ddr4Signals
      , (.c0_ddr4_cke) <$> ddr4Signals
      , (.c0_ddr4_odt) <$> ddr4Signals
      , (.c0_ddr4_cs_n) <$> ddr4Signals
      , (.c0_ddr4_ck_t) <$> ddr4Signals
      , (.c0_ddr4_ck_c) <$> ddr4Signals
      , (.c0_ddr4_act_n) <$> ddr4Signals
      , (.c0_ddr4_reset_n) <$> ddr4Signals
      , c0_ddr4_dm_dbi_n_o
      , c0_ddr4_dq_o
      , c0_ddr4_dqs_t_o
      , c0_ddr4_dqs_c_o
      )
 where
  testStart = isJust <$> testInput
  testStartRst = unsafeFromActiveLow testStart

  testInput :: Signal Ddr200 (Maybe TestChunk)
  testInput = hitlVio Bottom clkUi testDone testSuccess

  testDone = sticky clkUi testStartRst $ testStart .&&. fmap isDone testResult
  testSuccess = sticky clkUi testStartRst $ testStart .&&. fmap isSuccess testResult

  isDone :: TestState -> Bool
  isDone Busy = False
  isDone _ = True

  isSuccess :: TestState -> Bool
  isSuccess Success = True
  isSuccess _ = False

  testCycleCounter :: Signal Ddr200 (Unsigned 32)
  testCycleCounter = c
   where
    c = register clkUi rst (toEnable (fmap (not . isDone) testResult)) 0 (c + 1)
    rst = rstUi `orReset` unsafeFromActiveLow testStart

  debugIla :: Signal Ddr200 ()
  debugIla =
    setName @"ddr4DebugIla"
      $ ila
        ( ilaConfig
            $ "trigger"
            :> "capture"
            :> "rstUi"
            :> "testStart"
            :> "testDone"
            :> "testSuccess"
            :> "calibrationComplete"
            :> "testCycleCounter"
            -- AXI signals
            :> "m2s_wa_isWriteAddress"
            :> "m2s_wa_awaddr"
            :> "s2m_wa_awready"
            :> "m2s_wd_isWriteData"
            :> "m2s_wd_wdata"
            :> "s2m_wd_wready"
            :> "s2m_wr_isWriteResponse"
            :> "s2m_wr_bresp"
            :> "m2s_wr_bready"
            :> "m2s_ra_isReadAddress"
            :> "m2s_ra_araddr"
            :> "s2m_ra_arready"
            :> "s2m_rd_isReadData"
            :> "s2m_rd_rdata"
            :> "s2m_rd_rresp"
            :> "m2s_rd_rready"
            -- DebugInfo from @ReadData@ channel
            :> "dataOkay"
            :> "isError"
            :> "hasData"
            :> "readData"
            :> "expectedData"
            :> Nil
        )
          { depth = D8192
          }
        clkUi
        testStart
        (pure True :: Signal Ddr200 Bool)
        -- Debug probes
        (unsafeToActiveHigh rstUi)
        testStart
        (isDone <$> testResult)
        (isSuccess <$> testResult)
        calibrationComplete
        testCycleCounter
        -- AXI signals
        (isWriteAddress <$> m2s_wa)
        ((._awaddr) <$> m2s_wa)
        ((._awready) <$> s2m_wa)
        (isWriteData <$> m2s_wd)
        ((map (fromJust . fromStrobeDataType) . (._wdata)) <$> m2s_wd)
        ((._wready) <$> s2m_wd)
        (isWriteResponse <$> s2m_wr)
        ((._bresp) <$> s2m_wr)
        ((._bready) <$> m2s_wr)
        (isReadAddress <$> m2s_ra)
        ((._araddr) <$> m2s_ra)
        ((._arready) <$> s2m_ra)
        (isReadData <$> s2m_rd)
        ((._rdata) <$> s2m_rd)
        ((._rresp) <$> s2m_rd)
        ((._rready) <$> m2s_rd)
        -- @ReadData@ verifier debug info
        ((.dataOkay) <$> debugInfo)
        ((.isError) <$> debugInfo)
        ((.hasData) <$> debugInfo)
        ((.readData) <$> debugInfo)
        ((.expectedData) <$> debugInfo)

  dutRst = rstUi `orReset` testStartRst `orReset` (unsafeFromActiveLow calibrationComplete)

  ( (m2s_wr, m2s_rd)
    , (m2s_wa, m2s_wd, m2s_ra, testResult, debugInfo)
    ) =
      ( toSignals
          $ dut (SNat @AddressesPerChunk) (SNat @StepSize) clkUi dutRst (fromJustX <$> testInput)
      )
        (
          ( s2m_wr
          , s2m_rd
          )
        ,
          ( s2m_wa
          , s2m_wd
          , s2m_ra
          , ()
          , ()
          )
        )

  axi_shim_reset = noReset
  -- Ideally `sysRst` depend on `testStart`, but we can't synchronize a reset to
  -- @refDom@ of `ddr4Axi` because we do not have access to a clock in @refDom@,
  -- even though it is asynchronous.
  sysRst = noReset

  ( clkUi
    , rstUi
    , calibrationComplete
    , s2m_wa
    , s2m_wd
    , s2m_wr
    , s2m_ra
    , s2m_rd
    , ddr4Signals
    , c0_ddr4_dm_dbi_n_o
    , c0_ddr4_dq_o
    , c0_ddr4_dqs_t_o
    , c0_ddr4_dqs_c_o
    ) =
      ddr4Axi
        refClkDiff
        sysRst
        axi_shim_reset
        m2s_wa
        m2s_wd
        m2s_wr
        m2s_ra
        m2s_rd
        c0_ddr4_dm_dbi_n
        c0_ddr4_dq
        c0_ddr4_dqs_t
        c0_ddr4_dqs_c
{-# OPAQUE ddr4Test #-}
{-# ANN
  ddr4Test
  ( Synthesize
      { t_name = "ddr4Test"
      , t_inputs =
          [ PortProduct
              "c0_sys_clk"
              [PortName "p", PortName "n"]
          , PortName "c0_ddr4_dm_dbi_n"
          , PortName "c0_ddr4_dq"
          , PortName "c0_ddr4_dqs_t"
          , PortName "c0_ddr4_dqs_c"
          ]
      , t_output =
          PortProduct
            ""
            [ PortName "c0_ddr4_adr"
            , PortName "c0_ddr4_ba"
            , PortName "c0_ddr4_bg"
            , PortName "c0_ddr4_cke"
            , PortName "c0_ddr4_odt"
            , PortName "c0_ddr4_cs_n"
            , PortName "c0_ddr4_ck_t"
            , PortName "c0_ddr4_ck_c"
            , PortName "c0_ddr4_act_n"
            , PortName "c0_ddr4_reset_n"
            , PortName "c0_ddr4_dm_dbi_n_o"
            , PortName "c0_ddr4_dq_o"
            , PortName "c0_ddr4_dqs_t_o"
            , PortName "c0_ddr4_dqs_c_o"
            ]
      }
  )
  #-}

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'ddr4Test
    , targetXdcs = ["ddr4.xdc"]
    , externalHdl = []
    , testCases = testCasesFromEnum @TestChunk [HwTargetByIndex 0] ()
    , mDriverProc = Nothing
    , mPostProc = Nothing
    }
