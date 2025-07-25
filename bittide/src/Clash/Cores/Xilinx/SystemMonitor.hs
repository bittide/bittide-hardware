-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Clash.Cores.Xilinx.SystemMonitor (
  Status (..),
  temperatureMonitorCelcius,
  temperatureMonitor,
  toCelcius,
) where

import Clash.Prelude

import Clash.Cores.Xilinx.Xpm.Cdc.Internal
import Clash.Functor.Extra ((<<$>>))

{- | A wrapper for a SYSMONE1 instance which only monitors the FPGA temperature. Converts
the measured value of the temperature sensor to the temperature in degree Celcius.

XXX: This function can lead to timing issues because the instantiated DSP does
     not include input/output registers.
-}
temperatureMonitorCelcius ::
  forall dom.
  (HiddenClockResetEnable dom, 1 <= DomainPeriod dom) =>
  Signal dom (Status, Maybe (Signed 11))
temperatureMonitorCelcius = bundle (status, temperature)
 where
  (status, measurement) = unbundle temperatureMonitor
  temperature = toSigned <<$>> dflipflop (toCelcius . unpack <<$>> dflipflop measurement)
   where
    toSigned ::
      forall int frac. (KnownNat int, KnownNat frac) => SFixed int frac -> Signed int
    toSigned = (resize . bitCoerce) . flip shiftR (natToNum @frac)

{- | A wrapper for a SYSMONE1 instance which only monitors the FPGA temperature and
outputs the raw measurement.
-}
temperatureMonitor ::
  forall dom.
  (HiddenClockResetEnable dom, 1 <= DomainPeriod dom) =>
  Signal dom (Status, Maybe (BitVector 10))
temperatureMonitor = bundle (status, tOut)
 where
  tOut = mux dRdy (Just <$> measurement) (pure Nothing)

  -- The 10-bit measurement is stored in the MSBs.
  measurement = resize . flip shiftR 6 <$> dO
  (status, dRdy, dO) = unbundle $ sysMon @dom hasClock hasReset dEn dAddr dWe dI

  dEn = counter .==. pure 0
  dWe = pure False
  dAddr = pure 0x00
  dI = pure 0x00

  counter = register (0 :: Index 10) $ satSucc SatWrap <$> counter

{- | Convert the measurement to temperature in degrees Celcius. Assumes the temperature
sensor uses the on-chip reference. For more information see:

    https://docs.amd.com/v/u/en-US/ug580-ultrascale-sysmon
-}
toCelcius ::
  forall adcDepth. (KnownNat adcDepth) => Unsigned adcDepth -> SFixed (adcDepth + 1) 14
toCelcius measurement = t
 where
  t = measurementSF * (a `shiftR` (natToNum @adcDepth)) + b
  measurementSF :: SFixed (adcDepth + 1) 14
  measurementSF = bitCoerce (resize measurement `shiftL` 14)
  a = 501.3743
  b = -273.6777

data Status = Status
  { busy :: Bool
  , channel :: BitVector 6
  , endOfConversion :: Bool
  , endOfSequence :: Bool
  }
  deriving (Eq, Generic, NFDataX)

{- The internal ADC clock is derived from the DRP clock. The SYSMON supports a DRP clock
frequency up to 250 MHz, whereas the The maximum frequency of the ADC clock is 5.2 MHz.
A clock divider with a minimal value of 2 is used to get the appropriate ADC clock
frequency.
-}
type AdcClockDiv dom = Max 2 (HzToPeriod 5_200_000 `DivRU` DomainPeriod dom)

{- | A System Monitor (SYSMONE1) instance with an exposed Dynamic Reconfiguration Port
(DRP) interface. The SYSMON supports a DRP clock frequency up to 250 MHz. The clock
division ratio between the DRP clock (dClk) and and the lower frequency ADC clock is set
such that the ADC clock frequency is at most 5.2 MHz (the maximum ADCCCLK frequency).
For more information see:

    https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/SYSMONE1

TODO: Make the instance configurable through its parameters.
-}
sysMon ::
  forall dom.
  (KnownDomain dom, 1 <= DomainPeriod dom) =>
  -- | Clock input for the dynamic reconfiguration port.
  Clock dom ->
  -- | Reset signal for the SYSMON control logic.
  Reset dom ->
  -- | Enable signal for the dynamic reconfiguration port.
  Signal dom Bool ->
  -- | DRP address bus
  Signal dom (BitVector 8) ->
  -- | DRP write enable
  Signal dom Bool ->
  -- | DRP input data bus
  Signal dom (BitVector 16) ->
  -- | (SYSMON status ports, DRP data ready, DRP data)
  Signal dom (Status, Bool, BitVector 16)
sysMon dClk rst dEn dAddr dWe dI
  | natToInteger @(DomainToHz dom) > 250_000_000 =
      clashCompileError "DRP clock frequency cannot be higher than 250 MHz"
  | clashSimulation = sim
  | otherwise = synth
 where
  -- Definition used in Clash simulation
  sim = pure (status, False, 0)
   where
    status =
      Status
        { busy = False
        , channel = 0
        , endOfConversion = False
        , endOfSequence = False
        }

  -- Definition used for HDL generation
  clkDiv :: BitVector 8
  clkDiv = natToNum @(AdcClockDiv dom)
  synth = bundle (status, dRdy, dO)
   where
    status = Status <$> busy <*> channel <*> eoc <*> eos
    ( _
      , _
      , unPort -> dO
      , unPort -> dRdy
      , _
      , _
      , unPort -> busy
      , unPort -> channel
      , unPort -> eoc
      , unPort -> eos
      , _
      , _
      , _
      , _
      ) = go

    go ::
      ( Port "ALM" dom (BitVector 16)
      , Port "OT" dom Bool
      , Port "DO" dom (BitVector 16)
      , Port "DRDY" dom Bool
      , Port "I2C_SCLK_TS" dom Bit
      , Port "I2C_SDA_TS" dom Bit
      , Port "BUSY" dom Bool
      , Port "CHANNEL" dom (BitVector 6)
      , Port "EOC" dom Bool
      , Port "EOS" dom Bool
      , Port "JTAGBUSY" dom Bool
      , Port "JTAGLOCKED" dom Bool
      , Port "JTAGMODIFIED" dom Bool
      , Port "MUXADDR" dom (BitVector 5)
      )
    go =
      inst
        (instConfig "SYSMONE1")
          { library = Just "UNISIM"
          , libraryImport = Just "UNISIM.vcomponents.all"
          }
{- ORMOLU_DISABLE -}
        -- Analog Bus Register
        (Param @"INIT_45" @(BitVector 16) 0)
        -- INIT_40 - INIT_44: SYSMON configuration registers
        (Param @"INIT_40" @(BitVector 16) 0x9000) -- 16 sample average filter, disable averaging of calibration coefficients
        (Param @"INIT_41" @(BitVector 16) 0x2ED0) -- Continuous seq mode, disable ALM[6:4], enable calibration
        (Param @"INIT_42" @(BitVector 16) (clkDiv ++# 0))
        (Param @"INIT_43" @(BitVector 16) 0x000F) -- Disable ALM[11:8]
        (Param @"INIT_44" @(BitVector 16) 0)
        -- INIT_46 - INIT_4F: Sequence Registers
        (Param @"INIT_46" @(BitVector 16) 0)
        (Param @"INIT_47" @(BitVector 16) 0)
        (Param @"INIT_48" @(BitVector 16) 0x4701) -- Enable Temp, VCCINT, VCCAUX, VCCBRAM and calibration
        (Param @"INIT_49" @(BitVector 16) 0)
        (Param @"INIT_4A" @(BitVector 16) 0)
        (Param @"INIT_4B" @(BitVector 16) 0)
        (Param @"INIT_4C" @(BitVector 16) 0)
        (Param @"INIT_4D" @(BitVector 16) 0)
        (Param @"INIT_4E" @(BitVector 16) 0)
        (Param @"INIT_4F" @(BitVector 16) 0)
        -- INIT_50 - INIT_5F: Alarm Limit Registers
        (Param @"INIT_50" @(BitVector 16) 0)
        (Param @"INIT_51" @(BitVector 16) 0)
        (Param @"INIT_52" @(BitVector 16) 0)
        (Param @"INIT_53" @(BitVector 16) 0)
        (Param @"INIT_54" @(BitVector 16) 0)
        (Param @"INIT_55" @(BitVector 16) 0)
        (Param @"INIT_56" @(BitVector 16) 0)
        (Param @"INIT_57" @(BitVector 16) 0)
        (Param @"INIT_58" @(BitVector 16) 0)
        (Param @"INIT_59" @(BitVector 16) 0)
        (Param @"INIT_5A" @(BitVector 16) 0)
        (Param @"INIT_5B" @(BitVector 16) 0)
        (Param @"INIT_5C" @(BitVector 16) 0)
        (Param @"INIT_5D" @(BitVector 16) 0)
        (Param @"INIT_5E" @(BitVector 16) 0)
        (Param @"INIT_5F" @(BitVector 16) 0)
        -- INIT_60 - INIT_6F: User Supply Alarms
        (Param @"INIT_60" @(BitVector 16) 0)
        (Param @"INIT_61" @(BitVector 16) 0)
        (Param @"INIT_62" @(BitVector 16) 0)
        (Param @"INIT_63" @(BitVector 16) 0)
        (Param @"INIT_64" @(BitVector 16) 0)
        (Param @"INIT_65" @(BitVector 16) 0)
        (Param @"INIT_66" @(BitVector 16) 0)
        (Param @"INIT_67" @(BitVector 16) 0)
        (Param @"INIT_68" @(BitVector 16) 0)
        (Param @"INIT_69" @(BitVector 16) 0)
        (Param @"INIT_6A" @(BitVector 16) 0)
        (Param @"INIT_6B" @(BitVector 16) 0)
        (Param @"INIT_6C" @(BitVector 16) 0)
        (Param @"INIT_6D" @(BitVector 16) 0)
        (Param @"INIT_6E" @(BitVector 16) 0)
        (Param @"INIT_6F" @(BitVector 16) 0)
        -- Programmable Inversion Attributes: Specifies the use of the built-in
        -- programmable inversion on specific pins.
        (Param @"IS_CONVSTCLK_INVERTED" @Bit 0)
        (Param @"IS_DCLK_INVERTED"      @Bit 0)
        -- Analog simulation data file name
        (Param @"SIM_MONITOR_FILE"      @String "design.txt")
        -- SYSMON User voltage monitor
        (Param @"SYSMON_VUSER0_BANK"    @Integer 0)
        (Param @"SYSMON_VUSER0_MONITOR" @String "NONE")
        (Param @"SYSMON_VUSER1_BANK"    @Integer 0)
        (Param @"SYSMON_VUSER1_MONITOR" @String "NONE")
        (Param @"SYSMON_VUSER2_BANK"    @Integer 0)
        (Param @"SYSMON_VUSER2_MONITOR" @String "NONE")
        (Param @"SYSMON_VUSER3_BANK"    @Integer 0)
        (Param @"SYSMON_VUSER3_MONITOR" @String "NONE")
        -- Auxiliary Analog-Input Pairs
        (Port      @"VAUXN"             (pure 0 :: Signal dom (BitVector 16)))
        (Port      @"VAUXP"             (pure 0 :: Signal dom (BitVector 16)))
        -- SYSMON reset, conversion start and clock inputs
        (ResetPort @"RESET" @ActiveHigh rst)
        (Port      @"CONVSTCLK"         (pure 0 :: Signal dom Bit))
        (Port      @"CONVST"            (pure 0 :: Signal dom Bit))
        -- Dedicated Analog Input Pair
        (Port      @"VN"                (pure 0 :: Signal dom Bit))
        (Port      @"VP"                (pure 0 :: Signal dom Bit))
        -- Dynamic Reconfiguration Port (DRP)
        (ClockPort @"DCLK"              dClk)
        (Port      @"DADDR"             dAddr)
        (Port      @"DEN"               dEn)
        (Port      @"DI"                dI)
        (Port      @"DWE"               dWe)
        -- I2C interface
        (Port      @"I2C_SCLK"          (pure 0 :: Signal dom Bit))
        (Port      @"I2C_SDA"           (pure 0 :: Signal dom Bit))
{- ORMOLU_ENABLE -}
