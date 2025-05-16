-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=6 #-}

module Bittide.ClockControl.Registers where

import Clash.Prelude hiding (PeriodToCycles)

import Protocols
import Protocols.Wishbone

import Bittide.ClockControl
import Bittide.ClockControl.StabilityChecker
import Bittide.Wishbone
import Clash.Functor.Extra
import Clash.Signal.TH.Extra (deriveSignalHasFields)
import Clash.Sized.Vector.ToTuple (vecToTuple)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap

data ClockControlData (nLinks :: Nat) = ClockControlData
  { clockMod :: Maybe SpeedChange
  , stabilityIndications :: Vec nLinks StabilityIndication
  , allStable :: Bool
  , allSettled :: Bool
  }
  deriving (Generic, NFDataX, ShowX, Show)

deriveSignalHasFields ''ClockControlData

{- | A wishbone accessible clock control interface.
This interface receives the link mask and 'RelDataCount's from all links.
Furthermore it produces FINC/FDEC pulses for the clock control boards.

The word-aligned address layout of the Wishbone interface is as follows:

+----------------+--------------------+----------------------+----------------------+
| Field number   |  Field description | Field name           | Offset (in bytes)    |
+================+====================+======================+======================+
| 0              | Number of links    | 'num_links'          | 0x00                 |
| 1              | Link mask          | 'link_mask'          | 0x04                 |
| 2              | Link mask popcount | 'up_links'           | 0x08                 |
| 3              | Speed change       | 'change_speed'       | 0x0C                 |
| 4              | All links stable?  | 'links_stable'       | 0x10                 |
| 5              | All links settled? | 'links_settled'      | 0x14                 |
| 6              | Data counts        | 'data_counts'        | 0x18 -> 0x18 + links |
+----------------+--------------------+----------------------+----------------------+

__NB__: the `Maybe SpeedChange` part of the output is only asserted for a single cycle.
This must be stickied or otherwise held for the minimum pulse width specified by the
clock board this register is controlling.
-}
clockControlWb ::
  forall dom addrW nLinks m margin framesize.
  ( HiddenClockResetEnable dom
  , HasCallStack
  , KnownNat addrW
  , 1 <= framesize
  , 1 <= nLinks
  , KnownNat nLinks
  , KnownNat m
  , m <= 32
  , nLinks <= 32
  ) =>
  -- | Maximum number of elements the incoming buffer occupancy is
  -- allowed to deviate from the current @target@ for it to be
  -- considered "stable".
  SNat margin ->
  -- | Minimum number of clock cycles the incoming buffer occupancy
  -- must remain within the @margin@ for it to be considered "stable".
  SNat framesize ->
  -- | Link mask
  Signal dom (BitVector nLinks) ->
  -- | Counters
  Vec nLinks (Signal dom (RelDataCount m)) ->
  -- | Wishbone accessible clock control circuitry
  Circuit
    (ConstBwd MM, Wishbone dom 'Standard addrW (BitVector 32))
    (CSignal dom (ClockControlData nLinks))
clockControlWb mgn fsz linkMask counters = withMemoryMap mm $ Circuit go
 where
  mm =
    MemoryMap
      { tree = DeviceInstance locCaller "ClockControl"
      , deviceDefs = deviceSingleton deviceDef
      }
  deviceDef =
    DeviceDefinition
      { tags = []
      , registers =
          [ NamedLoc
              { name = Name "num_links" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(BitVector 32)
                    , address = 0x00
                    , access = ReadOnly
                    , tags = []
                    , reset = Nothing
                    }
              }
          , NamedLoc
              { name = Name "link_mask" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(BitVector 32)
                    , address = 0x04
                    , access = ReadOnly
                    , tags = []
                    , reset = Nothing
                    }
              }
          , NamedLoc
              { name = Name "up_links" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(BitVector 32)
                    , address = 0x08
                    , access = ReadOnly
                    , tags = []
                    , reset = Nothing
                    }
              }
          , NamedLoc
              { name = Name "change_speed" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @SpeedChange
                    , address = 0x0C
                    , access = ReadWrite
                    , tags = []
                    , reset = Nothing
                    }
              }
          , NamedLoc
              { name = Name "links_stable" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(BitVector 32)
                    , address = 0x10
                    , access = ReadOnly
                    , tags = []
                    , reset = Nothing
                    }
              }
          , NamedLoc
              { name = Name "links_settled" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(BitVector 32)
                    , address = 0x14
                    , access = ReadOnly
                    , tags = []
                    , reset = Nothing
                    }
              }
          , NamedLoc
              { name = Name "data_counts" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(Vec nLinks (BitVector 32))
                    , address = 0x18
                    , access = ReadOnly
                    , tags = []
                    , reset = Nothing
                    }
              }
          ]
      , deviceName =
          Name
            "ClockControl"
            "This interface receives the link mask and 'RelDataCount's from all links.\nFurthermore it produces FINC/FDEC pulses for the clock control boards."
      , definitionLoc = locHere
      }

  go (wbM2S, _) = (wbS2M, ccd)
   where
    ccd =
      ClockControlData
        <$> fIncDec1
        <*> stabInds
        <*> ccAllStable
        <*> ccAllSettled

    filterCounters vMask vCounts = flip map (zip vMask vCounts)
      $ \(isActive, count) -> if isActive == high then count else 0
    filteredCounters = unbundle $ filterCounters <$> fmap bv2v linkMask <*> bundle counters
    stabInds = bundle $ stabilityChecker mgn fsz <$> filteredCounters

    -- ▗▖ ▗▖▗▖ ▗▖▗▄▄▄▖▗▖  ▗▖    ▗▖  ▗▖▗▄▖ ▗▖ ▗▖     ▗▄▄▖▗▖ ▗▖ ▗▄▖ ▗▖  ▗▖ ▗▄▄▖▗▄▄▄▖    ▗▄▄▄▖▗▖ ▗▖▗▄▄▄▖ ▗▄▄▖
    -- ▐▌ ▐▌▐▌ ▐▌▐▌   ▐▛▚▖▐▌     ▝▚▞▘▐▌ ▐▌▐▌ ▐▌    ▐▌   ▐▌ ▐▌▐▌ ▐▌▐▛▚▖▐▌▐▌   ▐▌         █  ▐▌ ▐▌  █  ▐▌
    -- ▐▌ ▐▌▐▛▀▜▌▐▛▀▀▘▐▌ ▝▜▌      ▐▌ ▐▌ ▐▌▐▌ ▐▌    ▐▌   ▐▛▀▜▌▐▛▀▜▌▐▌ ▝▜▌▐▌▝▜▌▐▛▀▀▘      █  ▐▛▀▜▌  █   ▝▀▚▖
    -- ▐▙█▟▌▐▌ ▐▌▐▙▄▄▖▐▌  ▐▌      ▐▌ ▝▚▄▞▘▝▚▄▞▘    ▝▚▄▄▖▐▌ ▐▌▐▌ ▐▌▐▌  ▐▌▝▚▄▞▘▐▙▄▄▖      █  ▐▌ ▐▌▗▄█▄▖▗▄▄▞▘
    --
    -- ▗▖ ▗▖▗▄▄▖ ▗▄▄▄  ▗▄▖▗▄▄▄▖▗▄▄▄▖    ▗▄▄▄▖▗▖ ▗▖▗▄▄▄▖    ▗▄▄▖ ▗▖ ▗▖ ▗▄▄▖
    -- ▐▌ ▐▌▐▌ ▐▌▐▌  █▐▌ ▐▌ █  ▐▌         █  ▐▌ ▐▌▐▌       ▐▌ ▐▌▐▌ ▐▌▐▌
    -- ▐▌ ▐▌▐▛▀▘ ▐▌  █▐▛▀▜▌ █  ▐▛▀▀▘      █  ▐▛▀▜▌▐▛▀▀▘    ▐▛▀▚▖▐▌ ▐▌ ▝▀▚▖
    -- ▝▚▄▞▘▐▌   ▐▙▄▄▀▐▌ ▐▌ █  ▐▙▄▄▖      █  ▐▌ ▐▌▐▙▄▄▖    ▐▙▄▞▘▝▚▄▞▘▗▄▄▞▘
    readVec =
      dflipflop
        <$> ( pure (natToNum @nLinks)
                :> (zeroExtend @_ @_ @(32 - nLinks) <$> linkMask)
                :> (resize . pack . popCount <$> linkMask)
                :> (resize . pack <$> fIncDec1)
                :> (resize . pack . fmap boolToBit <$> linksStable)
                :> (resize . pack . fmap boolToBit <$> linksSettled)
                :> (pack . (extend @_ @_ @(32 - m)) <<$>> filteredCounters)
            )
    ccAllStable = allAvailable stable <$> linkMask <*> stabInds
    ccAllSettled = allAvailable settled <$> linkMask <*> stabInds

    linksStable = mapAvailable stable False <$> linkMask <*> stabInds
    linksSettled = mapAvailable settled False <$> linkMask <*> stabInds

    mapAvailable fn itemDefault mask = zipWith go1 (bitToBool <$> bv2v mask)
     where
      go1 avail item = if avail then fn item else itemDefault

    allAvailable f x y =
      and $ zipWith ((||) . not) (bitToBool <$> bv2v x) (f <$> y)

    -- ▗▖ ▗▖▗▄▄▖ ▗▄▄▄  ▗▄▖▗▄▄▄▖▗▄▄▄▖    ▗▄▄▄▖▗▖ ▗▖▗▄▄▄▖ ▗▄▄▖    ▗▖ ▗▖▗▖ ▗▖▗▄▄▄▖▗▖  ▗▖
    -- ▐▌ ▐▌▐▌ ▐▌▐▌  █▐▌ ▐▌ █  ▐▌         █  ▐▌ ▐▌  █  ▐▌       ▐▌ ▐▌▐▌ ▐▌▐▌   ▐▛▚▖▐▌
    -- ▐▌ ▐▌▐▛▀▘ ▐▌  █▐▛▀▜▌ █  ▐▛▀▀▘      █  ▐▛▀▜▌  █   ▝▀▚▖    ▐▌ ▐▌▐▛▀▜▌▐▛▀▀▘▐▌ ▝▜▌
    -- ▝▚▄▞▘▐▌   ▐▙▄▄▀▐▌ ▐▌ █  ▐▙▄▄▖      █  ▐▌ ▐▌▗▄█▄▖▗▄▄▞▘    ▐▙█▟▌▐▌ ▐▌▐▙▄▄▖▐▌  ▐▌
    --
    -- ▗▖  ▗▖▗▄▖ ▗▖ ▗▖     ▗▄▄▖▗▖ ▗▖ ▗▄▖ ▗▖  ▗▖ ▗▄▄▖▗▄▄▄▖    ▗▄▄▄▖▗▖ ▗▖▗▄▄▄▖    ▗▄▄▖ ▗▖ ▗▖ ▗▄▄▖
    --  ▝▚▞▘▐▌ ▐▌▐▌ ▐▌    ▐▌   ▐▌ ▐▌▐▌ ▐▌▐▛▚▖▐▌▐▌   ▐▌         █  ▐▌ ▐▌▐▌       ▐▌ ▐▌▐▌ ▐▌▐▌
    --   ▐▌ ▐▌ ▐▌▐▌ ▐▌    ▐▌   ▐▛▀▜▌▐▛▀▜▌▐▌ ▝▜▌▐▌▝▜▌▐▛▀▀▘      █  ▐▛▀▜▌▐▛▀▀▘    ▐▛▀▚▖▐▌ ▐▌ ▝▀▚▖
    --   ▐▌ ▝▚▄▞▘▝▚▄▞▘    ▝▚▄▄▖▐▌ ▐▌▐▌ ▐▌▐▌  ▐▌▝▚▄▞▘▐▙▄▄▖      █  ▐▌ ▐▌▐▙▄▄▖    ▐▙▄▞▘▝▚▄▞▘▗▄▄▞▘
    --
    -- Pull out the write-able fields
    (_, _, _, f3, _, _) = unbundle $ vecToTuple . take (SNat :: SNat 6) <$> writeVec

    fIncDec0 :: Signal dom (Maybe SpeedChange)
    fIncDec0 = unpack . resize <<$>> f3
    fIncDec1 :: Signal dom (Maybe SpeedChange)
    fIncDec1 = register Nothing fIncDec0

    (writeVec, wbS2M) = unbundle $ wbToVec @4 @_ <$> bundle readVec <*> wbM2S
