{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
module Bittide.Switch (switch) where

import Clash.Prelude

import Bittide.Calendar
import Bittide.ScatterGather (scatterEngine)

type DataLink a = Maybe (BitVector a)
type CrossbarIndex links = Index (links+1)
type CalendarEntry links memDepth = Vec links (Index memDepth, CrossbarIndex links)
type Calendar calDepth links memDepth = Vec calDepth (CalendarEntry links memDepth)
type ConfigurationPort calDepth links memDepth =
  Maybe (Index calDepth, CalendarEntry links memDepth)

-- | The Bittide Switch routes data from incoming to outgoing links based on a calendar.
-- The switch consists of a crossbar, a calendar and a scatter engine for all incoming links.
-- The crossbar selects one of te scatter engine outputs for every outgoing link, index 0
-- selects a null frame (Nothing) and higher indexes select read data from the scatter engines.
-- 1, selects engine 0, 2 select engine 1, etc.
switch ::
  forall dom links calDepth memDepth frameWidth .
  ( HiddenClockResetEnable dom
  , KnownNat links
  , KnownNat calDepth
  , KnownNat memDepth
  , KnownNat frameWidth
  , 1 <= links
  , 1 <= calDepth
  , 1 <= memDepth) =>
  -- | The bootstrap calendar.
  Calendar calDepth links memDepth ->
  -- | Configuration port for the switch calendar (which also controls scatter engines).
  Signal dom (ConfigurationPort calDepth links memDepth) ->
  -- | All incoming datalinks
  Signal dom (Vec links (DataLink frameWidth)) ->
  -- | All outgoing datalinks
  Signal dom (Vec links (DataLink frameWidth))
switch bootstrapCal writeCalendar streamsIn = crossBar <$> crossBarConfig <*> streams'
  where
    buffers = scatterEngine newMetaCycle
    streams' = bundle (buffers <$> unbundle streamsIn <*> unbundle gatherConfig)
    (calendars, newMetaCycle) = calendar bootstrapCal (pure False) writeCalendar
    (gatherConfig, crossBarConfig)  = unbundle $ unzip <$> calendars

-- | The crossbar receives a vector of indices and a vector of incoming frames.
-- For each outgoing link it will select a data source. 0 selects a null frame (Nothing),
-- therefore indexing of incoming links starts at 1 (index 1 selects incoming frame 0).
-- Source: bittide hardware, switch logic.
crossBar ::
  (KnownNat links, 1 <= links) =>
  Vec links (CrossbarIndex links) ->
  -- | Source selection for each outgoing link, 0 is a null frame, links start at index 1.
  Vec links a ->
  -- | Vector of incoming links.
  Vec links (Maybe a)
crossBar calendarEntry inputStreams  = fmap selectChannel calendarEntry
  where
    selectChannel i = (Nothing :> (Just <$> inputStreams)) !! i
