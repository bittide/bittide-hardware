-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Switch where

import Clash.Prelude

import Bittide.Calendar
import Bittide.ScatterGather (scatterEngine)
import Bittide.SharedTypes
import Bittide.Extra.Wishbone (WishboneM2S, WishboneS2M)

-- | An index which source is selected by the crossbar, 0 selects Nothing, k selects k - 1.
type CrossbarIndex links = Index (links+1)

-- | Stores for each link, an index where the incoming frame is written to in the scatter
-- memory and a crossbar index to select the outgoing frame.
type CalendarEntry links memDepth = Vec links (Index memDepth, CrossbarIndex links)

-- TODO: Remove Bittide.ScatterEngine and its tests before merging #71

{-# NOINLINE switch #-}
-- | The Bittide Switch routes data from incoming to outgoing links based on a calendar.
-- The switch consists of a crossbar, a calendar and a scatter engine for all incoming links.
-- The crossbar selects one of the scatter engine outputs for every outgoing link, index 0
-- selects a null frame (Nothing) and k selects engine k - 1.
switch ::
  forall dom nBytes addrW links memDepth frameWidth .
  ( HiddenClockResetEnable dom
  , KnownNat links
  , KnownNat memDepth
  , KnownNat frameWidth
  , 1 <= memDepth) =>
  -- | The calendar configuration
  CalendarConfig nBytes addrW (CalendarEntry links memDepth) ->
  -- | Wishbone interface wired to the calendar.
  Signal dom (WishboneM2S nBytes addrW) ->
  -- | All incoming datalinks
  Signal dom (Vec links (DataLink frameWidth)) ->
  -- | All outgoing datalinks
  (Signal dom (Vec links (DataLink frameWidth)), Signal dom (WishboneS2M nBytes))
switch calConfig wbIn streamsIn =
  (crossBar <$> crossBarConfig <*> availableFrames, wbOut)
 where
  inpBuffer = scatterEngine newMetaCycle
  availableFrames = bundle (inpBuffer <$> unbundle streamsIn <*> unbundle gatherConfig)
  (calendars, newMetaCycle, wbOut) = mkCalendar calConfig wbIn
  (gatherConfig, crossBarConfig)  = unbundle $ unzip <$> calendars

{-# NOINLINE crossBar #-}
-- | The crossbar receives a vector of indices and a vector of incoming frames.
-- For each outgoing link it will select a data source. 0 selects a null frame (Nothing),
-- therefore indexing of incoming links starts at 1 (index 1 selects incoming frame 0).
-- Source: bittide hardware, switch logic.
crossBar ::
  (KnownNat links) =>
  Vec links (CrossbarIndex links) ->
  -- | Source selection for each outgoing link, 0 is a null frame, links start at index 1.
  Vec links a ->
  -- | Vector of incoming links.
  Vec links (Maybe a)
crossBar calendarEntry inputStreams  = fmap selectChannel calendarEntry
 where
  selectChannel i = (Nothing :> (Just <$> inputStreams)) !! i
