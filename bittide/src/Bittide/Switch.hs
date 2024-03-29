-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Bittide.Switch where

import Clash.Prelude

import Protocols.Wishbone

import Bittide.Calendar
import Bittide.Link
import Bittide.SharedTypes

-- | An index which source is selected by the crossbar, 0 selects Nothing, k selects k - 1.
type CrossbarIndex links = Index (links+1)

-- | Stores for each link, an index where the incoming frame is written to in the scatter
-- memory and a crossbar index to select the outgoing frame.
type CalendarEntry links = Vec links (CrossbarIndex links)

data SwitchConfig links nBytes addrW where
  SwitchConfig ::
    (KnownNat preambleWidth, 1 <= preambleWidth, 1 <= nBytes, 2 <= addrW) =>
    { preamble :: BitVector preambleWidth
    , calendarConfig :: CalendarConfig nBytes addrW (CalendarEntry links)
    }
    -> SwitchConfig links nBytes addrW

deriving instance Show (SwitchConfig links nBytes addrW)

-- | Creates a 'switch' from a 'SwitchConfig'. This wrapper functions hides the @preambleWidth@
-- type variable from the rest of the implementation. For more documentation see 'switch'.
mkSwitch ::
  ( HiddenClockResetEnable dom
  , KnownNat links
  , KnownNat frameWidth, 1 <= frameWidth
  , KnownNat nBytes, 1 <= nBytes
  , KnownNat addrW, 2 <= addrW) =>
  SwitchConfig links nBytes addrW ->
  Signal dom (WishboneM2S addrW nBytes (Bytes nBytes)) ->
  Vec links (Signal dom (WishboneM2S addrW nBytes (Bytes nBytes))) ->
  Vec links (Signal dom (WishboneM2S addrW nBytes (Bytes nBytes))) ->
  Vec links (Signal dom (DataLink frameWidth)) ->
  ( Vec links (Signal dom (DataLink frameWidth))
  , Vec (1 + (links * 2)) (Signal dom (WishboneS2M (Bytes nBytes))))

mkSwitch SwitchConfig{..} = switch preamble calendarConfig

{-# NOINLINE switch #-}
-- TODO: The switch is currently hardcoded to be bidirectional, we intend to change this when
-- the need arises.

-- | The Bittide Switch routes data from incoming links to outgoing links based on a 'Calendar'.
-- The switch consists of a 'crossbar', a 'calendar' and receiver and transmitter logic per link.
-- The receive logic consists of a 'rxUnit' and a receive register (single depth
-- 'Bittide.ScatterGather.scatterUnit'). The transmit logic consists of a transmit register
-- (single depth 'Bittide.ScatterGather.gatherUnit') and a 'txUnit'. The 'crossbar' selects
-- one of the receive register's output for each transmit register. Index @0@ selects a
-- null frame @Nothing@ and @k@ selects receive register @(k - 1)@.
switch ::
  forall dom nBytes addrW links frameWidth preambleWidth .
  ( HiddenClockResetEnable dom
  , KnownNat addrW, 2 <= addrW
  , KnownNat frameWidth, 1 <= frameWidth
  , KnownNat links
  , KnownNat nBytes, 1 <= nBytes
  , KnownNat preambleWidth, 1 <= preambleWidth) =>
  -- | Preamble for Bittide links.
  BitVector preambleWidth ->
  -- | The calendar configuration
  CalendarConfig nBytes addrW (CalendarEntry links) ->
  -- | Wishbone interface wired to the calendar.
  Signal dom (WishboneM2S addrW nBytes (Bytes nBytes)) ->
  -- | Vector of wishbone master busses for the 'rxUnit's.
  Vec links (Signal dom (WishboneM2S addrW nBytes (Bytes nBytes))) ->
  -- | Vector of wishbone master busses for the 'txUnit's.
  Vec links (Signal dom (WishboneM2S addrW nBytes (Bytes nBytes))) ->
  -- | All incoming datalinks
  Vec links (Signal dom (DataLink frameWidth)) ->
  -- | All outgoing datalinks
  ( Vec links  (Signal dom (DataLink frameWidth))
  , Vec (1 + (2 * links)) (Signal dom (WishboneS2M (Bytes nBytes))))
switch preamble calConfig calM2S rxM2Ss txM2Ss streamsIn =
  (streamsOut,calS2M :> (rxS2Ms ++ txS2Ms))
  where
    (cal, _, calS2M) = mkCalendar calConfig calM2S
    rxS2Ms = rxUnit preamble sequenceCounter <$> streamsIn <*> rxM2Ss
    scatterFrames = register Nothing <$> streamsIn
    crossBarOut =  unbundle $ crossBar <$> cal <*> bundle scatterFrames
    gatherFrames = register Nothing <$> crossBarOut
    (txS2Ms, streamsOut) = unzip $ txUnit preamble sequenceCounter <$> gatherFrames <*> txM2Ss

{-# NOINLINE crossBar #-}
-- | The 'crossbar' receives a vector of indices and a vector of incoming frames.
-- For each outgoing link it will select a data source. @0@ selects a null frame @Nothing@,
-- therefore indexing of incoming links starts at 1 (index 1 selects incoming frame 0).
-- Source: bittide hardware, switch logic.
crossBar ::
  (KnownNat links) =>
  Vec links (CrossbarIndex links) ->
  -- | Source selection for each outgoing link, 0 is a null frame, links start at index 1.
  Vec links (Maybe a) ->
  -- | Vector of incoming links.
  Vec links (Maybe a)
crossBar calendarEntry inputStreams = fmap selectChannel calendarEntry
 where
  selectChannel i = (Nothing :> inputStreams) !! i
