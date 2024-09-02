-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Bittide.Switch where

import Clash.Prelude

import Bittide.Calendar
import Bittide.SharedTypes
import Data.Constraint.Nat.Extra
import Protocols
import Protocols.Wishbone

-- | An index which source is selected by the crossbar, 0 selects Nothing, k selects k - 1.
type CrossbarIndex links = Index (links + 1)

{- | Stores for each link, an index where the incoming frame is written to in the scatter
memory and a crossbar index to select the outgoing frame.
-}
type CalendarEntry links = Vec links (CrossbarIndex links)

switchC ::
  forall dom nBytes addrW links frameWidth.
  ( HiddenClockResetEnable dom
  , KnownNat links
  , KnownNat frameWidth
  , 1 <= frameWidth
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  , 2 <= addrW
  ) =>
  CalendarConfig nBytes addrW (CalendarEntry links) ->
  Circuit
    ( CSignal dom (Vec links (DataLink frameWidth))
    , Wishbone dom 'Standard addrW (Bytes nBytes)
    )
    (CSignal dom (Vec links (DataLink frameWidth)))
switchC conf = case (cancelMulDiv @nBytes @8) of
  Dict -> Circuit go
   where
    go ((unbundle -> streamsIn, calM2S), _) = ((pure (), calS2M), bundle streamsOut)
     where
      (streamsOut, calS2M) = switch conf calM2S streamsIn

{-# NOINLINE switch #-}

-- TODO: The switch is currently hardcoded to be bidirectional, we intend to change this when
-- the need arises.

{- | The Bittide Switch routes data from incoming links to outgoing links based on a 'Calendar'.
The switch consists of a 'crossbar', a 'calendar' and receiver and transmitter logic per link.
The receive logic consists of a 'rxUnit' and a receive register (single depth
'Bittide.ScatterGather.scatterUnit'). The transmit logic consists of a transmit register
(single depth 'Bittide.ScatterGather.gatherUnit') and a 'txUnit'. The 'crossbar' selects
one of the receive register's output for each transmit register. Index @0@ selects a
null frame @Nothing@ and @k@ selects receive register @(k - 1)@.
-}
switch ::
  forall dom nBytes addrW links frameWidth.
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , 2 <= addrW
  , KnownNat frameWidth
  , 1 <= frameWidth
  , KnownNat links
  , KnownNat nBytes
  , 1 <= nBytes
  ) =>
  CalendarConfig nBytes addrW (CalendarEntry links) ->
  -- | Wishbone interface wired to the calendar.
  Signal dom (WishboneM2S addrW nBytes (Bytes nBytes)) ->
  -- | All incoming datalinks
  Vec links (Signal dom (DataLink frameWidth)) ->
  -- | All outgoing datalinks
  ( Vec links (Signal dom (DataLink frameWidth))
  , Signal dom (WishboneS2M (Bytes nBytes))
  )
switch calConfig calM2S streamsIn = (streamsOut, calS2M)
 where
  (cal, _, calS2M) = mkCalendar @dom @nBytes @addrW calConfig calM2S
  scatterFrames = register Nothing <$> streamsIn
  gatherFrames = unbundle $ crossBar <$> cal <*> bundle scatterFrames
  streamsOut = register Nothing <$> gatherFrames

{-# NOINLINE crossBar #-}

{- | The 'crossbar' receives a vector of indices and a vector of incoming frames.
For each outgoing link it will select a data source. @0@ selects a null frame @Nothing@,
therefore indexing of incoming links starts at 1 (index 1 selects incoming frame 0).
Source: bittide hardware, switch logic.
-}
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
