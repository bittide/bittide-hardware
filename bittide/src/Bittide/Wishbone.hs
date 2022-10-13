-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0


{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Bittide.Wishbone where

import Clash.Prelude

import Data.Constraint (Dict(Dict))
import Protocols.Internal
import Protocols.Wishbone

import Bittide.SharedTypes
import Data.Constraint.Nat.Extra (divWithRemainder)
import Data.Maybe
import Data.Bool(bool)

-- Applying this hint yields a compile error
{-# ANN module "HLint: ignore Functor law" #-}

-- | A vector of base addresses, one for each slave.
type MemoryMap nSlaves = Vec nSlaves (Index nSlaves)

{-# NOINLINE singleMasterInterconnect #-}
-- | Component that maps multiple slave devices to a single master device over the wishbone
-- bus. It routes the incoming control signals to a slave device based on the 'MemoryMap',
-- a vector of base addresses.
singleMasterInterconnect ::
 forall dom nSlaves addrBitsPerBus a .
 ( HiddenClockResetEnable dom
 , KnownNat nSlaves, 1 <= nSlaves
 , KnownNat addrBitsPerBus
 , BitPack a
 , NFDataX a) =>
 MemoryMap nSlaves ->
 Circuit
  (Wishbone dom 'Standard (BitSize (Index nSlaves) + addrBitsPerBus) a)
  (Vec nSlaves (Wishbone dom 'Standard addrBitsPerBus a))
singleMasterInterconnect (fmap pack -> config) =
  Circuit go
 where
  go (masterS, slavesS) =
    fmap unbundle . unbundle $ route <$> masterS <*> bundle slavesS

  route master@(WishboneM2S{..}) slaves = (toMaster, toSlaves)
   where
    oneHotSelected = fmap (==addrIndex) config
    (addrIndex, newAddr) =
      split @_ @(BitSize (Index nSlaves)) @addrBitsPerBus addr
    toSlaves =
      (\newStrobe -> (updateM2SAddr newAddr master){strobe = strobe && newStrobe})
      <$> oneHotSelected
    toMaster
      | busCycle && strobe = foldMaybes emptyWishboneS2M (maskToMaybes slaves oneHotSelected)
      | otherwise = emptyWishboneS2M

-- | Given a vector with elements and a mask, promote all values with a corresponding
-- 'True' to 'Just', others to 'Nothing'.
--
-- Example:
--
-- >>> maskToMaybes ('a' :> 'b' :> Nil) (True :> False :> Nil)
-- Just 'a' :> Nothing :> Nil
--
maskToMaybes :: Vec n a -> Vec n Bool -> Vec n (Maybe a)
maskToMaybes = zipWith (bool Nothing . Just)

-- | Fold 'Maybe's to a single value. If the given vector does not contain any 'Just',
-- the default value is picked. Prefers the leftmost value when the vector contains
-- multiple 'Just's.
--
-- Example:
--
-- >>> foldMaybes 'a' (Nothing :> Just 'c' :> Nil)
-- 'c'
-- >>> foldMaybes 'a' (Just 'b' :> Just 'c' :> Nil)
-- 'b'
-- >>> foldMaybes 'a' (Nothing :> Nothing :> Nil)
-- 'a'
--
foldMaybes :: a -> Vec n (Maybe a) -> a
foldMaybes a Nil = a
foldMaybes dflt v@(Cons _ _) = fromMaybe dflt $ fold (<|>) v

-- | Version of 'singleMasterInterconnect' that does not use the 'Circuit' abstraction
-- from @clash-protocols@ but exposes 'Signal's directly.
singleMasterInterconnect' ::
 forall dom nSlaves addrBitsPerBus a .
 ( HiddenClockResetEnable dom
 , KnownNat nSlaves, 1 <= nSlaves
 , KnownNat addrBitsPerBus
 , BitPack a
 , NFDataX a) =>
 MemoryMap nSlaves ->
 Signal dom (WishboneM2S (BitSize (Index nSlaves) + addrBitsPerBus) (Regs a 8) a) ->
 Signal dom (Vec nSlaves (WishboneS2M a)) ->
 ( Signal dom (WishboneS2M a)
 , Signal dom (Vec nSlaves (WishboneM2S addrBitsPerBus (Regs a 8) a)))
singleMasterInterconnect' config master slaves = (toMaster, bundle toSlaves)
 where
  Circuit f = singleMasterInterconnect @dom @nSlaves @addrBitsPerBus @a config
  (toMaster, toSlaves) =
    case divWithRemainder @(Regs a 8) @8 @7 of
      Dict ->
        f (master, unbundle slaves)
