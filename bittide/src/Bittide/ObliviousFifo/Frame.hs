-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE PatternSynonyms #-}

module Bittide.ObliviousFifo.Frame
  ( Frame(Data, Empty, Lost)
  ) where

import Clash.Prelude

-- | A 'Frame' separates data packages into either valid data or
-- accumulated lost data. It is also used to indicate an empty
-- buffer.
data Frame a =
    Data a
    -- ^ data frame
  | EmptyOrLost (Index (2^BitSize a))
    -- ^ either an empty or a lost frame
    --
    -- __We use a joint internal constructor here to make use of the
    -- fact that there is no need for ever sending zero lost frames.
    -- Hence, we can re-utilize this value to denote an empty frame
    -- instead, i.e. we consider @EmptyOrLost 0@ to denote an emtpy
    -- frame, while @EmptyOrLost n@ with @n > 0@ denotes some number
    -- of lost frames. We need to join both cases here in order to end
    -- up with the desired 'BitPack' derivation, as using another data
    -- constructor would add an unnecessary bit instead. The
    -- @EmptyOrLost@ constructor is not exported on purpose, but it is
    -- split into the patterns @Empty@ or @Lost@ serving for pattern
    -- matching and smart construction. Accordingly, the user can
    -- never mess up with using them the wrong way.__
  deriving (Eq, Ord, Generic, NFDataX, BitPack)

instance (NFDataX a, BitPack a, Show a) => Show (Frame a) where
  show = \case
    Data (show -> s) -> "Data " <> if ' ' `elem` s then "(" <> s <> ")" else s
    Lost (show -> s) -> "Lost " <> s
    Empty            -> "Empty"

-- | empty buffer indication frame
pattern Empty :: BitPack a => Frame a
pattern Empty <- EmptyOrLost 0 where
  Empty = EmptyOrLost 0

-- | lost data indication frame
pattern Lost :: (NFDataX a, BitPack a) => Index (2^BitSize a) -> Frame a
pattern Lost n <- EmptyOrLost (positiveNum -> Just n) where
  Lost n | n == 0 = deepErrorX
             $ "You cannot loose nothing. "
            <> "Lost can only be applied to values greater than zero."
         | otherwise =
             EmptyOrLost n

-- | A positive number filter.
positiveNum :: (Num a, Ord a) => a -> Maybe a
positiveNum n
  | n >= 1    = Just n
  | otherwise = Nothing

{-# COMPLETE Data, Empty, Lost #-}