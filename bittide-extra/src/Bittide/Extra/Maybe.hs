-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Extra.Maybe where

import Clash.Prelude

import Data.Maybe

{- $setup
>>> import Clash.Prelude
-}

{- | Extract `Just` from a `Vec` of `Maybe`s. Prefer left-most `Just`. If no `Just` is
found, use a default value.

>>> fromMaybesL 0 (Just 1 :> Just 2 :> Nothing :> Nil)
1
>>> fromMaybesL 0 (Nothing :> Nothing :> Nothing :> Nil)
0
-}
fromMaybesL :: a -> Vec n (Maybe a) -> a
fromMaybesL a = fromMaybe a . fold (<|>) . (Nothing :>)

{- | Extract `Just` from a `Vec` of `Maybe`s. Prefer right-most `Just`. If no `Just` is
found, use a default value.

>>> fromMaybesR 0 (Just 1 :> Just 2 :> Nothing :> Nil)
2
>>> fromMaybesR 0 (Nothing :> Nothing :> Nothing :> Nil)
0
-}
fromMaybesR :: a -> Vec n (Maybe a) -> a
fromMaybesR a = fromMaybe a . fold (flip (<|>)) . (Nothing :>)

{- | Returns 'Just a' when the boolean is 'True', or 'Nothing' when 'False'.

* Examples:

   >>> orNothing True 5
   Just 5

   >>> orNothing False "Hello"
   Nothing
-}
orNothing :: Bool -> a -> Maybe a
orNothing True a = Just a
orNothing False _ = Nothing
