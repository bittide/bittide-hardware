-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK hide #-}

module Bittide.Axi4.Internal where

import Clash.Prelude

import Bittide.Extra.Maybe
import Data.Maybe
import Data.Proxy
import Protocols
import Protocols.Axi4.Stream

import qualified Protocols.DfConv as DfConv

{- $setup
>>> import Clash.Prelude
>>> import Data.Maybe
-}

{- | Function to move all keep, data and strobes in an Axi4Stream to the front of
the vectors based on the _tkeep field.
-}
packAxi4Stream ::
  (KnownAxi4StreamConfig conf) =>
  Axi4StreamM2S conf userType ->
  Axi4StreamM2S conf userType
packAxi4Stream axi = output
 where
  output = axi{_tdata = newData, _tstrb = newStrobe, _tkeep = newKeep}
  (newData, newKeep, newStrobe) =
    unzip3
      $ fmap (\b -> (maybe 0 fst b, isJust b, maybe False snd b)) (packVec inpVec)
  inpVec = orNothing <$> _tkeep axi <*> zip (_tdata axi) (_tstrb axi)

{- | Function that moves all @Just@ values in a `Vec n (Maybe a)` to the front of
the vector.

>>> packVec (Just 1 :> Nothing :> Just 2 :> Nil)
Just 1 :> Just 2 :> Nothing :> Nil
>>> packVec (Nothing :> Nothing :> Just 3 :> Nil)
Just 3 :> Nothing :> Nothing :> Nil
-}
packVec :: (KnownNat n) => Vec n (Maybe a) -> Vec n (Maybe a)
packVec = foldr f (repeat Nothing)
 where
  f (Just a) acc = Just a +>> acc
  f Nothing acc = acc

{- | Splits an Axi4StreamM2S into a tuple of two Axi4StreamM2S. The first contains
all lower bytes of the transfer, the second contains the upper bytes. The first
output contains a transfer if at least one of the corresponding keep bits is
high, or none of the keep bits are high. The second output will contain a transfer
only if at least one of the corresponding keep bits is high. A transfer with
only null bytes and _tlast set will produce a transfer with _tlast set in the
first output, the second output will be @Nothing@.
-}
splitAxi4Stream ::
  forall widthA widthB idWith destWidth userTypeA userTypeB.
  ( KnownNat widthA
  , KnownNat widthB
  ) =>
  -- | Axi4Stream transfer to split into two transfers.
  Maybe
    ( Axi4StreamM2S
        ('Axi4StreamConfig (widthA + widthB) idWith destWidth)
        (userTypeA, userTypeB)
    ) ->
  -- |
  -- 1. Axi4Stream transfer with the first half of the data, keep and strobe vectors.
  -- 2. Axi4Stream transfer with the second half of the data, keep and strobe vectors.
  ( Maybe (Axi4StreamM2S ('Axi4StreamConfig widthA idWith destWidth) userTypeA)
  , Maybe (Axi4StreamM2S ('Axi4StreamConfig widthB idWith destWidth) userTypeB)
  )
splitAxi4Stream Nothing = (Nothing, Nothing)
splitAxi4Stream (Just axi) = (orNothing aValid axiA, orNothing bValid axiB)
 where
  axiA =
    Axi4StreamM2S
      { _tdata = dataA
      , _tkeep = keepA
      , _tstrb = strbA
      , _tlast = lastA
      , _tid = _tid axi
      , _tdest = _tdest axi
      , _tuser = fst $ _tuser axi
      }

  axiB =
    Axi4StreamM2S
      { _tdata = dataB
      , _tkeep = keepB
      , _tstrb = strbB
      , _tlast = lastB
      , _tid = _tid axi
      , _tdest = _tdest axi
      , _tuser = snd $ _tuser axi
      }

  (dataA, dataB) = splitAtI $ _tdata axi
  (keepA, keepB) = splitAtI $ _tkeep axi
  (strbA, strbB) = splitAtI $ _tstrb axi

  -- An asserted last signal will be assigned to the "last" valid transfer
  lastA = _tlast axi && not bValid
  lastB = _tlast axi && bValid

  -- The first output is valid if:
  -- \* At least one of the corresponding keep bits is set
  -- \* None of the other keep bits are set.
  aValid = or keepA || lastA
  bValid = or keepB

{- | Extends an @Axi4StreamM2S@ with null bytes. The lower indices of the vectors containing
data, keep and strobe are copied from the input transfer. The upper indices are filled
with null bytes. The _tlast, _tid, _tdest and _tuser fields are passed through.
-}
extendAxi ::
  forall widthA widthB idWith destWidth userType.
  ( KnownNat widthA
  , KnownNat widthB
  , KnownNat idWith
  , KnownNat destWidth
  ) =>
  Axi4StreamM2S ('Axi4StreamConfig widthA idWith destWidth) userType ->
  Axi4StreamM2S ('Axi4StreamConfig (widthA + widthB) idWith destWidth) userType
extendAxi axi =
  Axi4StreamM2S
    { _tdata = _tdata axi ++ repeat 0
    , _tkeep = _tkeep axi ++ repeat False
    , _tstrb = _tstrb axi ++ repeat False
    , _tlast = _tlast axi
    , _tid = _tid axi
    , _tdest = _tdest axi
    , _tuser = _tuser axi
    }

{- | Combines two Axi4StreamM2S into a single Axi4StreamM2S. The data, keep and strobe
vectors are concatenated. The first transfer must contain the lower part of the
data, the second transfer must contain the upper part of the data. If _tlast is
set in the first transfer, a second transfer is not allowed and the function
will return @Nothing@.
-}
combineAxi4Stream ::
  forall widthA widthB idWidth destWidth userTypeA userTypeB.
  ( KnownNat widthA
  , KnownNat widthB
  , KnownNat idWidth
  , KnownNat destWidth
  , NFDataX userTypeA
  , NFDataX userTypeB
  ) =>
  -- | First Axi4Stream transfer, should contain the lower bytes.
  Maybe (Axi4StreamM2S ('Axi4StreamConfig widthA idWidth destWidth) userTypeA) ->
  -- | Second Axi4Stream transfer, should contain the upper bytes.
  Maybe (Axi4StreamM2S ('Axi4StreamConfig widthB idWidth destWidth) userTypeB) ->
  -- | Combined Axi4Stream transfer, or @Nothing@ if the transfers are not compatible.
  Maybe
    ( Axi4StreamM2S
        ('Axi4StreamConfig (widthA + widthB) idWidth destWidth)
        (userTypeA, userTypeB)
    )
combineAxi4Stream maybeAxiA maybeAxiB = case (maybeAxiA, maybeAxiB) of
  (Just axiA, Just axiB) -> orNothing compatibleAxis axiNew
   where
    axiNew =
      Axi4StreamM2S
        { _tdata = _tdata axiA ++ _tdata axiB
        , _tkeep = _tkeep axiA ++ _tkeep axiB
        , _tstrb = _tstrb axiA ++ _tstrb axiB
        , _tlast = _tlast axiB
        , _tid = _tid axiA
        , _tdest = _tdest axiA
        , _tuser = (_tuser axiA, _tuser axiB)
        }
    -- We can only combine two Axi4Streams if they have the same id, dest and the first
    -- transfer is not the end of a packet.
    compatibleAxis =
      _tid axiA == _tid axiB && _tdest axiA == _tdest axiB && not (_tlast axiA)
  (Just axi, Nothing) ->
    Just
      $ Axi4StreamM2S
        { _tdata = _tdata axi ++ repeat 0
        , _tkeep = _tkeep axi ++ repeat False
        , _tstrb = _tstrb axi ++ repeat False
        , _tlast = _tlast axi
        , _tid = _tid axi
        , _tdest = _tdest axi
        , _tuser = (_tuser axi, deepErrorX "combineAxi4Stream: Undefined second _tuser")
        }
  (Nothing, Just axi) ->
    Just
      $ Axi4StreamM2S
        { _tdata = repeat 0 ++ _tdata axi
        , _tkeep = repeat False ++ _tkeep axi
        , _tstrb = repeat False ++ _tstrb axi
        , _tlast = _tlast axi
        , _tid = _tid axi
        , _tdest = _tdest axi
        , _tuser = (deepErrorX "combineAxi4Stream: Undefined first _tuser", _tuser axi)
        }
  _ -> Nothing

{- | A custom of `==` for Axi4StreamM2S that only checks the data bytes if they are valid.
TODO: We should make better use of ADTs in `Axi4StreamM2S` to allow us to use derived
typeclass instances.
-}
eqAxi4Stream ::
  (Eq userType, KnownAxi4StreamConfig conf) =>
  Axi4StreamM2S conf userType ->
  Axi4StreamM2S conf userType ->
  Bool
eqAxi4Stream axiA axiB = lastSame && idSame && destSame && userSame && and keepsSame && and bytesValid
 where
  keepsSame = (==) <$> _tkeep axiA <*> _tkeep axiB
  lastSame = _tlast axiA == _tlast axiB
  idSame = _tid axiA == _tid axiB
  destSame = _tdest axiA == _tdest axiB
  userSame = _tuser axiA == _tuser axiB

  -- For all bytes where the keep is high, the data and strb must be the same.
  keeps = (||) <$> _tkeep axiA <*> _tkeep axiB
  dataSame = (==) <$> _tdata axiA <*> _tdata axiB
  strbSame = (==) <$> _tstrb axiA <*> _tstrb axiB
  bytesValid = zipWith3 (\k d s -> (not k) || (d && s)) keeps strbSame dataSame

-- | Extend a `Vec n a` to an arbitrary, larger size by appending `errorX` values.ip Axi4)
extendWithErrorX ::
  forall n m a.
  (KnownNat n, KnownNat m, NFDataX a) =>
  Vec n a ->
  Vec (n + m) a
extendWithErrorX = (++ deepErrorX "extendWithErrorX: Undefined")

-- | Map a function over the _tuser field of an `Axi4StreamM2S.
axiUserMap ::
  forall userTypeA userTypeB conf.
  (userTypeA -> userTypeB) ->
  Axi4StreamM2S conf userTypeA ->
  Axi4StreamM2S conf userTypeB
axiUserMap f axi = axi{_tuser = f (_tuser axi)}

-- | Circuit version of `axiUserMap`, maps a function over the _tuser field of an `Axi4StreamM2S.
axiUserMapC ::
  forall dom conf userTypeA userTypeB.
  ( KnownAxi4StreamConfig conf
  , HiddenClockResetEnable dom
  , NFDataX userTypeA
  , NFDataX userTypeB
  ) =>
  (userTypeA -> userTypeB) ->
  Circuit
    (Axi4Stream dom conf userTypeA)
    (Axi4Stream dom conf userTypeB)
axiUserMapC f = DfConv.map Proxy Proxy (axiUserMap f)
