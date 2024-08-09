-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Bittide.ClockControl.Callisto.Types (
  CallistoResult (..),
  ReframingState (..),
  ControlConfig (..),
  ControlSt (..),
  RelDataCountS (..),
  VecS (..),
  BitsOf,
) where

import Clash.Prelude

import Foreign.C.Types (CInt, CUInt)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (Storable (..))

import Bittide.ClockControl
import Bittide.ClockControl.Foreign.Sizes
import Bittide.ClockControl.StabilityChecker (StabilityIndication)

import Data.Constraint
import Data.Constraint.Nat.Extra (OneMore, oneMore)

-- | Result of the clock control algorithm.
data CallistoResult (n :: Nat) = CallistoResult
  { maybeSpeedChange :: Maybe SpeedChange
  -- ^ Speed change requested for clock multiplier. This is 'Just' for a single
  -- cycle.
  , stability :: Vec n StabilityIndication
  -- ^ All stability indicators for all of the elastic buffers.
  , allStable :: Bool
  -- ^ Joint stability indicator signaling that all elastic buffers
  -- are stable.
  , allSettled :: Bool
  -- ^ Joint "being-settled" indicator signaling that all elastic
  -- buffers have been settled.
  , reframingState :: ReframingState
  -- ^ State of the Reframing detector
  }
  deriving (Generic, NFDataX)

-- | Callisto specific control configuration options.
data ControlConfig (m :: Nat) = ControlConfig
  { reframingEnabled :: Bool
  -- ^ Enable reframing. Reframing allows a system to resettle buffers around
  -- their midpoints, without dropping any frames. For more information, see
  -- [arXiv:2303.11467](https://arxiv.org/abs/2303.11467).
  , waitTime :: Unsigned 32
  -- ^ Number of cycles to wait until reframing takes place after
  -- stability has been detected.
  , targetCount :: RelDataCount m
  -- ^ Target data count. See 'targetDataCount'.
  }

type instance
  SizeOf (ControlConfig m) =
    SizeOf Int + SizeOf Int + SizeOf (RelDataCountS m)

type instance
  Alignment (ControlConfig m) =
    Alignment (RelDataCountS m)

instance
  ( KnownNat m
  , SizeOf (ControlConfig m) ~ 3 * SizeOf Int
  , Alignment (ControlConfig m) ~ Alignment Int
  ) =>
  Storable (ControlConfig m)
  where
  sizeOf = const $ natToNum @(SizeOf (ControlConfig m))
  alignment = const $ natToNum @(Alignment (ControlConfig m))

  peek p =
    let s = natToNum @(SizeOf Int)
     in ControlConfig
          <$> ((/= 0) <$> (peekByteOff p 0 :: IO Int))
          <*> (fromInteger . toInteger <$> (peekByteOff p s :: IO Int))
          <*> (fromInteger . toInteger <$> (peekByteOff p (2 * s) :: IO Int))

  poke p ControlConfig{..} = do
    let s = natToNum @(SizeOf Int)
    pokeByteOff p 0 ((toEnum $ fromEnum reframingEnabled) :: Int)
    pokeByteOff p s ((fromInteger $ toInteger waitTime) :: Int)
    pokeByteOff p (2 * s) ((fromInteger $ toInteger targetCount) :: Int)

{- | State of the state machine for realizing the "detect, store, and
wait" approach of [arXiv:2303.11467](https://arxiv.org/abs/2303.11467)
-}
data ReframingState
  = -- | The controller remains in this state until stability has been
    -- detected.
    Detect
  | -- | The controller remains in this state for the predefined
    -- number of cycles with the assumption that the elastic buffers
    -- of all other nodes are sufficiently stable after that time.
    Wait
      { targetCorrection :: !Float
      -- ^ Stored correction value to be applied at reframing time.
      , curWaitTime :: !(Unsigned 32)
      -- ^ Number of cycles to wait until reframing takes place.
      }
  | -- | Reframing has taken place. There is nothing more to do.
    Done
  deriving (Generic, NFDataX)

type instance
  SizeOf ReframingState =
    SizeOf CUInt + SizeOf Float + SizeOf CUInt

type instance
  Alignment ReframingState =
    Alignment CUInt

instance
  (SizeOf ReframingState ~ 12, Alignment ReframingState ~ 4) =>
  Storable ReframingState
  where
  sizeOf = const $ natToNum @(SizeOf ReframingState)
  alignment = const $ natToNum @(Alignment ReframingState)

  peek p =
    (peekByteOff p 0 :: IO CUInt) >>= \case
      0 -> return Detect
      1 -> return Done
      2 ->
        Wait
          <$> peekByteOff p 4
          <*> ((fromIntegral :: CUInt -> Unsigned 32) <$> peekByteOff p 8)
      _ -> error "out of range"

  poke p = \case
    Detect -> pokeByteOff p 0 (0 :: CUInt)
    Done -> pokeByteOff p 0 (1 :: CUInt)
    Wait{..} -> do
      pokeByteOff p 0 (2 :: CUInt)
      pokeByteOff p 4 targetCorrection
      pokeByteOff p 8 (fromIntegral curWaitTime :: CUInt)

-- | Callisto's internal state used in 'callisto'
data ControlSt = ControlSt
  { _z_k :: !(Signed 32)
  -- ^ Accumulated speed change requests, where speedup ~ 1, slowdown ~ -1.
  , _b_k :: !SpeedChange
  -- ^ Previously submitted speed change request. Used to determine the estimated
  -- clock frequency.
  , _steadyStateTarget :: !Float
  -- ^ Steady-state value (determined when stability is detected for
  -- the first time).
  , rfState :: !ReframingState
  -- ^ finite state machine for reframing detection
  }
  deriving (Generic, NFDataX)

type instance
  SizeOf ControlSt =
    SizeOf CInt + SizeOf SpeedChange + SizeOf Float + SizeOf ReframingState

type instance
  Alignment ControlSt =
    Alignment CInt

instance
  (SizeOf ControlSt ~ 24, Alignment ControlSt ~ 4) =>
  Storable ControlSt
  where
  sizeOf = const $ natToNum @(SizeOf ControlSt)
  alignment = const $ natToNum @(Alignment ControlSt)

  peek p =
    ControlSt
      <$> ((fromIntegral :: CInt -> Signed 32) <$> peekByteOff p 0)
      <*> peekByteOff p 4
      <*> peekByteOff p 8
      <*> peekByteOff p 12

  poke p ControlSt{..} = do
    pokeByteOff p 0 (fromIntegral _z_k :: CInt)
    pokeByteOff p 4 _b_k
    pokeByteOff p 8 _steadyStateTarget
    pokeByteOff p 12 rfState

newtype VecS (n :: Nat) a = VecS (Vec n a)

type instance
  SizeOf (VecS n a) =
    SizeOf Int + n * SizeOf a

type instance
  Alignment (VecS n a) =
    Alignment a

instance
  ( Alignment a ~ Alignment Int
  , Storable a
  , KnownNat (SizeOf a)
  , KnownNat (Alignment a)
  , KnownNat n
  , 1 <= n
  ) =>
  Storable (VecS n a)
  where
  sizeOf = const $ natToNum @(SizeOf (VecS n a))
  alignment = const $ natToNum @(Alignment (VecS n a))

  peek p = do
    let h = natToNum @(SizeOf Int)
        s = natToNum @(SizeOf a)
    vs <- mapM (peekByteOff p . (+ h) . (* s) . fromEnum) indicesI
    return $ VecS $ vs

  poke p (VecS v) = do
    let h = natToNum @(SizeOf Int)
        s = natToNum @(SizeOf a)
        d = p `plusPtr` h
    pokeByteOff p 0 (natToNum @n :: Int)
    mapM_ (\(x, i) -> pokeByteOff d (s * fromEnum i) x) $ zip v indicesI

type VecI n = Vec n (Index n)

type BitsOf a = 8 * SizeOf a

type Elems n =
  Div n (BitsOf Int) + OneMore (Mod n (BitsOf Int))

newtype RelDataCountS (n :: Nat) = RelDataCountS (Signed n)
  deriving newtype (Show, Eq, Ord, Bits, Num, Real, Integral, Enum)

type instance
  SizeOf (RelDataCountS n) =
    SizeOf Int * Elems n

type instance
  Alignment (RelDataCountS n) =
    SizeOf Int

instance (KnownNat n, 1 <= n) => Storable (RelDataCountS n) where
  sizeOf = const $ natToNum @(SizeOf (RelDataCountS n))
  alignment = const $ natToNum @(Alignment (RelDataCountS n))

  peek p = case oneMore @n @(BitsOf Int) of
    Dict ->
      let
        s = natToNum @(BitsOf Int)

        v :: VecI (Elems n)
        v = indicesI

        toEnum' :: Int -> BitVector (BitsOf Int)
        toEnum' = toEnum
       in
        RelDataCountS
          . resize
          . unpack
          . pack
          <$> (sequence $ map (fmap toEnum' . peekByteOff p . (* s) . fromEnum) v)

  poke p (RelDataCountS c) = case oneMore @n @(BitsOf Int) of
    Dict ->
      let
        s = natToNum @(BitsOf Int)

        v :: Vec (Elems n) Int
        v = fromEnum <$> unpack' (pack $ resize' c)

        resize' :: RelDataCount n -> RelDataCount (BitsOf (RelDataCountS n))
        resize' = resize

        unpack' ::
          BitVector (BitsOf (RelDataCountS n)) ->
          Vec (Elems n) (BitVector (BitsOf Int))
        unpack' = unpack
       in
        mapM_ (\(x, i) -> pokeByteOff p (s * fromEnum i) x) $ zip v indicesI
