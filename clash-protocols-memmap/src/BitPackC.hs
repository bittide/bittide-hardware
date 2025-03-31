-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=15 #-}

module BitPackC where

import Clash.Prelude
import GHC.Generics

type SizeInBytes a = DivRU a 8
type NextPowerOfTwo a = 2 ^ CLog 2 (Max 1 a)

type ByteSizeCPaddedTo n a = ByteSizeC a + (n - Mod (ByteSizeC a) n)

type AlignmentPadding at needToGoTo = (needToGoTo - at `Mod` needToGoTo) `Mod` needToGoTo

type TagType nTags = SizeInBytes (CLog 2 nTags)

toLittleEndian :: forall n. (KnownNat n) => BitVector (n * 8) -> BitVector (n * 8)
toLittleEndian bits = pack bytes'
 where
  bytes = unpack bits :: Vec n (BitVector 8)
  bytes' = reverse bytes

fromLittleEndian :: forall n. (KnownNat n) => BitVector (n * 8) -> BitVector (n * 8)
fromLittleEndian = toLittleEndian @n -- hehe

{- | Typeclass that can be implemented (or derived) to enable C-FFI safe
representation of Haskell types.

This can be derived automatically as long as 'Generic' is implemented for the
type.
Currently there is a high likelihood that more complex types need an unusual
amount of constraints on the instance. Ideally this won't be necessary when
@ghc-typelits-extra@ gets extended to automatically discharge those
constraints.
-}
class
  (KnownNat (ByteSizeC a), KnownNat (AlignmentC a), 1 <= AlignmentC a) =>
  BitPackC a
  where
  type ByteSizeC a :: Nat
  type AlignmentC a :: Nat

  type ByteSizeC a = GByteSizeC (Rep a)
  type AlignmentC a = GAlignmentC (Rep a)

  packC :: a -> BitVector (ByteSizeC a * 8)
  unpackC :: BitVector (ByteSizeC a * 8) -> a

  default packC ::
    (Generic a, GBitPackCTop (Rep a), GByteSizeC (Rep a) ~ ByteSizeC a) =>
    a ->
    BitVector (ByteSizeC a * 8)
  packC val = bits
   where
    repr = from val
    bits = gpackCTop repr

  default unpackC ::
    (Generic a, GBitPackCTop (Rep a), GByteSizeC (Rep a) ~ ByteSizeC a) =>
    BitVector (ByteSizeC a * 8) ->
    a
  unpackC bits = val
   where
    val = to repr
    repr = gunpackCTop bits

instance (KnownNat n) => BitPackC (BitVector n) where
  type ByteSizeC (BitVector n) = NextPowerOfTwo (SizeInBytes n)
  type AlignmentC (BitVector n) = ByteSizeC (BitVector n)

  packC val = toLittleEndian $ case compareSNat (SNat @n) (SNat @(ByteSizeC (BitVector n) * 8)) of
    SNatLE -> zeroExtend @_ @n @(ByteSizeC (BitVector n) * 8 - n) val
    SNatGT -> deepErrorX "shouldn't happen"
  unpackC bits = case compareSNat (SNat @n) (SNat @(ByteSizeC (BitVector n) * 8)) of
    SNatLE -> leToPlus @n @(ByteSizeC (BitVector n) * 8) truncateB (fromLittleEndian bits)
    SNatGT -> deepErrorX "shouldn't happen"

instance (KnownNat n) => BitPackC (Unsigned n) where
  type ByteSizeC (Unsigned n) = NextPowerOfTwo (SizeInBytes n)
  type AlignmentC (Unsigned n) = ByteSizeC (Unsigned n)

  packC val = packC (bitCoerce val :: BitVector n)
  unpackC bits = bitCoerce (unpackC bits :: BitVector n)

instance (KnownNat n) => BitPackC (Signed n) where
  type ByteSizeC (Signed n) = NextPowerOfTwo (SizeInBytes n)
  type AlignmentC (Signed n) = ByteSizeC (Signed n)

  packC val = toLittleEndian $ case compareSNat (SNat @n) (SNat @(ByteSizeC (Signed n) * 8)) of
    SNatLE -> bitCoerce $ signExtend @_ @n @(ByteSizeC (Signed n) * 8 - n) val
    SNatGT -> deepErrorX "shouldn't happen"
  unpackC bits = case compareSNat (SNat @n) (SNat @(ByteSizeC (Signed n) * 8)) of
    SNatLE ->
      leToPlus @n @(ByteSizeC (Signed n) * 8) $ bitCoerce (truncateB $ fromLittleEndian bits)
    SNatGT -> deepErrorX "shouldn't happen"

instance (KnownNat n, 1 <= n) => BitPackC (Index n) where
  type ByteSizeC (Index n) = ByteSizeC (BitVector (CLog 2 n))
  type AlignmentC (Index n) = AlignmentC (BitVector (CLog 2 n))

  packC val = packC (pack val)
  unpackC bits = unpack (unpackC bits :: BitVector (CLog 2 n))

instance BitPackC Float where
  type ByteSizeC Float = 4
  type AlignmentC Float = 4

  packC = pack
  unpackC = unpack

instance BitPackC Double where
  type ByteSizeC Double = 8
  type AlignmentC Double = 8

  packC = pack
  unpackC = unpack

instance BitPackC Bool where
  type ByteSizeC Bool = 1
  type AlignmentC Bool = 1

  packC False = 0
  packC True = 1

  unpackC 0 = False
  unpackC _ = True

packCwithAlignPadding ::
  forall align a.
  (KnownNat align, BitPackC a, 1 <= align, Mod (ByteSizeC a) align <= align) =>
  a ->
  BitVector (ByteSizeCPaddedTo align a * 8)
packCwithAlignPadding val = packC val ++# 0

unpackCwithAlignPadding ::
  forall align a.
  (KnownNat align, BitPackC a, 1 <= align, Mod (ByteSizeC a) align <= align) =>
  BitVector (ByteSizeCPaddedTo align a * 8) ->
  a
unpackCwithAlignPadding bits = unpackC trimmed
 where
  (trimmed, _rest) = split bits

instance
  (KnownNat n, BitPackC a, Mod (ByteSizeC a) (AlignmentC a) <= AlignmentC a) =>
  BitPackC (Vec n a)
  where
  type ByteSizeC (Vec n a) = (ByteSizeCPaddedTo (AlignmentC a) a) * n
  type AlignmentC (Vec n a) = AlignmentC a

  packC val = pack $ packCwithAlignPadding @(AlignmentC a) <$> val
  unpackC bits = unpackCwithAlignPadding @(AlignmentC a) <$> unpack bits

instance
  ( BitPackC a
  , Mod 1 (Max 1 (AlignmentC a)) <= 1
  , Mod 1 (Max 1 (AlignmentC a)) <= Max 1 (AlignmentC a)
  ) =>
  BitPackC (Maybe a)
instance
  ( BitPackC a
  , BitPackC b
  , 1 <= Max (AlignmentC a) (AlignmentC b)
  , Mod (ByteSizeC a) (AlignmentC b) <= AlignmentC b
  , Mod (ByteSizeC a) (AlignmentC b) <= ByteSizeC a
  , Mod 1 (Max (AlignmentC a) (AlignmentC b))
      <= Max (AlignmentC a) (AlignmentC b)
  , Mod 1 (Max (AlignmentC a) (AlignmentC b)) <= 1
  ) =>
  BitPackC (Either a b)

instance
  ( BitPackC a
  , BitPackC b
  , 1 <= Max (AlignmentC a) (AlignmentC b)
  , Mod (ByteSizeC a) (AlignmentC b) <= AlignmentC b
  , Mod (ByteSizeC a) (AlignmentC b) <= ByteSizeC a
  ) =>
  BitPackC (a, b)

instance
  ( BitPackC a
  , BitPackC b
  , BitPackC c
  , 1 <= Max (AlignmentC a) (AlignmentC b)
  , 1 <= Max (AlignmentC b) (AlignmentC c)
  , 1 <= Max (AlignmentC a) (Max (AlignmentC b) (AlignmentC c))
  , Mod (ByteSizeC a) (AlignmentC b) <= AlignmentC b
  , Mod (ByteSizeC a) (AlignmentC b) <= ByteSizeC a
  , Mod (ByteSizeC b) (AlignmentC c) <= AlignmentC c
  , Mod (ByteSizeC b) (AlignmentC c) <= ByteSizeC b
  , Mod (ByteSizeC a) (Max (AlignmentC b) (AlignmentC c))
      <= Max (AlignmentC b) (AlignmentC c)
  , Mod (ByteSizeC a) (Max (AlignmentC b) (AlignmentC c))
      <= ByteSizeC a
  ) =>
  BitPackC (a, b, c)

class (KnownNat (GByteSizeC f), KnownNat (GAlignmentC f), 1 <= GAlignmentC f) => GBitPackCTop f where
  type GByteSizeC f :: Nat
  type GAlignmentC f :: Nat

  gpackCTop :: f a -> BitVector (GByteSizeC f * 8)
  gunpackCTop :: BitVector (GByteSizeC f * 8) -> f a

instance (GBitPackCTop inner) => GBitPackCTop (D1 m inner) where
  type GByteSizeC (D1 m inner) = GByteSizeC inner
  type GAlignmentC (D1 m inner) = GAlignmentC inner

  gpackCTop (M1 x) = gpackCTop x
  gunpackCTop bits = M1 $ gunpackCTop bits

instance
  (BitPackC inner) =>
  GBitPackCTop (Rec0 inner)
  where
  type GByteSizeC (Rec0 inner) = ByteSizeC inner
  type GAlignmentC (Rec0 inner) = AlignmentC inner

  gpackCTop (K1 x) = packC x
  gunpackCTop bits = K1 $ unpackC bits

instance GBitPackCTop U1 where
  type GByteSizeC U1 = 0
  type GAlignmentC U1 = 1

  gpackCTop U1 = 0
  gunpackCTop _bits = U1

instance GBitPackCTop V1 where
  type GByteSizeC V1 = 0
  type GAlignmentC V1 = 1

  gpackCTop _ = 0
  gunpackCTop _bits = error "can't construct a value of type V1"

instance (GBitPackCFields inner) => GBitPackCTop (C1 m inner) where
  type GByteSizeC (C1 m inner) = GFieldSize inner
  type GAlignmentC (C1 m inner) = GFieldAlignment inner

  gpackCTop (M1 x) = gpackCfields x
  gunpackCTop bits = M1 $ gunpackCfields bits

instance
  ( GBitPackCSums a
  , GBitPackCSums b
  , 1 <= Max (GSumAlignment a) (GSumAlignment b)
  , ( Mod
        ( Div
            (CLog 2 (GNumConstructors a + GNumConstructors b) + 7)
            8
        )
        (Max (GSumAlignment a) (GSumAlignment b))
        <= Div
            (CLog 2 (GNumConstructors a + GNumConstructors b) + 7)
            8
    )
  , ( Mod
        (Div (CLog 2 (GNumConstructors a + GNumConstructors b) + 7) 8)
        (Max (GSumAlignment a) (GSumAlignment b))
        <= Max (GSumAlignment a) (GSumAlignment b)
    )
  ) =>
  GBitPackCTop (a :+: b)
  where
  type
    GByteSizeC (a :+: b) =
      TagType (GNumConstructors (a :+: b))
        + AlignmentPadding
            (TagType (GNumConstructors (a :+: b)))
            (GSumAlignment (a :+: b))
        + GSumSize (a :+: b)
  type GAlignmentC (a :+: b) = GSumAlignment (a :+: b)

  gpackCTop x = tag ++# padding ++# payload
   where
    tag' = gConstructorTag 0 x
    tag :: BitVector (TagType (GNumConstructors (a :+: b)) * 8) = fromIntegral tag'
    padding = 0
    payload = gpackCsum x
  gunpackCTop bits = gunpackCsum (fromIntegral tag) payload
   where
    ( tag :: BitVector (TagType (GNumConstructors (a :+: b)) * 8)
      , rest
      ) = split bits
    ( _padding
      , payload :: BitVector (GSumSize (a :+: b) * 8)
      ) = split rest

class
  ( KnownNat (GNumConstructors f)
  , KnownNat (GSumSize f)
  , KnownNat (GSumAlignment f)
  , 1 <= GSumAlignment f
  , 1 <= GNumConstructors f
  ) =>
  GBitPackCSums f
  where
  type GNumConstructors f :: Nat
  type GSumSize f :: Nat
  type GSumAlignment f :: Nat

  gConstructorTag :: Int -> f a -> Int

  gpackCsum :: f a -> BitVector (GSumSize f * 8)

  gunpackCsum :: Int -> BitVector (GSumSize f * 8) -> f a

instance
  (GBitPackCSums a, GBitPackCSums b, 1 <= Max (GSumAlignment a) (GSumAlignment b)) =>
  GBitPackCSums (a :+: b)
  where
  type GNumConstructors (a :+: b) = GNumConstructors a + GNumConstructors b
  type GSumSize (a :+: b) = Max (GSumSize a) (GSumSize b)
  type GSumAlignment (a :+: b) = Max (GSumAlignment a) (GSumAlignment b)

  gConstructorTag n (L1 _a) = n
  gConstructorTag n (R1 b) = gConstructorTag (n + 1) b

  gpackCsum (L1 a) = gpackCsum a ++# padding
   where
    padding = 0 :: BitVector ((Max (GSumSize a) (GSumSize b) - GSumSize a) * 8)
  gpackCsum (R1 b) = gpackCsum b ++# padding
   where
    padding = 0 :: BitVector ((Max (GSumSize a) (GSumSize b) - GSumSize b) * 8)

  gunpackCsum 0 bits = L1 $ gunpackCsum 0 bits'
   where
    ( bits'
      , _padding :: BitVector ((Max (GSumSize a) (GSumSize b) - GSumSize a) * 8)
      ) = split bits
  gunpackCsum n bits = R1 $ gunpackCsum (n - 1) bits'
   where
    ( bits'
      , _padding :: BitVector ((Max (GSumSize a) (GSumSize b) - GSumSize b) * 8)
      ) = split bits

instance (GBitPackCFields inner) => GBitPackCSums (C1 m inner) where
  type GNumConstructors (C1 m inner) = 1
  type GSumSize (C1 m inner) = GFieldSize inner
  type GSumAlignment (C1 m inner) = GFieldAlignment inner

  gConstructorTag n _ = n

  gpackCsum (M1 n) = gpackCfields n
  gunpackCsum _ bits = M1 $ gunpackCfields bits

class
  (KnownNat (GFieldSize f), KnownNat (GFieldAlignment f), 1 <= GFieldAlignment f) =>
  GBitPackCFields f
  where
  type GFieldSize f :: Nat
  type GFieldAlignment f :: Nat

  gpackCfields :: f a -> BitVector (GFieldSize f * 8)
  gunpackCfields :: BitVector (GFieldSize f * 8) -> f a

instance GBitPackCFields U1 where
  type GFieldSize U1 = 0
  type GFieldAlignment U1 = 1

  gpackCfields U1 = 0
  gunpackCfields _ = U1

instance (GBitPackCTop inner) => GBitPackCFields (S1 m inner) where
  type GFieldSize (S1 m inner) = GByteSizeC inner
  type GFieldAlignment (S1 m inner) = GAlignmentC inner

  gpackCfields (M1 x) = gpackCTop x
  gunpackCfields bits = M1 $ gunpackCTop bits

instance
  ( GBitPackCFields a
  , GBitPackCFields b
  , 1 <= Max (GFieldAlignment a) (GFieldAlignment b)
  , Mod (GFieldSize a) (GFieldAlignment b) <= GFieldAlignment b
  , ( Mod (GFieldSize a) (GFieldAlignment b)
        <= GFieldSize a
    )
  ) =>
  GBitPackCFields (a :*: b)
  where
  type
    GFieldSize (a :*: b) =
      GFieldSize a + AlignmentPadding (GFieldSize a) (GFieldAlignment b) + GFieldSize b
  type GFieldAlignment (a :*: b) = Max (GFieldAlignment a) (GFieldAlignment b)

  gpackCfields (a :*: b) = gpackCfields a ++# 0 ++# gpackCfields b

  gunpackCfields bits = gunpackCfields a' :*: gunpackCfields b'
   where
    ( a' :: BitVector (GFieldSize a * 8)
      , rest
      ) = split bits
    ( _padding
      , b' :: BitVector (GFieldSize b * 8)
      ) = split rest
