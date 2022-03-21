{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Bittide.SharedTypes where
import Clash.Prelude

import Data.Proxy
import Data.Type.Equality ((:~:)(Refl))


type AtLeastOne n = (KnownNat n, 1 <= n)
type ByteEnable bytes = BitVector bytes
type DataLink frameWidth = Maybe (BitVector frameWidth)
type NatFitsInBits n bits = NatRequiredBits n <= bits
type NatRequiredBits n = CLog 2 (n + 1)
type Paddable a = (BitPack a, NFDataX a, 1 <= BitSize a)
type TypeRequiredRegisters t regSize = DivRU (BitSize t) regSize
type WriteAny maxIndex writeData = Maybe (Index maxIndex, writeData)

-- | Vector of registors that stores data coming from a communication bus. It can be used
-- to store any arbitrary data type, the last register is padded with p bits. The number of
-- registers and the amount of padding depends on the bit size of the stored data.
data RegisterBank regSize content where
  RegisterBank :: (KnownNat registers, 1 <= regSize, (p + BitSize content) ~ (regSize * registers)) => SNat p -> SNat registers -> Vec registers (BitVector regSize) -> RegisterBank regSize content

instance (KnownNat regSize, 1 <= regSize ) => ShowX (RegisterBank regSize content) where
  showX (RegisterBank _ _ v) = showX v
  showsPrecX i (RegisterBank _ _ v) = showsPrecX i v

instance (AtLeastOne regSize, Paddable content) => NFDataX (RegisterBank regSize content) where
  deepErrorX str = paddedToRegisters @regSize @content $ padData (deepErrorX str)
  hasUndefined (RegisterBank _ _ v) = hasUndefined v
  rnfX = rnfX
  ensureSpine = id

-- | Data type that ensures a fits in a register bank of size (n * bw), by
-- padding it with p bits.
data Padded bw a where
  Padded :: ((p + BitSize a) ~ (bw * n), Paddable a) => a -> SNat p -> SNat n -> Padded bw a

instance (KnownNat bw, Paddable a, NFDataX a) => NFDataX (Padded bw a) where
  deepErrorX str = padData (errorX str)
  hasUndefined (Padded a _ _) = hasUndefined a
  rnfX = rnfX
  ensureSpine = id

padData :: forall a bw . (KnownNat bw, Paddable a) => a -> Padded bw a
padData a =
  case (someNatVal padding, someNatVal nrOfRegs) of
    (Just (SomeNat p), Just (SomeNat r)) -> go p r
    _ -> error "padData: Negative padding or negative number of registers."
 where
  szA = natVal $ Proxy @(BitSize a)
  busWidth = natVal $ Proxy @bw
  padding = busWidth - 1 - mod (szA - 1) busWidth
  nrOfRegs =
    case quotRem szA busWidth of
      (q, 0) -> q
      (q, _) -> q + 1

  go :: forall p r . (KnownNat p, KnownNat r) => Proxy p -> Proxy r -> Padded bw a
  go p r =
    case sameNat (Proxy @(p + BitSize a)) (Proxy @(bw * r)) of
      Just Refl -> Padded a (snatProxy p) (snatProxy r)
      Nothing -> error "padData: Padded data /= (registers * register size)"

bvAsPadded :: forall bw a. (Paddable a, KnownNat bw) => BitVector bw -> Padded bw a
bvAsPadded bv =
  case someNatVal p of
    Just (SomeNat pNat) ->
      go pNat

    Nothing ->
      error "bvAsPadded: Negative padding"
 where
  szA = natVal (Proxy @(BitSize a))
  bw  = natVal (Proxy @bw)
  p   = bw - szA

  go :: forall p. (KnownNat p) => Proxy p -> Padded bw a
  go p1 =
    case sameNat (Proxy @(p + BitSize a)) (Proxy @bw) of
      Just Refl ->
        Padded (unpack . snd $ split @_ @p bv :: a) (snatProxy p1) (SNat @1)

      Nothing ->
        error "bvAsPadded: Padded data size /= bus width"

paddedToData :: Padded bw a -> a
paddedToData (Padded a _ _ ) = a

registersToData :: (Paddable a, KnownNat regSize) => RegisterBank regSize a -> a
registersToData = paddedToData . registersToPadded

paddedToRegisters :: forall bw a . (BitPack a, KnownNat bw, 1 <= bw) => Padded bw a -> RegisterBank bw a
paddedToRegisters padded =
  case padded of
    Padded a padding regs -> go a padding regs
 where
  go :: forall p r . ((p + BitSize a) ~ (bw  * r)) => a -> SNat p -> SNat r ->  RegisterBank bw a
  go a' p@SNat r@SNat =
    RegisterBank p r $ unpack ((0b0 :: BitVector p) ++# pack a')

registersToPadded :: forall bw a . (Paddable a, KnownNat bw) => RegisterBank bw a -> Padded bw a
registersToPadded rb =
  case rb of
    RegisterBank padding@SNat regs@SNat vec -> go padding regs vec
 where
  go :: forall p r . SNat p -> SNat r -> Vec r (BitVector bw) -> Padded bw a
  go p@SNat r@SNat v =
    case sameNat (Proxy @(p + BitSize a)) (Proxy @(bw * r)) of
      Just Refl -> Padded (unpack . snd . split @_ @p $ pack v) p r
      Nothing   -> error $ "registersToPadded: Incorrect data size: (" <> show p <> " + " <> show (SNat @(BitSize a)) <> ") /= (" <> show (SNat @bw) <> " * " <> show r <> ")"
