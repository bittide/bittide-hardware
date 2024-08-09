-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitPrelude #-}

module Bittide.Simulate (
  OutputMode (..),
  SimPlotSettings (..),
  simPlot,
  someCCC,
) where

import Clash.Prelude (
  KnownDomain,
  KnownNat,
  SomeNat (..),
  natToNum,
  snatProxy,
  someNatVal,
  type (+),
  type (<=),
 )

import Clash.Signal.Internal (Femtoseconds (..))
import Clash.Sized.Vector qualified as V

import Control.Monad (forM_, zipWithM_)
import Data.Aeson (FromJSON, ToJSON, Value (..))
import Data.Aeson qualified as A
import Data.Aeson.Types (typeMismatch)
import Data.Array (bounds, (!))
import Data.ByteString.Lazy qualified as BSL (appendFile)
import Data.Csv (encode, toField)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import System.Exit (die)
import System.FilePath ((</>))
import System.Random (Random (..), randomRIO)
import Text.Read (Read (..), lexP, pfail, readMaybe)
import Text.Read.Lex (Lexeme (Ident))

import Data.Type.Equality ((:~:) (..))
import GHC.TypeLits.Compare ((:<=?) (..))
import GHC.TypeLits.Witnesses ((%<=?))
import GHC.TypeLits.Witnesses qualified as TLW (SNat (..))

import Bittide.Arithmetic.Ppm (Ppm (..), diffPeriod)
import Bittide.ClockControl (ClockControlConfig (..), clockPeriodFs, defClockConfig)
import Bittide.Plot (fromRfState, plot)
import Bittide.Simulate.Topology (allSettled, simulate, simulationEntity)
import Bittide.Topology

data OutputMode = CSV | PDF
  deriving (Ord, Eq)

instance Show OutputMode where
  show = \case
    CSV -> "csv"
    PDF -> "pdf"

instance Read OutputMode where
  readPrec =
    lexP >>= \case
      Ident "csv" -> return CSV
      Ident "pdf" -> return PDF
      _ -> pfail

instance ToJSON OutputMode where
  toJSON = String . T.pack . show

instance FromJSON OutputMode where
  parseJSON v = case v of
    String str -> maybe tmm return $ readMaybe $ T.unpack str
    _ -> tmm
   where
    tmm = typeMismatch "OutputMode" v

data SimPlotSettings = SimPlotSettings
  { plotSamples :: Int
  , periodsize :: Int
  , mode :: OutputMode
  , dir :: FilePath
  , stopStable :: Maybe Int
  , fixClockOffs :: [Float]
  , fixStartDelays :: [Int]
  , maxStartDelay :: Int
  , sccc :: SomeClockControlConfig
  , save :: [Float] -> [Int] -> Maybe Bool -> IO ()
  }

{- | Creates some clock control configuration from the default with
the given parameters modified.
-}
someCCC ::
  forall dom.
  (KnownDomain dom) =>
  Proxy dom ->
  Bool ->
  Bool ->
  Int ->
  Integer ->
  Integer ->
  IO SomeClockControlConfig
someCCC _ reframe rustySim waittime margin framesize =
  case someNatVal margin of
    Just (SomeNat pMargin) -> case somePositiveNat framesize of
      Just (SomePositiveNat pFramesize) ->
        return $
          SomeClockControlConfig @dom @12 $
            ClockControlConfig
              { cccStabilityCheckerMargin = snatProxy pMargin
              , cccStabilityCheckerFramesize = snatProxy pFramesize
              , cccEnableReframing = reframe
              , cccReframingWaitTime = fromInteger $ toInteger waittime
              , cccEnableRustySimulation = rustySim
              , ..
              }
      _ -> die "ERROR: the given frame size must be positive"
    _ -> die "ERROR: the given margin must be non-negative"
 where
  ClockControlConfig{..} = defClockConfig @dom

  somePositiveNat :: Integer -> Maybe SomePositiveNat
  somePositiveNat n =
    someNatVal n >>= \(SomeNat (_ :: p n)) ->
      case TLW.SNat @1 %<=? TLW.SNat @n of
        LE Refl -> Just $ SomePositiveNat (Proxy @n)
        _ -> Nothing

{- | Simulates and plots the given topology according to the given
parameters.
-}
simPlot :: STop -> SimPlotSettings -> IO Bool
simPlot (STop (t :: Topology n)) settings@SimPlotSettings{..} =
  case TLW.SNat @1 %<=? topSize t of
    LE Refl -> case topSize t %<=? TLW.SNat @20 of
      LE Refl -> case sccc of
        SomeClockControlConfig (ccc :: ClockControlConfig dom d m f) ->
          case TLW.SNat @1 %<=? TLW.SNat @f of
            LE Refl -> case TLW.SNat @1 %<=? TLW.SNat @d of
              LE Refl -> case TLW.SNat @(n + d) %<=? TLW.SNat @32 of
                LE Refl -> case TLW.SNat @(1 + n) %<=? TLW.SNat @32 of
                  LE Refl -> simPlot# settings ccc t
                  _ -> die "ERROR: 1 + nodes <= 32"
                _ -> die "ERROR: nodes + dcount <= 32"
              _ -> die "ERROR: elastic buffer data counts must contain data"
            _ -> die "ERROR: the given frame size must be positive"
      _ -> die "ERROR: the given topology must have not more than 20 nodes"
    _ -> die "ERROR: the given topology must have at least 1 node"
 where
  topSize :: (KnownNat n) => Topology n -> TLW.SNat n
  topSize = const TLW.SNat

{- | Creates and write plots for a given topology according to the
given output mode.
-}
simPlot# ::
  forall dom nodes dcount margin framesize.
  ( KnownDomain dom
  , -- \^ domain
    KnownNat nodes
  , -- \^ the size of the topology is know
    KnownNat dcount
  , -- \^ the size of the data counts is known
    KnownNat margin
  , -- \^ the margins of the stability checker are known
    KnownNat framesize
  , -- \^ the frame size of cycles within the margins required is known
    1 <= nodes
  , -- \^ the topology consists of at least one node
    1 <= dcount
  , -- \^ data counts must contain data
    nodes + dcount <= 32
  , -- \^ computational limit of the clock control
    1 + nodes <= 32
  , -- \^ computational limit of the clock control
    1 <= framesize
  ) =>
  -- \^ frames must at least cover one element

  -- | simulation settings
  SimPlotSettings ->
  -- | clock control configuration
  ClockControlConfig dom dcount margin framesize ->
  -- | the topology
  Topology nodes ->
  -- | stability result
  IO Bool
simPlot# simSettings ccc t = do
  clockOffsets <-
    V.zipWith (maybe id const) givenClockOffsets
      <$> genClockOffsets ccc
  startupDelays <-
    V.zipWith (maybe id const) givenStartupDelays
      <$> genStartupOffsets maxStartDelay
  let
    simResult =
      simulate t stopStable plotSamples periodsize $
        simulationEntity
          t
          ccc
          (Femtoseconds . floor <$> clockOffsets)
          startupDelays
    saveSettings = save (V.toList clockOffsets) (V.toList startupDelays)

  saveSettings Nothing

  case mode of
    PDF -> plot dir t $ fmap (fmap (\(a, b, c, d) -> (a, b, fromRfState c, d))) simResult
    CSV -> dumpCsv simResult

  let result = allSettled $ V.map last simResult
  saveSettings $ Just result
  return result
 where
  SimPlotSettings
    { plotSamples
    , periodsize
    , mode
    , dir
    , stopStable
    , fixClockOffs
    , fixStartDelays
    , maxStartDelay
    , save
    } = simSettings

  givenClockOffsets =
    V.unsafeFromList $
      take (natToNum @nodes) $
        (Just <$> fixClockOffs) <> repeat Nothing

  givenStartupDelays =
    V.unsafeFromList $
      take (natToNum @nodes) $
        (Just <$> fixStartDelays) <> repeat Nothing

  dumpCsv simulationResult = do
    forM_ [0 .. n] $ \i -> do
      let eb = topologyGraph t ! i
      writeFile
        (filename i)
        ( "t,clk"
            <> show i
            <> concatMap (\j -> ",eb" <> show i <> show j) eb
            <> "\n"
        )
    let dats = V.map (encode . fmap flatten) simulationResult
    zipWithM_
      (\dat i -> BSL.appendFile (filename i) dat)
      (V.toList dats)
      [(0 :: Int) ..]

  filename i = dir </> "clocks" <> "_" <> show i <> ".csv"
  flatten (a, b, _, v) = toField a : toField b : (toField . fst <$> v)
  (z, n)
    | z == 0 = bounds $ topologyGraph t
    | otherwise = error "lower bound not 0"

-- | Generates a vector of random clock offsets.
genClockOffsets ::
  forall dom k n m c a.
  (KnownDomain dom, KnownNat k, KnownNat n, Random a, Num a) =>
  ClockControlConfig dom n m c ->
  IO (V.Vec k a)
genClockOffsets ClockControlConfig{cccDeviation} =
  V.traverse# (const genOffset) $ V.repeat ()
 where
  genOffset = do
    Ppm offsetPpm <- randomRIO (-cccDeviation, cccDeviation)
    let nonZeroOffsetPpm = if offsetPpm == 0 then cccDeviation else Ppm offsetPpm
        Femtoseconds fs = diffPeriod nonZeroOffsetPpm $ clockPeriodFs @dom Proxy
    pure $ fromIntegral fs

-- | Generates a vector of random startup offsets.
genStartupOffsets :: (Random a, Num a, KnownNat k) => a -> IO (V.Vec k a)
genStartupOffsets limit =
  V.traverse# (const ((+ 1) <$> randomRIO (0, limit))) $ V.repeat ()

data SomePositiveNat
  = forall n.
    (KnownNat n, 1 <= n) =>
    SomePositiveNat (Proxy n)

data SomeClockControlConfig
  = forall dom dcount margin framesize.
    ( KnownDomain dom
    , KnownNat dcount
    , KnownNat margin
    , KnownNat framesize
    ) =>
    SomeClockControlConfig (ClockControlConfig dom dcount margin framesize)
