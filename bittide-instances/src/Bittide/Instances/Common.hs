-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Common where

import Clash.Prelude

import Bittide.Arithmetic.PartsPer (PartsPer)
import Bittide.ClockControl.Si5395J
import Bittide.Cpus.Types (BittideCpu)
import Bittide.DoubleBufferedRam (ContentType (Vec))
import Bittide.ProcessingElement (PeConfig (..), PrefixWidth)
import Bittide.ProcessingElement.Util (vecsFromElf)
import Data.Maybe (fromMaybe)
import GHC.Float.RealFracMethods (roundFloatInteger)
import GHC.Stack (HasCallStack)
import Project.FilePath (CargoBuildType, findParentContaining, firmwareBinariesDir)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import VexRiscv (DumpVcd (..))

import qualified Bittide.Arithmetic.PartsPer as PartsPer
import Control.Monad.Extra (unlessM)
import Data.String.Interpolate (i)
import System.Directory (doesFileExist)

-- | Availabe step size configurations.
data StepSizeSelect
  = PPB_1
  | PPB_10
  | PPB_100
  | PPB_500
  | PPM_1
  deriving (Generic, NFDataX, BitPack, Eq, Enum, Bounded, Show)

{- | The step size, as it is used by all tests. Note that changing the
step size for individual tests requires recalibration of the clock
offsets, which is why we fix it to a single and common value here.
-}
commonStepSizeSelect :: StepSizeSelect
commonStepSizeSelect =
  -- Don't forget to update the value of f_step this value in "callisto.rs".
  PPB_10

commonStepSizePartsPer :: PartsPer
commonStepSizePartsPer = case commonStepSizeSelect of
  PPB_1 -> PartsPer.ppb 1
  PPB_10 -> PartsPer.ppb 10
  PPB_100 -> PartsPer.ppb 100
  PPB_500 -> PartsPer.ppb 500
  PPM_1 -> PartsPer.ppm 1

partsPerToSteps :: PartsPer -> Signed 32
partsPerToSteps =
  fromIntegral . roundFloatInteger . PartsPer.toSteps commonStepSizePartsPer

commonSpiConfig :: TestConfig6_200_on_0a_RegisterMap
commonSpiConfig = case commonStepSizeSelect of
  PPB_1 -> testConfig6_200_on_0a_1ppb
  PPB_10 -> testConfig6_200_on_0a_10ppb
  PPB_100 -> testConfig6_200_on_0a_100ppb
  PPB_500 -> testConfig6_200_on_0a_500ppb
  PPM_1 -> testConfig6_200_on_0a_1ppm

data PeConfigElfSource
  = NameOnly String
  | TryEnv {envVar :: String, backup :: String}

dumpVcdFromEnvVar :: (HasCallStack) => String -> IO DumpVcd
dumpVcdFromEnvVar envVar = do
  mVal <- lookupEnv envVar
  case mVal of
    Just s -> return $ DumpVcd s
    _ -> return NoDumpVcd

peConfigFromElf ::
  forall depthI depthD nBusses iBusTimeout dBusTimeout.
  ( HasCallStack
  , KnownNat depthI
  , 1 <= depthI
  , KnownNat depthD
  , 1 <= depthD
  , KnownNat nBusses
  , 2 <= nBusses
  , PrefixWidth nBusses <= 30
  ) =>
  SNat depthI ->
  SNat depthD ->
  PeConfigElfSource ->
  CargoBuildType ->
  SNat iBusTimeout ->
  SNat dBusTimeout ->
  Bool ->
  (forall dom. BittideCpu dom) ->
  IO (PeConfig nBusses)
peConfigFromElf depthI depthD elfSource buildType iBusTimeout dBusTimeout includeIlaWb cpu = do
  root <- findParentContaining "cabal.project"
  binaryPath <- case elfSource of
    NameOnly binName -> return $ root </> firmwareBinariesDir "riscv32imc" buildType </> binName
    TryEnv{envVar, backup} -> do
      name <- fromMaybe backup <$> lookupEnv envVar
      return $ root </> firmwareBinariesDir "riscv32imc" buildType </> name
  unlessM (doesFileExist binaryPath)
    $ error [i|Path #{binaryPath} does not point to an extant file!|]
  (iMem, dMem) <- vecsFromElf binaryPath Nothing
  return
    $ PeConfig
      { depthI = depthI
      , depthD = depthD
      , initI = Just (Vec (iMem))
      , initD = Just (Vec (dMem))
      , iBusTimeout = iBusTimeout
      , dBusTimeout = dBusTimeout
      , includeIlaWb = includeIlaWb
      , cpu = cpu
      }

emptyPeConfig ::
  forall depthI depthD nBusses iBusTimeout dBusTimeout.
  ( HasCallStack
  , KnownNat depthI
  , 1 <= depthI
  , KnownNat depthD
  , 1 <= depthD
  , KnownNat nBusses
  , 2 <= nBusses
  , PrefixWidth nBusses <= 30
  ) =>
  SNat depthI ->
  SNat depthD ->
  SNat iBusTimeout ->
  SNat dBusTimeout ->
  Bool ->
  (forall dom. BittideCpu dom) ->
  PeConfig nBusses
emptyPeConfig depthI depthD iBusTimeout dBusTimeout includeIlaWb cpu =
  PeConfig
    { depthI = depthI
    , depthD = depthD
    , initI = Nothing
    , initD = Nothing
    , iBusTimeout = iBusTimeout
    , dBusTimeout = dBusTimeout
    , includeIlaWb = includeIlaWb
    , cpu = cpu
    }
