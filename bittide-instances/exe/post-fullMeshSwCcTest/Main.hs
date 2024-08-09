-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- | This program extracts the UGNs from the last sample of the ila dumps of the fullMeshSwCcTest,
And combines them from both sides of each link to calculate the IGNs.
-}
module Main where

import qualified Clash.Prelude as C
import Prelude

import Control.DeepSeq (deepseq)
import Control.Monad (filterM, forM, forM_, join)
import qualified Data.ByteString.Lazy as ByteString
import Data.Csv
import Data.Int (Int64)
import Data.List (intercalate, sort)
import Data.Maybe (catMaybes)
import qualified Data.Vector as Vector
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hPutStr, hPutStrLn, stderr, withFile)
import Text.Read (readMaybe)

import Bittide.Instances.Hitl.Setup (fpgaSetup)

type FpgaNo = Int
type FpgaId = String
type TestNo = Int
type LinkNo = Int

data Ugns a = Ugns
  { probe_ugn0 :: a
  , probe_ugn1 :: a
  , probe_ugn2 :: a
  , probe_ugn3 :: a
  , probe_ugn4 :: a
  , probe_ugn5 :: a
  , probe_ugn6 :: a
  }
  deriving (Show, Functor)

ugnsToList :: Ugns a -> [a]
ugnsToList ugns =
  [ probe_ugn0 ugns
  , probe_ugn1 ugns
  , probe_ugn2 ugns
  , probe_ugn3 ugns
  , probe_ugn4 ugns
  , probe_ugn5 ugns
  , probe_ugn6 ugns
  ]

instance (FromField a) => FromNamedRecord (Ugns a) where
  parseNamedRecord m =
    Ugns
      <$> m .: "probe_ugn0"
      <*> m .: "probe_ugn1"
      <*> m .: "probe_ugn2"
      <*> m .: "probe_ugn3"
      <*> m .: "probe_ugn4"
      <*> m .: "probe_ugn5"
      <*> m .: "probe_ugn6"

fpgaIds :: [FpgaId]
fpgaIds = C.toList $ fmap fst fpgaSetup

fpgas :: [(FpgaNo, FpgaId)]
fpgas = zip [(0 :: FpgaNo) ..] fpgaIds

fpgaLinks :: [[Int]]
fpgaLinks = C.toList $ fmap (C.toList . fmap fromIntegral . snd) fpgaSetup

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ilaDir, exitCode0] -> case readMaybe @ExitCode exitCode0 of
      Nothing -> error $ "Couldn't parse second argument (" <> show exitCode0 <> ") as ExitCode"
      Just exitCode
        | exitCode /= ExitSuccess -> error $ "Test run failed, got exit code " <> show exitCode
        | otherwise -> doIt ilaDir
    ilaDir : _ -> doIt ilaDir
    [] -> error "I need the path to the ila-data"

-- | Parses "CCn" to Just n, otherwise Nothing
parseCCnum :: String -> Maybe TestNo
parseCCnum str = case splitAt 2 str of
  ("CC", n) -> readMaybe n
  _ -> Nothing

doIt :: FilePath -> IO ()
doIt ilaDir = do
  dirs :: [FilePath] <-
    join $ (filterM (\d -> doesDirectoryExist (ilaDir </> d))) <$> listDirectory ilaDir
  let testNos = sort $ catMaybes $ map parseCCnum dirs

  tests :: [[[Int64]]] <- forM testNos $ \testNo ->
    forM fpgas $ \(fpgaNo, fpgaId) -> do
      let filename = ilaDir </> "CC" ++ show testNo </> show fpgaNo ++ "_" ++ fpgaId </> "fincFdecIla.csv"
      hPutStrLn stderr $ "Reading " <> filename
      Right (_, csv0) <- decodeByName @(Ugns String) <$> ByteString.readFile filename
      let results = ugnsToList $ read @Int64 . ("0x" <>) <$> Vector.last csv0
      results `deepseq` pure results

  ugns <- forM tests $ \test -> do
    forM (zip [(0 :: FpgaNo) ..] test) $ \(fpgano, ugns) -> do
      fmap (fpgano,) $
        forM (zip [(0 :: LinkNo) ..] ugns) $ \(linkno, ugn) -> do
          let otherUgn = test !! (fpgaLinks !! fpgano !! linkno) !! linkno
          pure (linkno, ugn, ugn + otherUgn)

  forM_ fpgas $ \(fpgaNo, _) -> do
    withFile (ilaDir </> "results_fpga_" <> show fpgaNo <> ".csv") WriteMode $ \h -> do
      let ugns1 = (!! fpgaNo) <$> ugns
      hPutStr h "testno,"
      y <- forM [(0 :: LinkNo) .. 6] $ \linkNo ->
        pure $ "ugn" <> show linkNo <> "," <> "ign" <> show linkNo
      hPutStrLn h (intercalate "," y)

      forM_ (zip testNos ugns1) $ \(testNo, (_fpgaNo, rows)) -> do
        hPutStr h $ show testNo <> ","
        x <- forM rows $ \(_linkNo, ugn, ign) -> do
          pure $ show ugn <> "," <> show ign
        hPutStrLn h $ intercalate "," x
