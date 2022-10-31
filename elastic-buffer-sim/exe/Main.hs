-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main (main) where

import Control.Monad (when)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import System.Console.Docopt
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Bittide.Topology

readOrError :: forall a. (Typeable a, Read a) => String -> a
readOrError s =
  case readMaybe s of
    Nothing -> error ("Could not parse '" <> s <> "' as '" ++ show (typeRep (Proxy @a)) ++ "'")
    Just a  -> a

patterns :: Docopt
patterns = [docopt|
sim version 0.1.0

Usage:
  sim csv <steps> <points>
  sim plot complete2 <steps> <points>
  sim plot complete3 <steps> <points>
  sim plot complete6 <steps> <points>
  sim plot diamond <steps> <points>
  sim plot star7 <steps> <points>
  sim plot tree32 <steps> <points>
  sim plot tree23 <steps> <points>
  sim plot hypercube3 <steps> <points>
  sim plot hypercube4 <steps> <points>
  sim check complete3 <interval> <steps>

Options:
  <points> Number of points to keep + pass to plotting library
|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` (command "check")) $ do
    n <- readOrError <$> args `getArgOrExit` (argument "interval")
    s <- readOrError <$> args `getArgOrExit` (argument "steps")
    isStable <- (!! s) <$> checkStabilityK3 n
    if isStable
      then putStrLn ("Stabilized after " ++ show s ++ " steps.")
      else error ("Did not stabilize after " ++ show s ++ " steps")

  when (args `isPresent` (command "csv")) $ do
    n <- readOrError <$> args `getArgOrExit` (argument "steps")
    p <- readOrError <$> args `getArgOrExit` (argument "points")
    let k = n `quot` p
    dumpCsv p k

  when (args `isPresent` (command "plot")) $ do
    n <- readOrError <$> args `getArgOrExit` (argument "steps")
    p <- readOrError <$> args `getArgOrExit` (argument "points")
    let k = n `quot` p
    let
      plotFn
        | args `isPresent` (command "diamond") = plotDiamond
        | args `isPresent` (command "complete2") = plotK2
        | args `isPresent` (command "complete3") = plotK3
        | args `isPresent` (command "complete6") = plotK6
        | args `isPresent` (command "star7") = plotStar7
        | args `isPresent` (command "tree32") = plotTree32
        | args `isPresent` (command "tree23") = plotTree23
        | args `isPresent` (command "hypercube3") = plotHypercube
        | args `isPresent` (command "hypercube4") = plotHypercube4
        | otherwise = error "Internal error: Unknown command"
    plotFn p k
