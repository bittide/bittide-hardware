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
  sim plot <steps> <points>

Options:
  <points> Number of points to keep + pass to plotting library
|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` (command "csv")) $ do
    n <- readOrError <$> args `getArgOrExit` (argument "steps")
    p <- readOrError <$> args `getArgOrExit` (argument "points")
    let k = n `quot` p
    dumpCsv p k

  when (args `isPresent` (command "plot")) $ do
    n <- readOrError <$> args `getArgOrExit` (argument "steps")
    p <- readOrError <$> args `getArgOrExit` (argument "points")
    let k = n `quot` p
    plotEbs p k
