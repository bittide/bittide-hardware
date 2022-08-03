module Main (main) where

import Bittide.Topology

import System.Environment (getArgs)

main :: IO ()
main = dumpCsv . read . head =<< getArgs
