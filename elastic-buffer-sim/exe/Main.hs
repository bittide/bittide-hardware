module Main (main) where

import Bittide.Layout

import System.Environment (getArgs)

main :: IO ()
main = dumpCsv . read . head =<< getArgs
