-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Protocols.Internal.TH.Extra (traceCTupleInstances) where

import Control.Monad (zipWithM)
import Control.Monad.Extra (concatMapM)
import Data.Proxy
import Language.Haskell.TH
import Protocols.Internal.Types.Extra (TraceC (..))
import Protocols.Plugin (Circuit (..))
import Prelude

{- | Generate 'TraceC' instances for tuple sizes @n@ through @m@ (inclusive).

Usage:

> $(traceCTupleInstances 3 12)
-}
traceCTupleInstances :: Int -> Int -> DecsQ
traceCTupleInstances n m = concatMapM traceCTupleInstance [n .. m]

traceCTupleInstance :: Int -> DecsQ
traceCTupleInstance n =
  [d|
    instance ($instCtx) => TraceC $instTy where
      shock $(varP nameN) = $mkTraceShockExpr
      signal $(varP nameN) = $mkTraceSignalExpr
      names $(varP proxyN) $(varP nameN) = $mkSignalNamesExpr
      trace $(varP nameN) = $mkTraceExpr
    |]
 where
  nameN = mkName "name"
  proxyN = mkName "_proxy"
  circTys = map (\i -> varT $ mkName $ "p" <> show i) [1 .. n]
  instCtx = foldl appT (tupleT n) $ map (\ty -> [t|TraceC $ty|]) circTys
  instTy = foldl appT (tupleT n) circTys

  suffixes = map show [0 .. n - 1]
  mkSuffix s = litE $ stringL $ "_" <> s

  fwdPat = tupP $ map (\i -> varP $ mkName $ "fwd" <> show i) [1 .. n]
  bwdPat = tupP $ map (\i -> varP $ mkName $ "bwd" <> show i) [1 .. n]
  fwdExprs' = map (\i -> varE $ mkName $ "fwd" <> show i <> "'") [1 .. n] :: [ExpQ]
  bwdExprs' = map (\i -> varE $ mkName $ "bwd" <> show i <> "'") [1 .. n] :: [ExpQ]

  -- Generate:
  --   Circuit $ \(~(fwd1, ..., fwdN), ~(bwd1, ..., bwdN)) ->
  --     let Circuit traced1 = method @(p1) (name <> "_0")
  --         ...
  --         (bwd1', fwd1') = traced1 (fwd1, bwd1)
  --         ...
  --     in ((bwd1', ..., bwdN'), (fwd1', ..., fwdN'))
  mkCircuitExpr mkMethodExpr = do
    circuitDecs <-
      concat <$> zipWithM (\i ty -> mkCircuitDec (mkMethodExpr ty) i) [1 .. n] circTys
    resultDecs <-
      concatMapM mkResultDec [1 .. n]
    LetE (circuitDecs <> resultDecs)
      <$> [e|($(tupE bwdExprs'), $(tupE fwdExprs'))|]

  mkCircuitDec methodExpr i =
    [d|
      $[p|Circuit $(varP $ mkName $ "traced" <> show i)|] =
        $methodExpr ($(varE nameN) <> $(mkSuffix (suffixes !! (i - 1))))
      |]

  mkResultDec i =
    [d|
      $[p|
        ( $(varP $ mkName $ "bwd" <> show i <> "'")
          , $(varP $ mkName $ "fwd" <> show i <> "'")
          )
        |] =
          $(varE $ mkName $ "traced" <> show i)
            ( $(varE $ mkName $ "fwd" <> show i)
            , $(varE $ mkName $ "bwd" <> show i)
            )
      |]

  mkTraceShockExpr =
    [e|
      Circuit $ \($(tildeP fwdPat), $(tildeP bwdPat)) -> $(mkCircuitExpr (\ty -> [e|traceShock @($ty)|]))
      |]
  mkTraceSignalExpr =
    [e|
      Circuit $ \($(tildeP fwdPat), $(tildeP bwdPat)) -> $(mkCircuitExpr (\ty -> [e|traceSignal @($ty)|]))
      |]
  mkTraceExpr =
    [e|Circuit $ \($(tildeP fwdPat), $(tildeP bwdPat)) -> $(mkCircuitExpr (\ty -> [e|trace @($ty)|]))|]

  mkSignalNamesExpr =
    foldl1
      (\a b -> [e|$a <> $b|])
      ( zipWith
          (\ty s -> [e|(signalNames (Proxy @($ty)) ($(varE nameN) <> $(mkSuffix s)))|])
          circTys
          suffixes
      )
