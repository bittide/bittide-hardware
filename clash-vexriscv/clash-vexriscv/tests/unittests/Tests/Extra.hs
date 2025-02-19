-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.Extra where

import Data.Functor ((<&>))
import Language.Haskell.TH (mkName)
import Language.Haskell.TH.Lib
import Prelude

{- | Generate a do-expression where each statement is a call to @test@ and the
arguments are determined by the carthesian product of given argument names.

For example:

>   carthesianProductTests ["x", "y"]
> ======>
>   do
>     test x x
>     test x y
>     test y x
>     test y y
-}
carthesianProductTests :: [String] -> ExpQ
carthesianProductTests names =
  doE $
    cartProd names <&> \(aName, bName) ->
      noBindS $
        let
          aExp = varE (mkName aName)
          bExp = varE (mkName bName)
         in
          [|test $aExp $bExp|]
 where
  cartProd xs = [(a, b) | a <- xs, b <- xs]
