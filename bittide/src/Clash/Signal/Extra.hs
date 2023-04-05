module Clash.Signal.Extra where

import Clash.Prelude

import Clash.Signal.Internal
import Data.List.NonEmpty (NonEmpty)

import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty

-- | Converts a list of elements into a signal of elements. Unlike 'fromList'
-- it also takes a clock, reset, and enable. When the reset is asserted, it
-- will insert the first value of the given list. When the enable is deasserted,
-- elements are repeated. As usual, the reset takes precedence over the enable.
--
-- __NB__: Not translatable to HDL
fromListWithResetAndEnable ::
  forall dom a .
  (HiddenClockResetEnable dom, NFDataX a, Show a) =>
  NonEmpty a ->
  Signal dom a
fromListWithResetAndEnable inp =
  go
    (fromEnable hasEnable)
    (unsafeToHighPolarity hasReset)
    inpAsSignal

 where
  eolError = deepErrorX "fromListWithResetAndEnable: end of list"
  inpAsSignal = fromList (NonEmpty.toList inp <> L.repeat eolError)

  go :: Signal dom Bool -> Signal dom Bool -> Signal dom a -> Signal dom a
  go (ena :- enables) (rst :- resets) (x :- xs) =
    output :- go enables resets remaining
   where
    output
      | rst       = NonEmpty.head inp
      | otherwise = x
    remaining
      | rst       = inpAsSignal
      | ena       = xs
      | otherwise = x :- xs
