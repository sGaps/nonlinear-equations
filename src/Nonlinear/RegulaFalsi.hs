module Nonlinear.RegulaFalsi (
    regulaFalsi
) where

import Data.Tuple.Extra (fst3,snd3,thd3)

-- takeWhile that includes the point where the condition isn't met
takeInclusive predicate = uncurry (<>) . fmap (take 1) . span predicate

absurdDistance , absurdValue :: RealFrac x => x
absurdDistance = 1 / 0
absurdValue    = 1 / 0

--                                                          
-- return (distance , last_computed, (previous: x_i-1, low = x_i, best position: x_i+1) )
boundedRegula n f lowest highest = 
    take n . iterate moveEndpoints $ (absurdDistance, f lowest, (absurdValue, lowest , highest) )
    where moveEndpoints (_, _, (_,low,high)) =
            let slope    = (high - low) / (f high - f low)
                position = high - (f high) * slope
                distance = position - high
                computed = f position

                (overwrote,shortest) = if computed * f high < 0
                            then (low,  high)
                            else (high, low)
            in (distance, computed, (overwrote,shortest,position))

regulaFalsi n sigma epsilon f low high = takeInclusive stopCriteria . boundedRegula n f low $ high
    where rangePrecision = (>= sigma) . abs . fst3
          rootPrecision  = (>= epsilon) . abs . snd3
          stopCriteria iter = rangePrecision iter || rootPrecision iter

