module Nonlinear.Secant (
    secant
) where

import Data.Tuple.Extra (fst3,snd3,thd3)

takeInclusive predicate = uncurry (<>) . fmap (take 1) . span predicate

absurdDistance , absurdValue :: RealFrac x => x
absurdDistance = 1 / 0
absurdValue    = 1 / 0

-- print these x_{i-1}, x_i , x_{i+1}, | x_{i+1} | 

-- return (distance , last_computed, (x_i-1, low = x_i, high = x_i+1) )
boundedSecant n f lowest highest = 
    take n . iterate moveEndsBySecant $ (absurdDistance, f lowest, (absurdValue, lowest , highest) )
    where moveEndsBySecant (_, _, (_,low,high) ) =
            let (shortest,biggest) = if abs (f low) > abs (f high)
                            then (high,low)
                            else (low,high)
                increment    = (biggest - shortest) / (f biggest - f shortest)
                next         = shortest - (f shortest) * increment
                distance     = shortest - next
                computed     = f next
            in (distance, computed, (low, next, shortest))

secant n sigma epsilon f low high = takeInclusive stopCriteria . boundedSecant n f low $ high
    where rangePrecision = (>= sigma) . abs . fst3
          rootPrecision  = (>= epsilon) . abs . snd3
          stopCriteria iter = rangePrecision iter || rootPrecision iter

