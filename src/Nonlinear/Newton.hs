module Nonlinear.Newton (
    newton
) where

import Data.Tuple.Extra (snd3,thd3)

takeInclusive predicate = uncurry (<>) . fmap (take 1) . span predicate

-- on 1 and 2 > table with the following data:
--      i | x_i | f( x_i ) | f'( x_i )

absurdDistance = 1 / 0

boundedNewton n f f' x0 = 
    take n . iterate newtonRaphson $ (x0,f x0, absurdDistance)
    where newtonRaphson (x,fx,_) =
            let next     = x - fx / (f' x)
                distance = next - x
            in (next, f next, distance)

-- returns (x, f x, distance)
newton n sigma epsilon f f' x0 = takeInclusive stopCriteria . boundedNewton n f f' $ x0
    where rangePrecision = (>= sigma) . abs . thd3
          rootPrecision  = (>= epsilon) . abs . snd3
          stopCriteria iter = rangePrecision iter || rootPrecision iter

