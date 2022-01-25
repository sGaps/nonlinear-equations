module Nonlinear.Bisection (
    bisection
) where

import Data.Function    (on)
import Data.List        (scanl')
import Data.Tuple.Extra (thd3)

-- on 1 and 2 > table with the following data:
--      i | x_i | f( x_i ) | f'( x_i )

-- on 3 > table with:
--      i | x_{i-1} | x_i | x_{i+1} | f(x_{i-1}) | f(x_i) | f(x_{i+1}) |

notEqualSign :: (Num a , Eq a) => a -> a -> Bool
notEqualSign = (/=) `on` signum

-- returns (halves , (low , high, last computed))
boundedBisection :: RealFrac x
                    => Int
                    -> (x -> x)
                    -> x
                    -> x
                    -> [(x, (x,x,x))]
boundedBisection n f lowest highest =
                zip halves
                . scanl' moveEndpoints (lowest, highest, f dist)
                . take n $ halves

    where halves = drop 1 . iterate (/2) $ dist
          dist   = highest - lowest
          moveEndpoints (low,high,_) distance =
                let next     = low + distance
                    computed = f next
                in if f low `notEqualSign` computed
                    then (low, next, computed)
                    else (next, high, computed)

-- returns (halves , (low , high, last computed))
-- TODO: replace takeWhile with split or break, so I can include the iteration where it stops
bisection n sigma epsilon f low high = takeWhile stopCriteria . boundedBisection n f low $ high
    where rangePrecision = (>= sigma) . abs . fst
          rootPrecision  = (>= epsilon) . abs . thd3 . snd 
          stopCriteria iter = rangePrecision iter || rootPrecision iter

