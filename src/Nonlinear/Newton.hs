module Nonlinear.Newton (
    newton
) where

import Data.Function    (on)
import Data.Tuple.Extra (snd3,thd3)

-- on 1 and 2 > table with the following data:
--      i | x_i | f( x_i ) | f'( x_i )

--next = prev - (f prev) / (f' prev)

-- how many iters (M)
-- initial point (x): input
-- initial function point (y): f x
--
--
-- with bounds::
-- initial point (x0): input
-- initial function point (v): f x0
--
--  iter through newton-raphson interation
--      next point (x_1): x_0 - v / f' (x_0)
--      update function point (v): f x_1
--
--      %print index, x_1, f x_1 /v/
--      if distance < sigma || function value < epsilon, stop
--      update original point (x0): x1

absurdDistance = 1 / 0
-- TODO: Change `Nothing` by 1 / 0 or notDefined or absurdDistance
boundedNewton n f f' x0 = 
    take n . iterate newtonRaphson $ (x0,f x0, absurdDistance)
    where newtonRaphson (x,fx,_) =
            let next     = x - fx / (f' x)
                distance = next - x
            in (next, f next, distance)

-- returns (x, f x, distance)
newton n sigma epsilon f f' x0 = takeWhile stopCriteria . boundedNewton n f f' $ x0
    where rangePrecision = (>= sigma) . abs . thd3
          rootPrecision  = (>= epsilon) . abs . snd3
          stopCriteria iter = rangePrecision iter || rootPrecision iter

