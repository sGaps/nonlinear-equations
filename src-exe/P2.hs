module Main (main) where

import Nonlinear.Newton (newton)
import Control.Monad    (forM_)

-- Constants for `f` and `f'`
a = 1/4
b = 1/2

-- The input function and its derivative
f  x = a*(x^2) - x*(sin x) - b*(cos (2*x)) + b
f' x = b*x - sin (x) - x*cos (x) + sin (2*x)

-- The set of initial points (aka: each t0)
-- how many digits of accuracy
-- max number of iterations
startpoints = [pi/2, 5*pi, 10*pi]   -- the first is fine, the second is better, but the last one diverges.
accuracy    = 1e-7                  -- It might be because the function's the form/derivative.
limit       = 7000                  -- what if... we just give it enough time?

-- TODO: move;
-- for string concatenation
_str = showString
_tab = showChar '\t'
_sep = showChar '\t'

-- handy aliases
newtonMethod  = newton limit (accuracy^2) accuracy f f'

main :: IO ()
main = do
    putStrLn "Author: Gabriel P."
    putStrLn "Problem 2.\n"

    putStrLn "Environment:"
    putStrLn . _tab . _str "accuracy: "     . shows accuracy    . _str "\n"
             . _tab . _str "limit: "        . shows limit       . _str "\n"
             . _tab . _str "start points: " . shows startpoints . _str "\n" $ ""
    solution

solution :: IO ()
solution = do

    -- run every test case:
    forM_ (zip [1..] startpoints) (\(ix, x0) -> do
        let results = newtonMethod x0

        putStrLn . _str "[case " . shows ix . _str "]\n"
                 . _tab . _str "Running Newton's method with the point:\n"
                 . _tab . _str "(x0 = " . shows x0 . _str ")" $ ""

        putStrLn . _str "iter" . _sep . _str "x" . _sep . _str "f(x)" . _sep . _str "f'(x)" $ ""

        forM_ (zip [0..] results) (\(i, (x, fx, dist)) -> do
            putStrLn . shows i 
                     . _sep . shows x
                     . _sep . shows fx
                     . _sep . shows (f' x) $ ""
            )

        putStrLn ""
        )

