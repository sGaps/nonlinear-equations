module Main (main) where

import Nonlinear.Secant      (secant)
import Nonlinear.RegulaFalsi (regulaFalsi)
import Control.Monad         (forM_)

-- Constants for `f` and `f'`
a = 2
b = 34/7
c = 209/49
d = 173/343

-- The input function
f x = a*(x^3) - b*(x^2) + c*x - d

-- The given range
-- how many digits of accuracy
-- max number of iterations
startrange = (-1.0,1.0)
accuracy    = 1e-7
limit       = 100

-- TODO: move;
-- for string concatenation
_str = showString
_tab = showChar '\t'
_sep = showChar '\t'

-- handy aliases
type Iteration a = (a, a, (a,a,a))
regulaMethod = regulaFalsi limit (accuracy^2) accuracy f
secantMethod = secant      limit (accuracy^2) accuracy f


main :: IO ()
main = do
    putStrLn "Author: Gabriel P."
    putStrLn "Problem 3.\n"

    putStrLn "Environment:"
    putStrLn . _tab . _str "accuracy: "     . shows accuracy    . _str "\n"
             . _tab . _str "limit: "        . shows limit       . _str "\n"
             . _tab . _str "start range [inclusive]: " . shows startrange . _str "\n" $ ""

    runMethod "Running Secant method"       secantMethod
    runMethod "Running Regula Falsi method" regulaMethod

runMethod :: String -> (Float -> Float -> [Iteration Float]) -> IO ()
runMethod title method = do
    let results = uncurry method startrange

    putStrLn . _tab . _str title . _str "\n"
             . _tab . _str "with the inclusive range:"
             . _tab . shows startrange $ ""

    putStrLn . _str "iter"
             . _sep . _str "x_{i-1}"  . _sep . _str "f(x_{i-1})"
             . _sep . _str "x_i"      . _sep . _str "f(x_i)"
             . _sep . _str "x_{i+1}*" . _sep . _str "f(x_{i+1}*)"
             $ ""

    forM_ (zip [0..] results) (\(i, (dist,f_last,(x_prev, x_curr, x_next))) -> do
        putStrLn . shows i 
                 . _sep . shows x_prev . _sep . shows (f x_prev)
                 . _sep . shows x_curr . _sep . shows (f x_curr)
                 . _sep . shows x_next . _sep . shows (f x_next)
                 $ ""
        )

    putStrLn ""

