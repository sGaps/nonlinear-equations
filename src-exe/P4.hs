module Main (main) where

import Nonlinear.RegulaFalsi (regulaFalsi)
import Control.Monad         (forM_)
import Data.Tuple.Extra      (thd3)

-- prev. l/p3

-- Constants for `f`
a = (exp(1)/3)
b = 0.25

-- The input function. concentration of the given substance in `mL`
--                     original function: c(t) = A * t * exp(-t/3)
c t = a * t*(exp (-t/3)) - b

-- The given range
-- how many digits of accuracy
-- max number of iterations
startrange = (1,20)
accuracy    = 1e-7
limit       = 100

-- TODO: move;
-- for string concatenation
_str = showString
_tab = showChar '\t'
_sep = showChar '\t'
_eol = _str "\n"

-- handy alias
regulaMethod = regulaFalsi limit (accuracy^2) accuracy c

type Iteration a = (a, a, (a,a,a))

main :: IO ()
main = do
    putStrLn "Author: Gabriel P."
    putStrLn "Problem 3.\n"

    putStrLn "Environment:"
    putStrLn . _tab . _str "accuracy: "     . shows accuracy . _eol
             . _tab . _str "limit: "        . shows limit    . _eol
             . _tab . _str "start range [inclusive]: " . shows startrange . _eol $ ""

    putStrLn . _tab . _str "Running Regula Falsi method over the function `c(t)`" . _eol
             . _tab . _str "with the inclusive range:"
             . _tab . shows startrange $ ""

    let iterations          = uncurry regulaMethod startrange
        (last_ix,last_iter) = last . zip [1..] $ iterations
        minutes   = (thd3 . thd3) last_iter
        dosage    = c minutes

    putStrLn . _str "Time in minutes (T):"       . _tab . shows minutes . _eol
             . _str "Dosage in milliliters (d):" . _tab . shows dosage  . _eol
             . _str "After (" . shows last_ix . _str ") iterations" . _eol
             $ ""

    putStrLn . _str "iter"
             . _sep . _str "a"  . _sep  . _str "c(a)"
             . _sep . _str "b"  . _sep  . _str  "c(b)"
             $ ""

    forM_ (zip [0..] iterations) (\(i, (_,_,(_, x_low, x_high))) -> do
        putStrLn . shows i 
                 . _sep . shows x_low  . _sep . shows (c x_low)
                 . _sep . shows x_low  . _sep . shows (c x_low)
                 . _sep . shows x_high . _sep . shows (c x_high)
                 $ ""
        )

