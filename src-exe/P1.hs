module Main (main) where

import Nonlinear.Newton (newton)
import Control.Monad    (forM_)
import Data.Tuple.Extra (fst3,snd3)

-- Constants for `f` and `f'`
a = 12.15
b = 36.875
c = 7.4
d = 13.2

-- Simplified expressions
leftTerm  t = exp $ -(t^2) + a*t - b
rightTerm t = exp $ -(t^2) + c*t - d

-- The input function and its derivative
f  t = 1 - leftTerm t - rightTerm t
f' t = 1 - (a - 2*t)*(leftTerm t) - (c - 2*t)*(rightTerm t)

-- The set of initial points (aka: each t0)
-- how many digits of accuracy
-- max number of iterations
startpoints = [3.5, 3.6, 3.7, 3.8, 3.9]
accuracy    = 1e-7
limit       = 100

-- TODO: move;
-- for string concatenation
_str = showString
_tab = showChar '\t'
_sep = showChar '\t'

-- handy aliases
similar t0 t1 = abs (t0 - t1) < accuracy
newtonMethod  = newton limit (accuracy^2) accuracy f f'

main :: IO ()
main = do
    putStrLn "Author: Gabriel P."
    putStrLn "Problem 1.\n"

    putStrLn "Environment:"
    putStrLn . _tab . _str "accuracy: "     . shows accuracy    . _str "\n"
             . _tab . _str "limit: "        . shows limit       . _str "\n"
             . _tab . _str "start points: " . shows startpoints . _str "\n" $ ""

    part1
    part2


part1 :: IO ()
part1 = do
    putStrLn "=> Part 1: Testing Starting Points"

    -- run every test case:
    forM_ (zip [1..] startpoints) (\(ix, t0) -> do
        let results = newtonMethod t0

        putStrLn . _str "[case " . shows ix . _str "]\n"
                 . _tab . _str "Running Newton's method with the point:\n"
                 . _tab . _str "(t0 = " . shows t0 . _str ")" $ ""

        putStrLn . _str "iter" . _sep . _str "t" . _sep . _str "f(t)" . _sep . _str "f'(t)" $ ""

        forM_ (zip [0..] results) (\(i, (t, ft, dist)) -> do
            putStrLn . shows i 
                     . _sep . shows t
                     . _sep . shows ft
                     . _sep . shows (f' t) $ ""
            )

        putStrLn ""
        )

part2 :: IO ()
part2 = do
    putStrLn "=> Part 2: Looking for new starting points" 
    
    let fromPart1    = fmap (fst3 . last . newtonMethod) startpoints
        resultsRoots = zip [3, 3.05 .. 4]
                        $  fmap (last . newtonMethod) [3, 3.05 .. 4]

        convergentRoots = filter (similar 0 . snd3 . snd) resultsRoots

        notPreviouslyFound (t0,res) =
                        not . or
                            . fmap (similar . fst3 $ res)
                            $ fromPart1
        rootsFound = filter notPreviouslyFound convergentRoots

    putStrLn . _str "PointID" . _sep . _str "t0"
             . _sep . _str "t" . _sep . _str "f(t)" . _sep . _str "f'(t)" $ ""
    forM_ (zip [0..] rootsFound) (\(i, (t0, (t, ft, dist))) -> do
        putStrLn . shows i 
                 . _sep . shows t0
                 . _sep . shows t
                 . _sep . shows ft
                 . _sep . shows (f' t) $ ""
        )


