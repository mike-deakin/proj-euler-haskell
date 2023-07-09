module Problem001 (solution) where

import Solutions

fizzBuzz :: Int -> Bool
fizzBuzz n = 
    n `rem` 3 == 0 || n `rem` 5 == 0

solution :: Solution Int Int
solution Nothing = solution (Just 1000)
solution (Just n) = sum (filter fizzBuzz [1..n-1])
