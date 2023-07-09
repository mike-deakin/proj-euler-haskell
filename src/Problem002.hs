module Problem002 (solution) where

import Solutions

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

solution :: Solution Int Int
solution Nothing = solution (Just 4000000)
solution (Just n) = sum $ takeWhile (< n) $ filter even fibs
