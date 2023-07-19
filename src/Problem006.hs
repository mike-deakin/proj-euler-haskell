module Problem006 (solution, directSolution) where

import Solutions (Solution)

pow2 :: Int -> Int
pow2 = flip (^) (2 :: Integer)

solution :: Solution Int Int
solution Nothing = solution (Just 100)
solution (Just n) = sumSq - sqSum
  where
    sumSq = pow2 $ sum [1 .. n]
    sqSum  = sum $ pow2 <$> [1 .. n]

directSolution :: Solution Int Int
directSolution Nothing = directSolution (Just 100)
directSolution (Just n) = sqSum - sumSq
  where
    sqSum = pow2 ((n * (n + 1)) `div` 2)
    sumSq = (n * (2 * n + 1) * (n + 1)) `div` 6
