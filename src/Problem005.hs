module Problem005 where

import Solutions

divisors :: [Int]
divisors = [11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

dividesAll :: Int -> Bool
dividesAll n = all (\x -> n `rem` x == 0) divisors

solution :: Solution () Int
solution _ = head $ filter dividesAll [20, 40 .. ]
