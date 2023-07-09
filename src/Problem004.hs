{-# LANGUAGE PartialTypeSignatures #-}

module Problem004 where

import Solutions
import Data.Function
import Control.Applicative

naiveSoln :: Int -> Int
naiveSoln n = liftA2 (*) [maxn,maxn-1..100] [maxn,maxn-1..100]
    & map show
     & filter isPalindrome
     & map (read :: String -> Int)
     & maximum
    where maxn = 10^n - 1

optimisedSoln :: Int -> Int
optimisedSoln 3 =
    [x*y | x <- [990::Int,979..110], y <- [999,998..x]]
    & map show
    & filter isPalindrome
    & map (read :: String -> Int)
    & maximum
optimisedSoln _ = error "Only optimised for n = 3 case!"

solution :: Solution Int Int
solution Nothing = optimisedSoln 3
solution (Just n) = naiveSoln n

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome str = str == reverse str

