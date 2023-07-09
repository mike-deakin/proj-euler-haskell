{-# LANGUAGE PartialTypeSignatures #-}

module Problem004 where

import Solutions
import Data.Function

solution :: Solution Int Int
solution Nothing = solution (Just 3)
solution (Just n) = (*) <$> [maxn,maxn-1..1] <*> [maxn,maxn-1..1]
    & map show
     & filter isPalindrome
     & map (read :: String -> Int)
     & maximum
    where maxn = 10^n - 1

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome str = str == reverse str

