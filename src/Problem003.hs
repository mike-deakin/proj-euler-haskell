module Problem003 (solution) where 

import Solutions
import Data.Numbers.Primes (primeFactors)

solution :: Solution Int Int
solution Nothing = solution (Just 600851475143)
solution (Just n) = last $ primeFactors n
