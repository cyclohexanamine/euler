{- 
   s(n) is the smallest number m such that n divides m!
   sumS(n) is the sum of s(i)s from 2 to n.
   
   s(n) is determinable from the prime factorisation of n:
    For each a^i, we find the smallest number F such that there are i factors of a in F!, and the largest F is s(n).
    The Fs are found by iteration through Legendre's formula.
 -}
 
import Data.List
import Data.Digits
import Math.NumberTheory.Primes.Factorisation


sp p = sum . digits p

findF :: Integer -> Integer -> Integer
findF a i | a*i <= a^2 = a*i
findF a i = let ip = i*(a-1)
                iters = iterate (\m -> ip + (sp a m)) $ (ip)
            in case find (\m -> m >= ip + (sp a m)) iters of Just m -> last . takeWhile (\m -> m >= ip + (sp a m)) . reverse $ [1..m]

s n =     
 -- let primes = group . sort . primeFactors $ n
     -- facs = zipWith (findF) (map head primes) . map genericLength $ primes
    let facs = map (\(a, i) -> findF a . fromIntegral $ i) . factorise $ n
    in maximum facs
    
sumS n = foldl' (\sum n -> sum + s n) 0 [2..n]

main = print $ sumS (10^8)