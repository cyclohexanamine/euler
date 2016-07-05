module MathUtils 
( testList, diffList
, divisibleBy, divisByList
, expMod
, erasSieve, isPrime, nextPrime
, isSquare, primeFactors, divisors,
) where

import Data.List
import Data.Maybe (fromJust)


testList :: (a -> Bool) -> [a] -> Bool
testList f l = (/= length l) . length . takeWhile (not . f) $ l

diffList :: Num a => [a] -> [a]
diffList l = drop 1 . zipWith (-) l $ (0:l)


-- Divisibility

divisibleBy :: Integer -> Integer -> Bool
divisibleBy a b = a `mod` b == 0

divisByList :: Integer -> [Integer] -> Bool
divisByList a l = testList (divisibleBy a) l

dropN :: Integer -> Integer -> [Integer] -> [Integer]
dropN min n l = let lists = splitAt (fromJust . findIndex (>=min) $ l) l
                in  fst lists ++ (filter (\x -> x `mod` n /= 0) . snd $ lists)


-- Modular

rExpMod b e m c e'
    | e' >= e   = c
    | otherwise = rExpMod b e m ((b * c) `mod` m) (e'+1)

expMod b e m = rExpMod b e m 1 0
                
                
-- Primality

lowPrimeBound = 7

lowPrimes = erasSieve lowPrimeBound
lowPrimeProduct = product $! lowPrimes
highPrimes = [1] ++ ((erasSieve . ceiling . (/2) . fromIntegral $ lowPrimeProduct) \\ lowPrimes)
primeAdditions = nubBy (\a b -> let (x, y) = (min a b, max a b) in lowPrimeProduct+x == y) $! (map negate . reverse $ highPrimes) ++ highPrimes

isPrime :: Integer -> Bool
isPrime n
    | n == 1 = False
    | n `elem` lowPrimes = True
    | n `divisByList` lowPrimes = False
    | otherwise = let rtN = ceiling . sqrt . fromIntegral $ n
                      divisors = takeWhile (<=rtN) . dropWhile (<=1) . concat . map (\a -> map (\b -> lowPrimeProduct*a+b) primeAdditions) $ [0..]
                  in not (n `divisByList` divisors)


erasSieve' :: Integer -> [Integer] -> [Integer]
erasSieve' n l
    | n^2 > last l = l
    | otherwise    = let sieved = dropN (n^2) n l
                         nextN = head . dropWhile (<= n) $ sieved
                     in erasSieve' nextN sieved

erasSieve :: Integer -> [Integer]
erasSieve max = erasSieve' 2 [2..max]
       
nextPrime :: Integer -> Integer
nextPrime 2 = 3
nextPrime n = fromJust . find isPrime . map (n+) $ [2,4..]


-- Factorisation

isSquare :: Integer -> Bool
isSquare x = let x' = truncate $ sqrt (fromIntegral x :: Double) in x'*x' == x

numFactor :: Integer -> Integer -> Integer 
numFactor n a
    | n `mod` a == 0    = (+1) . numFactor (n `quot` a) $ a
    | otherwise         = 0
       
nextPrimeFactor :: Integer -> Integer -> [Integer]
nextPrimeFactor n largest
    | n `mod` largest == 0  = let nextN = (n `quot` largest) in if isPrime nextN then largest:[nextN] else largest:(nextPrimeFactor nextN largest)
    | largest > n           = []
    | otherwise             = nextPrimeFactor n (nextPrime largest)
    
primeFactors :: Integer -> [Integer]
primeFactors n
    | isPrime n = [n]
    | otherwise = nextPrimeFactor n 2

divisors :: Integer -> [Integer]
divisors n = nub . map product . subsequences . primeFactors $ n