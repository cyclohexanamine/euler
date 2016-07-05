import System.Environment
import Data.Char (digitToInt)
import Data.List
import MathUtils (isPrime, divisibleBy)

main = getArgs >>= \args -> print . sum . allSRTHs . floor $ (read (head args) :: Double)

allSRTHs :: Integer -> [Integer]
allSRTHs max =
    let nIterations = ceiling (log (fromIntegral max) / log 10.0) - 1
        rTHarshads = allRTHarshads nIterations
        strongs = filter strong rTHarshads
        candidates = filter (< max) . nextNumbers $ strongs
    in filter isPrime candidates


-- Harshads

allRTHarshads :: Integer -> [Integer]
allRTHarshads nIterations =
    let rTHarshads = genericIndex (iterate nextRTHarshads [(0, 0)]) nIterations
    in map fst rTHarshads

nextRTHarshads :: [(Integer, Integer)] -> [(Integer, Integer)]
nextRTHarshads l =
    let newl = l >>= \(num, digitsum) -> map (\digitN -> (num*10 + digitN, digitsum + digitN)) [0..9]
    in filter harshad newl

harshad :: (Integer, Integer) -> Bool
harshad (0, 0) = True
harshad (num, digitsum) = divisibleBy num digitsum

strong :: Integer -> Bool
strong 0 = False
strong n = isPrime . quot n . sumDigits . show $ n

sumDigits :: String -> Integer
sumDigits = foldl (\sum digit -> sum + fromIntegral (digitToInt digit)) 0

nextNumbers :: [Integer] -> [Integer]
nextNumbers l = l >>= \num -> map (\digitN -> num*10 + digitN) [0..9]