import System.Environment
import Data.List
import MathUtils

nextPascalRow l = let topRow = [0] ++ l ++ [0]
                  in map (\n -> (topRow !! n) + (topRow !! (n+1))) [0 .. length l]
pascalNumbers n = nub . concat . take n . iterate nextPascalRow $ [1]

squareFree x = let pf = primeFactors x in length (nub pf) == length pf

main = getArgs >>= print . sum . filter squareFree . pascalNumbers . read . head
