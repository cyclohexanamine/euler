import Data.List (findIndex)
import Data.Maybe (fromJust)

fibPair (a, b) = (b, a+b)
sizeIndex = (+1) . fromJust . findIndex (\(a, b) -> a > 10^999) . iterate fibPair $ (1, 1)
main = print sizeIndex

fibN :: Int -> Integer
fibN n = fst (iterate fibPair (1, 1) !! (n-1))