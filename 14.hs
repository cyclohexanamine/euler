import Data.Function
import Data.List

collatz n
    | n == 1 = n : []
    | even n = n : collatz (quot n 2)
    | odd n  = n : collatz (3*n + 1)
    
main = print . fst . foldl' (\(n, len) (a, lena) -> if lena > len then (a, lena) else (n, len)) (0, 0) . map (\n -> (n, length . collatz $ n)) $ [1..10^6-1]