main = print . primes $ 2000000

primes :: Int -> Int
primes n = sum . foldl (\acc a -> filter (\x -> x `mod` a /= 0 || x < a^2) acc) [2..n] $ [2.. floor . sqrt . fromIntegral $ n]