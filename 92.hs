import Data.Digits

chain :: Integer -> [Integer]
chain 1 = [1]
chain 89 = [89]
chain n = chain . sum . map (^2) . digits 10 $ n
end89 = (==89) . last . chain

-- main = print . length . filter end89 $ [1..10^7-1]


-- Faster

maxCached = 1000
chains = map chain' [0..maxCached]
chain' 1 = False
chain' 89 = True
chain' n = chain' . sum . map (^2) . digits 10 $ n

chain'' n
    | n <= maxCached = chains !! (fromIntegral n)
    | otherwise      = chain'' . sum . map (^2) . digits 10 $ n
    
main = print . length . filter chain'' $ [1..10^7-1]