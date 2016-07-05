import MathUtils
testConjecture n = 
    let largestSquare = floor. sqrt . (/2) . fromIntegral $ n
        squares = [1 .. largestSquare]
        residues = map ((n-) . (2*) . (^2)) squares
    in testList isPrime residues
main = print . head . filter (not . testConjecture) . filter (not . isPrime) $ [3,5..]