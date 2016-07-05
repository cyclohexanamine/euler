import Data.List

main = print $ s 5

s :: Integral a => Int -> a
s n = sum . map toNum $ uniques
    where toNum str = sum . zipWith (\num digit -> 2^num * if digit == '1' then 1 else 0) [0..] . reverse $ str
          uniques = map concatSubs . filterFinal n $ sequences
          sequences = (iterate filterNextSubs [[take n . repeat $ '0']]) !! (2^n - n)
          
          
nextSubs :: [String] -> [[String]]
nextSubs strList = fmap (\digit -> strList ++ [(tail . last $ strList) ++ digit]) ["0", "1"]

uniqueSubs :: [String] -> Bool
uniqueSubs strList = (length strList) == (length . nub $ strList)

filterNextSubs :: [[String]] -> [[String]]
filterNextSubs = filter uniqueSubs . concat . map nextSubs

filterFinal :: Int -> [[String]] -> [[String]]
filterFinal n strs = filter (\strList -> uniqueSubs . subSeqN n $ (last strList ++ head strList)) strs

subSeqN :: Int -> [a] -> [[a]]
subSeqN n l = takeWhile ((==n).length) . map (\x -> take n . drop x $ l) $ [0..]

concatSubs :: [String] -> String
concatSubs strList = head strList ++ map last (drop 1 strList)