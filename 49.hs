import MathUtils
import Data.List
import Data.Maybe

otherPerms :: Integer -> [Integer]
otherPerms n = sort . nub . map read .  permutations . show $ n

-- possibleSequences :: [Integer] -> [[Integer]]
-- possibleSequences l = 
    -- let seqs = filter ((==3) . length) . subsequences $ l
        -- seqDiffs = zip seqs (map diffList seqs) 
    -- in map fst . filter ((==1) . length . nub . snd) $ seqDiffs

possibleSequences :: [Integer] -> [[Integer]]
possibleSequences l = foldl (\acc (n, pos) -> let seqN = 
                                                   foldl (\acc next -> let next2 = next+next-n in if next2 `elem` l then [n,next,next2]:acc else acc) [] (drop pos l)
                                              in if seqN /= [] then seqN++acc else acc) 
                       [] (zip l [1..])
    
findSequence :: Integer -> Maybe [[Integer]]
findSequence n = 
    let perms = otherPerms n
        seqs = possibleSequences perms
        primeSeqs = filter (not . testList (not . isPrime)) seqs
    in if primeSeqs == [] then Nothing else Just primeSeqs
    
main = print . nub . concat . map fromJust . filter isJust . map findSequence $ [1000..9999]