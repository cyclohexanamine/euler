import Data.List
import System.Environment
import Math.NumberTheory.Primes.Factorisation
import Math.NumberTheory.Primes.Testing
import MathUtils (testList)
import qualified Data.Set as S

testPrimeGenerating n = not . testList (not . isPrime) . map (\d -> d + quot n d) . S.elems . divisors $ n
s max = foldl' (\sum n -> if testPrimeGenerating n then sum+n else sum) 0 [1..max]
main = getArgs >>= print . s . (10^) . read . head