import Polynomial
import Factorize

cycloPolyGen :: (Integral a) => Int -> Polynomial a
cycloPolyGen n
    | n == 1                    = listPoly [-1, 1]
    | isPrime n                 = listPoly (replicate n 1)
    | even n && odd (n `div` 2) = polyEval (cycloPolyGen (n `div` 2)) (-1, 1)
    | isPrimePower n            = polyEval (cycloPolyGen (head $ primeFactors n)) (1, head (primeFactors n) ^ (length (primeFactors n) - 1))
    | otherwise                 = polyDivInt (listPoly $ -1 : replicate (n - 1) 0 ++ [1]) (product [cycloPolyGen d | d <- init (factors n)])