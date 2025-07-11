{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Factorize where

isFactorOf :: Integral a => a -> a -> Bool
isFactorOf a n = n `mod` a == 0

primes :: Integral a => [a]
primes = sieve [2..]
    where
    sieve (p:ns) = p : sieve [n | n <- ns, not $ p `isFactorOf` n]

isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral

isPrime :: Integral a => a -> Bool
isPrime n = (n > 1) && null [ a | a <- [2..isqrt n], a `isFactorOf` n]

primeFactors :: Integral a => a -> [a]
primeFactors n
    | n < 2     = []
    | isPrime n = [n]
    | otherwise = firstFactor n 2 : primeFactors (n `div` firstFactor n 2)
    where
    firstFactor m p = if p `isFactorOf` m then p else firstFactor m (p + 1)

factors :: Integral a => a -> [a]
factors n = factors' 1
    where
    factors' count
        | count == n = [n]
        | count `isFactorOf` n = count : factors' (count + 1)
        | otherwise = factors' (count + 1)

is2prime :: Integral a => a -> Bool
is2prime n = even n && isPrime (n `div` 2)

isPrimePower :: Integral a => a -> Bool
isPrimePower n = n >= 2 && all (== head (primeFactors n)) (primeFactors n)
