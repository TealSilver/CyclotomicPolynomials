{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module NumberTheory where

isFactorOf :: Integral a => a -> a -> Bool
isFactorOf a n = n `mod` a == 0

primes :: Integral a => [a]
primes = sieve [2..]
    where
    sieve (p:ns) = p : sieve [n | n <- ns, not $ p `isFactorOf` n]

-- integer square root
isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral

isPrime :: Integral a => a -> Bool
isPrime n = (abs n > 1) && null [ a | a <- [2..isqrt (abs n)], a `isFactorOf` abs n]

primeFactors :: Integral a => a -> [a]
primeFactors n
    | n < 2     = []
    | isPrime n = [n]
    | otherwise = firstFactor n 2 : primeFactors (n `div` firstFactor n 2)
    where
    firstFactor m p = if p `isFactorOf` m then p else firstFactor m (p + 1)

distinctPrimeFactors :: Integral a => a -> [a]
distinctPrimeFactors n = dpf n 1
    where
    dpf m l 
        | m < 2                = []
        | isPrime m && m == l  = []
        | isPrime m            = [m]
        | firstFactor m 2 == l = dpf (m `div` l) l
        | otherwise            = firstFactor n 2 : dpf (m `div` firstFactor n 2) (firstFactor n 2)
    firstFactor m' p = if p `isFactorOf` m' then p else firstFactor m' (p + 1)

factors :: Integral a => a -> [a]
factors n = factors' 1
    where
    factors' count
        | count == n = [n]
        | count `isFactorOf` n = count : factors' (count + 1)
        | otherwise = factors' (count + 1)

-- finds if a number is twice a prime
is2prime :: Integral a => a -> Bool
is2prime n = even n && isPrime (n `div` 2)

isPrimePower :: Integral a => a -> Bool
isPrimePower n = n >= 2 && all (== head (primeFactors n)) (primeFactors n)

-- first prime omega function, counts distinct prime factors
littleOmega :: Integral a => a -> Int
littleOmega = length . distinctPrimeFactors 

-- second prime omega function, counts prime factors with multiplicity
bigOmega :: Integral a => a -> Int
bigOmega = length . primeFactors

-- Liouville lambda function, 1 for even number of prime factors, -1 otherwise
liouville :: (Integral a, Num b) => a -> b
liouville n = if (even . bigOmega) n then 1 else -1

-- Mobius function: 1 if n = 1, (-1)^k if n factors into k distinct primes, 0 if some square > 1 divides n
mobius :: (Integral a, Num b) => a -> b
mobius n = if littleOmega n == bigOmega n then liouville n else 0

-- Mertens function
mertens :: (Integral a, Num b) => a -> b
mertens n = sum $ map mobius [1..n]

-- Euler's totient function, counting the number of relative primes of n up to n
totient :: (Integral a, Num b) => a -> b
totient n = 1 + totient' 2
    where
    totient' k 
        | k == n       = 0
        | gcd k n == 1 = 1 + totient' (k + 1)
        | otherwise    = totient' (k + 1)

cototient :: (Integral a, Num b) => a -> b
cototient n = cototient' 2
    where
    cototient' k
        | k == n       = 1
        | gcd k n == 1 = cototient' (k + 1)
        | otherwise    = 1 + cototient' (k + 1)

-- sum of proper divisors, only works on strictly positive integers
aliquotSum :: Integral a => a -> a
aliquotSum n 
    | n < 1     = undefined
    | n == 1    = 0
    | otherwise = sum [k | k <- [2..n - 1], k `isFactorOf` n]

isDeficient :: Integral a => a -> Bool
isDeficient n = n > aliquotSum n

isAbundant :: Integral a => a -> Bool
isAbundant n = n < aliquotSum n

isPerfect :: Integral a => a -> Bool
isPerfect n = n == aliquotSum n
