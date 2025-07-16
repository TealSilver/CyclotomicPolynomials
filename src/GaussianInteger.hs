{-# LANGUAGE InstanceSigs #-}
module GaussianInteger where

import Factorize

newtype GaussInt a = GaussInt (a, a) deriving Eq

i :: Integral a => GaussInt a
i = GaussInt (0, 1)

re :: GaussInt a -> a
re (GaussInt (a, _)) = a

im :: GaussInt a -> a
im (GaussInt (_, b)) = b

conjugate :: Num a => GaussInt a -> GaussInt a
conjugate (GaussInt (a, b)) = GaussInt (a, -b)

norm :: Num a => GaussInt a -> a
norm (GaussInt (a, b)) = (a * a) + (b * b)

isGaussPrime :: Integral a => GaussInt a -> Bool
isGaussPrime z@(GaussInt (a, b)) = case (a, b) of
    (_, 0) -> isPrime a && abs a `mod` 4 == 3
    (0, _) -> isPrime b && abs b `mod` 4 == 3
    (_, _) -> isPrime $ norm z

instance (Num a) => Num (GaussInt a) where
    (+) :: Num a => GaussInt a -> GaussInt a -> GaussInt a
    (+) (GaussInt (a, b)) (GaussInt (c, d)) = GaussInt (a + c, b + d)

    (*) :: Num a => GaussInt a -> GaussInt a -> GaussInt a
    (*) (GaussInt (a, b)) (GaussInt (c, d)) = GaussInt ((a * c) - (b * d), (a * d) + (b * c))

    negate :: Num a => GaussInt a -> GaussInt a
    negate (GaussInt (a, b)) = GaussInt (-a, -b)

    fromInteger :: Num a => Integer -> GaussInt a
    fromInteger a = GaussInt (fromInteger a, 0)

    abs :: Num a => GaussInt a -> GaussInt a
    abs _ = undefined

    signum :: Num a => GaussInt a -> GaussInt a
    signum _ = undefined

instance (Show a, Num a, Ord a) => Show (GaussInt a) where
    show :: Show a => GaussInt a -> String
    show (GaussInt (a, b)) = if b >= 0
                            then show a ++ " + " ++ show b ++ "i"
                            else show a ++ " - " ++ tail (show b) ++ "i"