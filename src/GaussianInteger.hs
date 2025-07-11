{-# LANGUAGE InstanceSigs #-}
module GaussianInteger where

newtype GaussInt a = GaussInt (a, a) deriving Eq

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