{-# OPTIONS -Wall -Werror #-}

{-
(*) Determine whether two positive integer numbers are coprime.
Two numbers are coprime if their greatest common divisor equals 1.

Example in Haskell:
λ> coprime 35 64
True
-}

coprime :: Int -> Int -> Bool
coprime n m = myGCD n m == 1

myGCD :: Int -> Int -> Int
myGCD n m
  | m == 0 = abs n
  | otherwise = myGCD m (n `mod` m)


-- 解答
coprime' :: Int -> Int -> Bool
coprime' a b = gcd a b == 1