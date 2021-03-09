{-# OPTIONS -Wall -Werror #-}
import Data.List (group)

{-
(**) Determine the prime factors of a given positive integer.

Construct a list containing the prime factors and their multiplicity.

Example in Haskell:

λ> prime_factors_mult 315
[(3,2),(5,1),(7,1)]
-}

-- OK! Q35 とほぼ同じで、タプルの計算だけ付け加えている。
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = helper n 2
  where 
    helper 1 _ = []
    helper num f 
      | f*f > num = [(num, 1)]
      | otherwise
        = if num `mod` f == 0
            then f `addTup` (helper (num `div` f) f)
            else helper num (f+1)
        where
          x `addTup` [] = [(x, 1)]
          x `addTup` tups@((z,w):ls)
            | x == z = (z, w+1):ls
            | otherwise = (x, 1):tups

-- 解答
prime_factors_mult' :: Int -> [(Int, Int)]
prime_factors_mult' n = map swap $ encode $ primeFactors n
  where swap (x,y) = (y,x)

-- q35 より
primeFactors :: Int -> [Int]
primeFactors n = primeFactorsHelper' n 2
  where
    primeFactorsHelper' num f
      | f * f > num      = [num] 
      | num `mod` f == 0 = f : primeFactorsHelper' (num `div` f) f
      | otherwise        = primeFactorsHelper' num (f + 1)

-- q10 より
-- import Data.List (group)
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) $ group xs

-- 別解（書き方が違うだけでほぼ同じ）
prime_factors_mult' :: Int -> [Int]
prime_factors_mult' = map encode . group . primeFactors
  where encode xs = (head xs, length xs)