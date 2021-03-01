-- {-# OPTIONS -Wall -Werror #-}
-- q29, q30 はありません。

-- _/_/_/_/  Arithmetric /_/_/_/_/_/ -- 

{-
(**) Determine whether a given integer number is prime.

Example in Haskell:

λ> isPrime 7
True
-}

-- 素朴な解答
-- n `div` 2 まで、なおかつ、2 乗して n より小さいものまでを調べればよい。
-- （そして、isPrime' の解法でやるように、もっと省略できる）
isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | n == 2 = True
  | otherwise = let factors = filter (\x -> n `mod` x == 0) [2..(n `div` 2)] in if length factors == 0 then True else False

-- check するのは 2 〜 √n まででいいらしい
-- n が合成数なら必ず √n より小さい素因数を持つため。
-- fromIntegral としないとエラーになる。
isPrime' :: Int -> Bool
isPrime' n
  | n < 2 = False
  | n == 2 = True
  | otherwise = let factors = filter (\x -> n `mod` x == 0) [2..(floor $ sqrt (fromIntegral n))] in if length factors == 0 then True else False

-- 解答
-- Well, a natural number k is a prime number if it is larger than 1 and no natural number n >= 2 with n^2 <= k is a divisor of k. 
-- However, we don't actually need to check all natural numbers n <= sqrt k. We need only check the primes p <= sqrt k:
-- 自分の isPrime' と基本的には同じ。ただし、3 より大きい素数は 6 の倍数より 1 大きいか小さいかのどちらか（むろん逆は成り立たない）であるということを利用している。
-- 6k, 6k + 1, 6k+2, ..., 6k + 5 を考えると、6 の倍数より 1 大きいか小さいかではない数が素数になりえないことがわかる
-- https://ameblo.jp/masanori432/entry-10090225881.html
isPrime1 :: (Integral a) => a -> Bool
isPrime1 n | n < 4 = n /= 1 
isPrime1 n = all ((/=0) . mod n) $ takeWhile (<= m) candidates 
        where candidates = (2:3:[x + i | x <- [6,12..], i <- [-1,1]])
              m = floor . sqrt $ fromIntegral n

-- isPrime :: Integral a => a -> Bool
-- isPrime k = k > 1 &&
--    foldr (\p r -> p*p > k || k `rem` p /= 0 && r)
--       True primesTME

-- {-# OPTIONS_GHC -O2 -fno-cse #-}
-- -- tree-merging Eratosthenes sieve
-- --  producing infinite list of all prime numbers
-- primesTME = 2 : gaps 3 (join [[p*p,p*p+2*p..] | p <- primes'])
--   where
--     primes' = 3 : gaps 5 (join [[p*p,p*p+2*p..] | p <- primes'])
--     join  ((x:xs):t)        = x : union xs (join (pairs t))
--     pairs ((x:xs):ys:t)     = (x : union xs ys) : pairs t
--     gaps k xs@(x:t) | k==x  = gaps (k+2) t 
--                     | True  = k : gaps (k+2) xs

-- -- duplicates-removing union of two ordered increasing lists
-- union (x:xs) (y:ys) = case (compare x y) of 
--            LT -> x : union  xs  (y:ys)
--            EQ -> x : union  xs     ys 
--            GT -> y : union (x:xs)  ys

-- 関連
-- 素数列を得る（エラトステネスの篩）
-- https://qiita.com/bra_cat_ket/items/6a99a9b01682886607d0