

{-
(**) Calculate Euler's totient function phi(m) (improved).

See problem 34 for the definition of Euler's totient function. 
If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be efficiently calculated as follows: 
Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:

phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
         (p2 - 1) * p2 ** (m2 - 1) * 
         (p3 - 1) * p3 ** (m3 - 1) * ...

Note that a ** b stands for the b'th power of a.
-}

-- 確認
-- Euler の totient 関数:
-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

-- problem 36 でつくったものは以下のような素因数分解（の指数表現）。
-- λ> prime_factors_mult 315
-- [(3,2),(5,1),(7,1)]

-- q36
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

phi :: Int -> Int
phi m = foldr 
          (\(p, m) acc -> (p-1) * (p ^ (m-1)) * acc) -- Int の冪乗は ^ を使えばいい。「**」を使うと Floating が出てきてややこしくなる。
          1 $ prime_factors_mult m

-- 解答
-- リスト内包表現使って product 関数でリストの積を求める（きれい）。
totient :: Int -> Int
totient m = product [(p - 1) * p ^ (c - 1) | (p, c) <- prime_factors_mult m]
