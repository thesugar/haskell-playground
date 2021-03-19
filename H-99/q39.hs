-- Q38 は省略

{-
(*) A list of prime numbers.

Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

Example in Haskell:

λ> primesR 10 20
[11,13,17,19]
-}

primesR :: Int -> Int -> [Int]
primesR m n = [x | x <- [m..n], isPrime x]
  where
    isPrime num
      | num < 2 = False
      | otherwise = all (\i -> num `mod` i /= 0) [2..floor $ sqrt $ fromIntegral num]

-- 解答
-- isPrime の実装は Q 31 より。3 より大きい整数で素数なのは mod 6 で 1 or (-1) のいずれかしかない。
isPrime' :: (Integral a) => a -> Bool
isPrime' n | n < 4 = n /= 1 
isPrime' n = all ((/=0) . mod n) $ takeWhile (<= m) candidates 
        where candidates = (2:3:[x + i | x <- [6,12..], i <- [-1,1]])
              m = floor . sqrt $ fromIntegral n

primesR' :: Integral a => a -> a -> [a]
primesR' a b | even a    = filter isPrime' [a+1, a+3..b] -- 偶数飛ばし
             | otherwise = filter isPrime' [a, a+2..b] -- 偶数飛ばし

-- If we are challenged to give all primes in the range between a and b we simply take all numbers from a up to b and filter all the primes through.
-- This is good for very narrow ranges as Q.31's isPrime tests numbers by trial division using (up to √b) a memoized primes list produced by sieve of Eratosthenes to which it refers internally. 
-- So it'll be slower, but immediate, testing the numbers one by one.

-- 別解
-- 与えられた範囲が大きいときは、ひとつひとつ試し割りするよりも、素数のリストから、対象外の範囲を取り除けばいい
-- primesTME ってのがすべての素数だとすると
-- primes = primesTME
-- primesR a b = takeWhile (<= b) $ dropWhile (< a) primes

-- エラトステネスの篩
primesR'' :: Integral a => a -> a -> [a]
primesR'' a b = takeWhile (<= b) $ dropWhile (< a) $ sieve [2..]
  where sieve (n:ns) = n:sieve [ m | m <- ns, m `mod` n /= 0 ]
    -- めちゃくちゃきれいだけど効率悪い

-- tree-merging Eratosthenes sieve, primesTME of Q.31, 
-- adjusted to produce primes in a given range (inclusive)
-- primesR''' a b | b < a || b < 2 = []
--                | otherwise      = takeWhile (<= b) $ primesFrom a

-- primesFrom a0 = (if a0 <= 2 then [2] else []) ++ 
--                 (gaps a $ mults $ span (< z) $ tail primesTME)
--   where
--     a = snap (max 3 a0) 3 2
--     z = ceiling $ sqrt $ fromIntegral a + 1       -- p<z => p*p<=a
--     snap v origin step = if r==0 then v else v+(step-r)
--         where r = rem (v-origin) step   -- NB: origin <= v ; else use MOD

--     mults (h,p':t) =                              -- p'>=z => p'*p'>a
--       join union ( [[x,x+s..] | p <- h,           -- heads unordered  
--                             let s=2*p; x=snap a (p*p) s]
--                    ++ [[p'*p',p'*p'+2*p'..]] )
--       `union'` join union' [[p*p,p*p+2*p..] | p <- t]

--     join  f (xs:t)    = f xs (join f (pairs f t))
--     join  f []        = []
--     pairs f (xs:ys:t) = f xs ys : pairs f t
--     pairs f t         = t
--     union' (x:xs) ys  = x : union xs ys           -- `union` of Q.31
--     gaps k xs@(x:t) | k==x  = gaps (k+2) t 
--                     | True  = k : gaps (k+2) xs