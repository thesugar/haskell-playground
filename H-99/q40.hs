{-
(**) Goldbach's conjecture.

Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. 
Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. 
It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). 
Write a predicate to find the two prime numbers that sum up to a given even integer.

Example:

Example in Haskell:

λ> goldbach 28
(5, 23)
-}

goldbach :: Int -> (Int, Int)
goldbach n
  | odd n || n <= 2 = error "argument must be an even number greater than 2."
  | n == 4 = (2, 2) -- 4 以外の場合は、(2, ○○) という結果が返ることはない（○○は必ず偶数になるから、素数ではありえない）。一方、4 の場合だけは (2, 2) という結果になる。それ以外の場合は、下の行のようにどっちも奇数として話を進めていい。
  | otherwise = helper 3 (n-3)
      where
        helper p q = if 
          p + q == n && isPrime p && isPrime q 
          then (p, q) else helper (p + 2) (q - 2)
              where
                isPrime m | m < 4 = m /= 1
                isPrime m = all ((/= 0) . mod m) $ takeWhile (<= (floor . sqrt $ fromIntegral m)) candidates
                  where
                    candidates = (2:3:[x + i | x <- [6,12..], i <- [-1, 1]])

-- isPrime 関数について
isPrime m | m < 4 = m /= 1
isPrime m = all ((/= 0) . mod m) $ takeWhile (<= (floor . sqrt $ fromIntegral m)) candidates
  where
    candidates = (2:3:[x + i | x <- [6,12..], i <- [-1, 1]]) -- i <- [-1, 1] とせずに i <- [1, -1] とすると、2:3:[7,5,11,9,..] のような形になって takeWhile したときに [2,3,7] みたいな形で取り出される
                                                             -- （この場合 5 で試し割りするのを skip してしまう）のでバグをうむ。注意。

-- 解答
goldbach' :: Int -> (Int, Int)
goldbach' a = head $
                  filter (\(x,y) -> isPrime x && isPrime y) $
                  map (\e -> (e, a - e)) [3..a `div` 2]
 where
    factors a = filter (isFactor a) [2..a-1]
    isFactor a b = a `mod` b == 0
    isPrime a = null $ factors a

-- using the previous problem.. (primesR)
-- primesR は与えられた範囲内に存在する素数をリストで返す関数
goldbach'' :: Int -> (Int, Int)
goldbach'' n = head [(x,y) | x <- pr, y <- pr, x+y==n]
    where 
      pr = primesR 2 (n-2)
      primesR :: Int -> Int -> [Int]
      primesR m n = [x | x <- [m..n], isPrime x]
        where
          isPrime num
            | num < 2 = False
            | otherwise = all (\i -> num `mod` i /= 0) [2..floor $ sqrt $ fromIntegral num]

goldbach''' :: Int -> [(Int, Int)]
goldbach''' n = [(x,y) | x <- primesR 2 (n-2), let y = n-x, isPrime y]
  where
      primesR m n = [x | x <- [m..n], isPrime x]
        where
          isPrime num
            | num < 2 = False
            | otherwise = all (\i -> num `mod` i /= 0) [2..floor $ sqrt $ fromIntegral num]