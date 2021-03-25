{-
(**) Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

In most cases, if an even number is written as the sum of two prime numbers, one of them is very small.
Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.

Example in Haskell:

λ> goldbachList 9 20
[(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
λ> goldbachList' 4 2000 50
[(73,919),(61,1321),(67,1789),(61,1867)]
-}

-- q40 から
goldbach :: Int -> (Int, Int)
goldbach n
  | odd n || n <= 2 = error "argument must be an even number greater than 2."
  | n == 4 = (2, 2)
  | otherwise = helper 3 (n-3)
      where
        helper p q = if p + q == n && isPrime p && isPrime q then (p, q) else helper (p + 2) (q - 2)
        candidates = (2:3:[x + i | x <- [6,12..], i <- [-1, 1]])
        isPrime m | m < 4 = m /= 1
        isPrime m = all ((/= 0) . mod m) $ takeWhile (<= (floor . sqrt $ fromIntegral m)) candidates

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList n m = let start = max (((n+1) `div` 2)*2) 4 -- ❌ max ... 4 とするの忘れてた。これをしないと goldbachList 2 20 などとしたとき（第一引数が 2 以下の場合）エラーになって結果が得られない。
                       -- end   = (m `div` 2)*2 -- [start, start+2 ..] と 2 ずつ足していくんだから、これいらない！よしなにやってくれる。
                   in  map goldbach [start, start+2 .. m]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' n m threshold = filter (\(x, y) -> x > threshold && y > threshold) $ goldbachList n m

-- 解答
goldbachList1 lb ub = map goldbach $ [even_lb,even_lb+2..ub]
  where even_lb = max ((lb+1) `div` 2 * 2) 4 -- 自分の解答でもそうしたけど、パリティで場合分けせずにどっちも (n + 1) `div` 2 * 2 で統一的に扱うのは美しいよね
goldbachList1' lb ub mv = filter (\(a,b) -> a > mv && b > mv) $
                         goldbachList1 lb ub

-- 別解
goldbachList2  n m   = map goldbach $ dropWhile (<4) $ filter even [n..m] -- (n+1) `div` 2 の美しさもあるけど、filter even [n..m] はめちゃくちゃ簡潔だな。。`div` を使うよりもさらに関数型という気がする
goldbachList2' n m i = filter (\(x,y) -> x > i && y > i) $ goldbachList2 n m