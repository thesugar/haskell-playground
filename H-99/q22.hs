{-# OPTIONS -Wall -Werror #-}

{-
       Create a list containing all integers within a given range.

       Example in Haskell:

       λ> range 4 9
       [4,5,6,7,8,9]
-}

range :: Int -> Int -> [Int]
range a b = [a..b]

range' :: Int -> Int -> [Int]
range' a b
       | a < b = a: range' (a+1) b
       | a == b = [b]
       | otherwise = error "The first index must be larger than the second one."

range'' :: Int -> Int -> [Int]
range'' a b = reverse $ reversedRange a b
       where reversedRange a' b'
              | a' < b' = b': (reversedRange a' (b'-1))
              | a' == b' = [a']
              | otherwise = error "The first index must be larger than the second one."

--- 解答
-- 自分の解答 range と同じ
range1 :: Int -> Int -> [Int]
range1 x y = [x..y]

-- enumFromTo 関数なんてあるんだ　他にも enumFrom 関数や enumFromThen、enumFromThenTo などいろいろある
range2 :: Int -> Int -> [Int]
range2 = enumFromTo

-- iterate 関数も便利そう　iterate :: (a -> a) -> a -> [a]
range3 :: Int -> Int -> [Int]
range3 x y = take (y-x+1) $ iterate (+1) x

-- start > stop なる引数がきたら逆順のリストを返すようにしている。それ以外は自分の解答 range' と同じ
range4 :: Int -> Int -> [Int]
range4 start stop
       | start > stop  = reverse (range stop start)
       | start == stop = [stop]
       | otherwise  = start:range (start+1) stop -- start < stop

-- これも、第一引数が第二引数より大きい場合には逆順のリストを返すようにしている。range4 と同じ。ただし reverse 関数は使わない実装
range5 :: Int -> Int -> [Int]
range5 n m
    | n == m = [n]
    | n < m = n:(range (n+1) m)
    | otherwise = n:(range (n-1) m) -- n > m

-- A generic and shorter version of the above
-- お〜いいね
range6 :: (Ord a, Enum a) => a -> a -> [a]
range6 a b | (a == b) = [a]
range6 a b = a:range6 ((if a < b then succ else pred) a) b

-- with scan
-- scan は fold と同じ処理をするが、アキュムレータの中間状態をすべて返すもの。
range7 :: Int -> Int -> [Int]
range7 l r = scanl (+) l (replicate (abs(r - l)) (if r > l then 1 else -1))

-- range7 とほぼ同じ。
-- 引数の大小に応じて演算子を + か - か変えるようにしている。
range8 :: Int -> Int -> [Int]
range8 l r = scanl op l $ replicate diff 1
  where
  op = if l < r then (+) else (-)
  diff = abs $ l - r