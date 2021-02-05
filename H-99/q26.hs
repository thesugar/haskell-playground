{-# OPTIONS -Wall -Werror #-}

{-
(**) Generate the combinations of K distinct objects chosen from the N elements of a list

In how many ways can a committee of 3 be chosen from a group of 12 people? 
We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). 
For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

Example:

Example in Haskell:

λ> combinations 3 "abcdef"
["abc","abd","abe",...]
-}

import System.Random
import Data.List (tails, subsequences, sort, group)

-- できた！と思ったけどこれ違うぞ。
-- ランダムに 3 人ピックアップするのではなく、組み合わせを列挙しないといけないのだから。
combinations :: Int -> [a] -> IO [a]
combinations _ [] = return []
combinations 0 _ = return []
combinations n (x:xs) = do
   r <- randomRIO (0, (length xs))
   if r < n
      then do
         rest <- combinations (n-1) xs
         return (x:rest)
      else combinations n xs

--- 解答

-- Import the 'tails' function
--   > tails [0,1,2,3]
--   [[0,1,2,3],[1,2,3],[2,3],[3],[]]
-- import Data.List (tails)

-- The implementation first checks if there's no more elements to select,
-- if so, there's only one possible combination, the empty one,
-- otherwise we need to select 'n' elements. Since we don't want to
-- select an element twice, and we want to select elements in order, to
-- avoid combinations which only differ in ordering, we skip some
-- unspecified initial elements with 'tails', and select the next element,
-- also recursively selecting the next 'n-1' element from the rest of the
-- tail, finally consing them together

-- Using list comprehensions
-- 乱数は使ってない。ランダムに 3 人選ぶのではなく、可能な組み合わせを列挙するんだから乱数不要。
combinations1 :: Int -> [a] -> [[a]]
combinations1 0 _ = [[]]
combinations1 n xs = [y:ys | y:xs' <- tails xs, ys <- combinations1 (n-1) xs']
   -- 順番が違うだけで実質同じ組み合わせを取らないように tails を使っている。

-- Alternate syntax, using 'do'-notation 
combinations2 :: Int -> [a] -> [[a]]
combinations2 0 _  = return []
combinations2 n xs = do
                  y:xs' <- tails xs
                  ys <- combinations2 (n-1) xs'
                  return (y:ys)

-- tails を使わず、Prelude の関数（drop）を使う
combinations3 :: Int -> [a] -> [[a]]
combinations3 0 _ = [[]]
combinations3 n xs = [ (xs !! i) : x | i <- [0..(length xs)-1], x <- combinations3 (n-1) (drop (i+1) xs)]


combinations4 :: Int -> [a] -> [[a]]
combinations4 0 _ = [[]] -- 0 個の要素を選択する場合は必ず空リストだけを含むリストを返す
combinations4 _ [] = [[]] -- このベースケースは本当は不要だが、 non-exhaustive patter matches の warning を回避するために書く
combinations4 k (x:xs) = x_start ++ others -- 結果として出力したいリストは、「すべての組み合わせ」（["abc", "abd", ...]）であって、そのうちの一つの組み合わせ（"abc" など）ではないことに注意。
                                           -- だから、この書き方だと「絶対 x が最初にくるじゃん」と思うかもしれないけど、すべてのありうるリストを列挙するのだから問題ない。
   where x_start = [ x:rest | rest <- combinations4 (k-1) xs] -- x から始まる組み合わせをすべて取得し、再帰的に、残りの xs から (k-1) 個を取り出す
         others = if k <= length xs then combinations4 k xs else [] -- x から始まるパターンをやり尽くしたら、combinations 関数に引数として渡すリストを xs にして（最初の要素を捨てて）、再帰を下ろしている

-- これも書き方が違うだけで combinations4 と同じ
combinations5 :: Int -> [a] -> [[a]]
combinations5 0 _ = [[]]
combinations5 _ [] = []
combinations5 n (x:xs) = (map (x:) (combinations5 (n-1) xs)) ++ (combinations5 n xs)

-- Data.List の subsequences 関数を使って、そのサブシーケンスのうち長さが所望の整数値と等しいものだけ取り出す。チートっぽい。
-- subsequences "abc" = ["","a","b","ab","c","ac","bc","abc"]
combinations6 :: Int -> [a] -> [[a]]
combinations6 k ns = filter ((k==) . length) (subsequences ns)


-- let's try it with combinations 2 [1,2,3]
combinations7 :: (Ord a) => Int -> [a] -> [[a]]
combinations7 n xs = compressed
    where
          -- create all combinations (multiple instances, permutations allowed)
          -- answer : [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
          combinations' n' _ | n' <= 0 = [[]]
          combinations' 1 xs' = map (:[]) xs'
          combinations' n' xs' = (:) <$> xs <*> combinations7 (n'-1) xs'
          -- sort every sublist and the list itself after that
          -- [[1,1],[1,2],[1,2],[1,3],[1,3],[2,2],[2,3],[2,3],[3,3]]
          sorted = sort . map sort $ combinations' n xs
          -- for each sublist, eliminate duplicates (see Problem 8)
          -- [[1],[1,2],[1,2],[1,3],[1,3],[2],[2,3],[2,3],[3]]
          grouped = map (map head . group) sorted
          -- filter all sublist with length < n,
          -- this means that they had at least two times the same value in it
          -- [[1,2],[1,2],[1,3],[1,3],[2,3],[2,3]]
          filtered = filter (\xs' -> length xs' == n) grouped
          -- eliminate duplicates a second time, this time in the list itself
          -- [[1,2],[1,3],[2,3]]
          compressed = map head . group $ filtered