{-# OPTIONS -Wall -Werror #-}


{-
Sorting a list of lists according to length of sublists

a) We suppose that a list contains elements that are lists themselves. 
The objective is to sort the elements of this list according to their length. 
E.g. short lists first, longer lists later, or vice versa.


Example in Haskell:

λ> lsort ["abc","de","fgh","de","ijkl","mn","o"]
["o","de","de","mn","abc","fgh","ijkl"]

b) Again, we suppose that a list contains elements that are lists themselves. 
But this time the objective is to sort the elements of this list according to their length frequency; 
i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

Example in Haskell:

λ> lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
["ijkl","o","abc","fgh","de","de","mn"]
-}

-- 解答みる

import Data.List
import Data.Ord (comparing)
import Data.Function (on)

lsort :: [[a]] -> [[a]]
lsort = sortBy (comparing length)

{-
sortBy :: (a -> a -> Ordering) -> [a] -> [a]

comparing :: Ord a => (b -> a) -> b -> b -> Ordering
comparing length "hoge" "ma" = GT
-}

-- comparing 関数を使わずとも・・・
lsort' :: [[a]] -> [[a]]
lsort' = sortBy (\xs ys -> compare (length xs) (length ys))

-- sortOn f is equivalent to sortBy . comparing f, but has the performance advantage of only evaluating f once for each element in the input list.
lsort'' :: [[a]] -> [[a]]
lsort'' = sortOn length

-- import Data.List (sort, group)
lSort :: (Ord a) => [[a]] -> [[a]]
lSort [] = []
lSort (x:xs) = shorter ++ (x: longer)
   where
      shorter = lSort [y | y <- xs, isShorterThan_x y] -- ここと
      longer  = lSort [y | y <- xs, isLongerOrEqual_x y] -- ここで lSort を再帰させている
      isShorterThan_x y = not $ isLongerOrEqual_x y
      isLongerOrEqual_x y
         | length y >= length x = True
         | otherwise = False
         -- すごい H 本第 5 章の quicksort の実装とほぼ同じ


-- (b)
--- 長さの頻度でソート　→　つまり、長さをベースにグルーピング（サブリスト化）して、そのグループ（サブリスト）の長さを比較すればよい
lfsort :: [[a]] -> [[a]]
lfsort lists = concat groups
    where groups = lsort $ groupBy equalLength $ lsort lists
          equalLength xs ys = length xs == length ys

{-
> :t groupBy
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]

> groupBy (\x y -> length x == length y) $ lsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
[["o"],["de","de","mn"],["abc","fgh"],["ijkl"]]
となるから、
> lsort $ groupBy (\x y -> length x == length y) $ lsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
[["o"],["ijkl"],["abc","fgh"],["de","de","mn"]]
であり、これを concat すると所望のリストが得られる。

ただし❗️❗️

先に lsort しているために、example として挙げられている出力とは厳密には一致しない。

⬇️ example
λ> lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
["ijkl","o","abc","fgh","de","de","mn"]

⬇️　本解法
> lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
["o","ijkl","abc","fgh","de","de","mn"]
-}

-- より完結な解法だが、これも同様に、example として挙げられている出力とは厳密には一致しない。
-- import Data.Function
lfsort' :: [[a]] -> [[a]]
lfsort' = concat . lsort . groupBy ((==) `on` length) . lsort

-- 非効率だが、example と出力が一致する
-- lfsort'' 関数で l を走査するとき、毎回の 2 要素の比較のたびに l（リスト）を対象にその要素と同じ長さのサブリストがいくつあるかという頻度を調べるから本当に効率悪いのでは。。
frequency :: Int -> [[a]] -> Int
frequency len l = length (filter (\x -> length x == len) l)

lfsort'' :: [[a]] -> [[a]]
lfsort'' l = sortBy (\xs ys -> compare (frequency (length xs) l) (frequency (length ys) l)) l

-- sortOn f は sortBy . comparing f と同じ。
-- ややこしく見えるけど、与えられたリスト -> （各要素の長さ、各要素）というタプルのリストにマッピングして、その最初の要素（fst。各要素の長さ）でソートして、
-- そのあとにその長さが同じものでグルーピングして、さらにそのグループの長さでソートすることで、長さの頻度順で並び替え、
-- それを concat して map snd することで所望のリストを得ている。
lfsort''' :: [[a]] -> [[a]]
lfsort''' = map snd . concat . 
            sortOn 
               length . groupBy ((==) `on` fst) . 
            sortOn fst . map (\x -> (length x, x))

-- import Control.Arrow ((>>>),(&&&),second)
-- import GHC.Exts (sortWith)

-- lfsort :: [[a]] -> [[a]]
-- lfsort = zip [1..] >>> map (second (length &&& id)) >>> sortWith (snd>>>fst) 
--            >>> cntDupLength undefined [] >>> sortWith (snd>>>fst) 
--            >>> sortWith fst >>> map (\(_,(_,(_,a))) -> a)
--   where cntDupLength :: Int -> [(Int,(Int,a))] -> [(Int,(Int,a))] -> [(Int,(Int,(Int,a)))]
--         cntDupLength _ lls [] = map ((,) (length lls)) $ reverse lls
--         cntDupLength _ [] (x@(_,(l,_)):xs) = cntDupLength l [x] xs
--         cntDupLength l lls ys@(x@(_,(l1,_)):xs)
--            | l == l1   = cntDupLength l (x:lls) xs
--            | otherwise = (map ((,) (length lls)) $ reverse lls) ++ cntDupLength undefined [] ys


freq :: [[a]] -> [a] -> Int
freq xs x
  = length $ filter ((== length x) . length) xs
{-
> freq ["abc", "de", "fgh", "de", "ijkl", "mn", "o"] "ho"
3
-}

lfSort :: (Ord a) => [[a]] -> [[a]]
lfSort [] = []
lfSort lst@(x:xs) = shorter ++ (x : longer)
  where
    shorter = lfSort [y | y <- xs, not (isFreqThan_x y)]
    longer  = lfSort [y | y <- xs, isFreqThan_x y]
    isFreqThan_x y 
      | freq lst y >= freq lst x = True
      | otherwise = False
