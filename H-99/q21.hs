{-# OPTIONS -Wall -Werror #-}

{-
Insert an element at a given position into a list.

Example in Haskell:

λ> insertAt 'X' "abcd" 2
"aXbcd"
-}

insertAt :: a -> [a] -> Int -> [a]
insertAt elm ls 1 = elm:ls
insertAt elm (x:xs) n = x:(insertAt elm xs (n-1))
insertAt _ _ _ = error "index out of bound"

-- ん〜〜、この解き方は個人的には好きではあるが、今回に関してはどうなんだろって思わざるをえない
-- x:xs の部分が、もっとスマートに書けそうな気がする
-- あと、length を使ってるから無限リストに対しては使えないんじゃないかと思ったけど、take 5 $ insertAt' 'X' ['a'..] 2 としても（ちょっと時間がかかるけど）"aXbcd" という結果が返ってきてくれる。遅延評価のなせる技？
insertAt' :: a -> [a] -> Int -> [a]
insertAt' elm ls n = snd $ foldr f (length ls, []) ls
       where f x (idx, xs) = if idx == n then (idx-1, elm:x:xs) else (idx-1, x:xs)

insertAt'' :: a -> [a] -> Int -> [a]
insertAt'' elm ls n = take (n-1) ls ++ [elm] ++ drop (n-1) ls

insertAt''' :: a -> [a] -> Int -> Maybe [a]
insertAt''' elm ls n
       | n <= 0 = Nothing
       | n > length ls = Nothing
       | otherwise = Just $ take (n-1) ls ++ [elm] ++ drop (n-1) ls
{-
これも無限リストに対しても使える。
       insertAt''' 'X' ['a'..] 2 >>= (\x -> Just (take 5 x))
       Just "aXbcd"
-}

-- 解答
insertAt1 :: a -> [a] -> Int -> [a]
insertAt1 x xs n = let (ys,zs) = splitAt (n-1) xs in ys ++ x:zs

-- 自分の解答 insertAt と同じ
insertAt2 :: a -> [a] -> Int -> [a]
insertAt2 x ys 1 = x:ys
insertAt2 x (y:ys) n = y:insertAt2 x ys (n-1)
insertAt2 _ _ _ = error "index out of bound"

-- 自分の回答 insertAt'' と同じ
insertAt3 :: a -> [a] -> Int -> [a]
insertAt3 x xs n = take (n-1) xs ++ [x] ++ drop (n-1) xs

-- お〜 foldl 使ってる。自分の回答 insertAt' と似ている。アキュムレータも (所望のリスト, インデックス) のタプルを使ってるし。
-- これも無限リストに対しても動作する（なんかかなり遅いけど）。take 5 $ insertAt4 'X' ['a'..] 2
insertAt4 :: a -> [a] -> Int -> [a]
insertAt4 el lst n = fst $ foldl helper ([],1) lst
       where helper (acc, i) x = if i == n then (acc ++ [el,x], i+1) else (acc++[x], i+1)

-- The use of foldl imposes the use of concatenation. （それ❗️）
-- With a foldr we can use (:) instead, which is faster (O(n) vs. O(n²)).
-- The use of zip [1..] does not seem to add any overhead compared to the same solution with the index stored in the accumulator.
-- あ〜〜なるほど、これはアキュムレータを普通にリスト（初期値 []）にしていて、畳み込まれるリストを zip [1..] lst（ペアのリスト）にしている。
-- 書き方自体は insertAt' と似たようなもの（where 以下）なんだけど、length を使ってリストの長さをとる必要がなくなるから早くなる。
-- zip を使うって基本的な技だけど、うっかり忘れてたな。。。
insertAt5 :: a -> [a] -> Int -> [a]
insertAt5 elt lst pos = foldr concat' [] $ zip [1..] lst
       where
              concat' (i, x) xs
                     | i == pos  = elt:x:xs
                     | otherwise = x:xs