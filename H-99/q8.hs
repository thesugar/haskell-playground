{-# OPTIONS -Wall -Werror #-}

import Data.List(group)

{-
(**) Eliminate consecutive duplicates of list elements.

If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

Example in Haskell:
λ> compress "aaaabccaadeeee"
"abcade"
-}

test :: (String -> String) -> Bool
test func = func "aaaabccaadeeee" == "abcade"

-- 自分の答えその 1
compress :: Eq a => [a] -> [a]
compress [] = []
compress ([x]) = [x]
compress (x1:x2:xs)
    | x1 == x2 = compress (x1:xs) -- ここの右辺は compress (x2:xs) としてもいいし、そうすると解答例の compress2 と同じになって、x2:xs は（compress2 でいう）ys であるから as パターンを使える
    | otherwise = x1:(compress (x2:xs))


-- 自分の答えその 2
compress' :: Eq a => [a] -> [a]
compress' = foldr (\x acc -> if length acc > 0 && x == head acc then acc else (x:acc)) []

---- 解答
-- import Data.List(group)
compress1 :: Eq a => [a] -> [a]
compress1 = map head . group
    -- > group "aaaabccaadeeee"
    -- ["aaaa","b","cc","aa","d","eeee"]
    -- group 関数なんて知らなかったな〜〜・・・←すごい Haskell 楽しく学ぼう 6 章の中の「標準モジュールの関数で問題を解く」でやってる！！

-- 自分の答えその 1 と同じ（as パターンを使っているが）
compress2 :: Eq a => [a] -> [a]
compress2 (x:ys@(y:_))
    | x == y    = compress2 ys
    | otherwise = x : compress2 ys
compress2 ys = ys
{-
ここで使われている @ は as パターン。すごい Haskell 3 章で出てくる。
as パターンは、値をパターンに分解しつつ、パターンマッチの対象になった値自体も参照したいときに使う
as パターンを作るには、普通のパターンの前に名前と @ を追加する

例えば、xs@(x:y:ys) のような as パターンを作れる。
このパターンは、x:y:ys に合致するものとまったく同じものに合致しつつ、xs で元のリスト全体にアクセスできる。
-}

-- 🤔❓❓
compress3 :: Eq a => [a] -> [a]
compress3 xs = foldr f (const []) xs Nothing
    where
        f x r a@(Just q) | x == q = r a
        f x r _ = x : r (Just x)
-- > https://stackoverflow.com/questions/28106666/haskell-99-questions-8-cant-understand-foldr
---- めちゃわかりやすい解説🎉
--- あと、こういうタイプの guard ってどんな挙動だっけ？ってことの確認↓
guardTest :: Int -> String
guardTest x = f x
    where
        f n | n `mod` 2 == 0 = "even number"
        f _ = "odd number"

-- 自分の答えその 2 と基本的には同じ。
-- this one is not so efficient, because it pushes the whole input onto the "stack" before doing anything else とのこと。
compress4 :: (Eq a) => [a] -> [a]
compress4 = foldr skipDups []
    where skipDups x [] = [x]
          skipDups x acc
                | x == head acc = acc
                | otherwise = x : acc

compress5 :: (Eq a) => [a] -> [a]
compress5 list = compress_acc list []
          where compress_acc [] acc = acc -- ここでの acc は [] に限定されている（はず）。ここからの再帰は発生しないので
                compress_acc [x] acc = (acc ++ [x]) -- ここでの acc は [] に限定されている（はず）。ここからの再帰は発生しないので
                compress_acc (x:xs) acc
                  | x == (head xs)  = compress_acc xs acc
                  | otherwise       = compress_acc xs (acc ++ [x])

--- かなりシンプル。
compress6 :: Eq a => [a] -> [a]
compress6 [] = []
compress6 (x:xs) = x : (compress6 $ dropWhile (== x) xs)

--- これもシンプルだし compress4 とか自分の答えその 2 と似通った発想
compress7 :: Eq a => [a] -> [a]
compress7 [] = []
compress7 x = foldr (\a b -> if a == (head b) then b else a:b) [last x] x

--- Wrong solution として紹介されているもの（`elem` を使って判断したら題意は満たさない。逆に、重複を排除したリストを作りたい場合はこれが使える）
compress8 :: Eq a => [a] -> [a]
compress8 xs = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] xs
-- Main> compress [1, 1, 1, 2, 2, 1, 1]
-- [2,1] - must be [1,2,1]


-- foldl を使った例
-- ++ を使ってるし怪しい
compress9 :: (Eq a) => [a] -> [a]
compress9 x = foldl (\a b -> if (last a) == b then a else a ++ [b]) [head x] x

-- これも foldl を使った例
compress10 :: (Eq a) => [a] -> [a]
compress10 x = reverse $ foldl (\a b -> if (head a) == b then a else b:a) [head x] x

-- A crazy variation that acts as a good transformer for fold/build fusion
-- build 関数よくわからない。

-- {-# INLINE compress #-}
-- compressor :: Eq a => [a] -> [a]
-- compressor xs = build (\c n ->
--   let
--     f x r a@(Just q) | x == q = r a
--     f x r _ = x `c` r (Just x)
--   in
--     foldr f (const n) xs Nothing)