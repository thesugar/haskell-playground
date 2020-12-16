{-# OPTIONS -Wall -Werror #-}
import Control.Applicative

{-
(**) Replicate the elements of a list a given number of times.

Example in Haskell:

λ> repli "abc" 3
"aaabbbccc"
-}

repli :: [a] -> Int -> [a]
repli xs n = foldr (\x acc -> take n (repeat x) ++ acc) [] xs -- ++ を使ってて微妙？

repli' :: [a] -> Int -> [a]
repli' xs n = concatMap (replicate n) xs -- 本問で replicate 使っちゃうのずるい気がするけど take n (repeat x) を使って良いとするなら replicate だっていいはず

repli_ :: [a] -> Int -> [a]
repli_ xs n = concatMap (take n . repeat . id) xs

-- replicate の部分は以下のようにも書ける。
-- > map snd $ zip [1..3] (repeat 'a')
-- "aaa"

repli'' :: [a] -> Int -> [a]
repli'' xs n = xs >>= (\x -> replicate n x)

repli''' :: [a] -> Int -> [a]
repli''' xs n = (<**> replicate n id) xs
       -- q14 を参考にした。<**> は、型シグネチャを確認するかぎり、<*> の引数を逆にしたものにすぎない。
       -- ちなみに、復習。
       -- *Main> Just (+3) <*> Just 9
       --  Just 12
       -- だから、当然 ↓
       -- *Main> Just 9 <**> Just (+3)
       -- Just 12
       
       -- つまり、本問では、xs という値を引数に取って結果を返す関数を (<**> replicate n id) この形でくくり出した。
       -- q14 で考察したとおりだが、ここで <**> は中置関数として使っているので、 xs <**> replicate ... の形になる。


--- 🤔❓以下の結果は "abcabcabc" になってしまう。。。なぜ？？
repli'''' :: [a] -> Int -> [a]
repli'''' xs n = (replicate n id) <*> xs

-- 解答
repli1 :: [a] -> Int -> [a]
repli1 xs n = concatMap (replicate n) xs -- 自分の repli' と同じ。

-- Pointfree style
repli2 :: [a] -> Int -> [a]
repli2 = flip $ concatMap . replicate

-- replicate を使わない
repli3 :: [a] -> Int -> [a]
repli3 xs n = concatMap (take n . repeat) xs -- はい。

-- リストモナド
repli4 :: [a] -> Int -> [a]
repli4 xs n = xs >>= replicate n -- 自分の解答だと xs >>= (\x -> replicate n x) としたけどたしかに replicate n と書くだけでよいのか。。

-- 冗長な解答
-- まあ冗長だし foldl だし（foldl だから ++ を使うことになるし）で微妙だけど明示的に再帰を書いていてわかりやすさはあるのかもなぁ
repli5 :: [a] -> Int -> [a]
repli5 xs n = foldl (\acc e -> acc ++ repli_' e n) [] xs
       where
              repli_' _ 0 = []
              repli_' x num = x : repli_' x (num - 1)

-- or, a version that does not use list concatenation:
repli6 :: [a] -> Int -> [a]
repli6 [] _ = []
repli6 (x:xs) n = foldr (const (x:)) (repli6 xs n) [1..n]
{-
repli6 = foldr ... repli6 ... というふうな構造の再帰になっているところはとっつきづらいが、この解答の説明にある "does not use list concatenation" のとおり。
repli6 の再帰をたどっていくと repli6 (x:[]) n = ... (repli6 [] n) [1..n] となって repli6 [] _ = [] のパターンマッチより repli6 (x:[]) n = ... [] [1..n] となるから、
repli6 (x:[]) n は計算できる。
すると、その再帰を、たどり切ったところから 1 つ上に戻って repli6 (x':[x]) n = ...(repli6 [x] n) [1..n] となるが、repli6 [x] n は repli6 (x:[]) n のことであり、それは計算できたのだから
repli6 (x':[x]) n も計算できる。以後同様。
"abc" を "aaabbbccc" と変換するにあたって、concatenation を使わずに再帰的にやってるわけだが、流れとしては "abc" のうちの 'c' を "ccc" に複製 -> 'b' を 3 回、そのときのアキュムレータ
（その時点で出来上がってるリスト）"ccc" に (:) でくっつけていく -> "bbbccc" ができあがる -> それに 'a' を 3 回くっつけていく、という流れ。
repli6 xs n は、「今から c をくっつけていくぞ」「今から b をくっつけていくぞ」「今から a をくっつけていくぞ」という各回における初期（？）アキュムレータになる（それぞれ [], "ccc", "bbbccc"）。

〜実際の関数の実装について〜

[1..n] で、replicate したい数だけ foldr をするというイメージ。
それで、foldr がとる関数は (const (x:)) だから、foldr (\y acc -> const (x:) y acc) と書いているのと同じ（以下参照）。
つまり、どんな要素 y がきても（ここでの y は 1, 2, ..）、x: を返すのだから、つまり n 回 (x:) をすることになる。
-}

repli6' :: [a] -> Int -> [a]
repli6' [] _ = []
repli6' (x:xs) n = foldr (\y acc -> const (x:) y acc) (repli6' xs n) [1..n]

-- めちゃアホっぽい挙動の追跡
repli6'' :: (Show a, Show b, Num b, Enum b) => [a] -> b -> [String]
repli6'' [] _ = []
repli6'' (x:xs) n = foldr (\y acc -> (show x ++ show y ++ "dayo"): acc) (repli6'' xs n) [1..n]
{-
> repli6'' "abc" 3
["'a'1dayo","'a'2dayo","'a'3dayo","'b'1dayo","'b'2dayo","'b'3dayo","'c'1dayo","'c'2dayo","'c'3dayo"]
-}