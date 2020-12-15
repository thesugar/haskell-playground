{-# OPTIONS -Wall -Werror #-}
import Control.Applicative

{-
(*) Duplicate the elements of a list.

Example in Haskell:

λ> dupli [1, 2, 3]
[1,1,2,2,3,3]
-}

dupli :: [a] -> [a]
dupli = foldr (\x acc -> x:x:acc) []

dupli' :: [a] -> [a]
dupli' = foldr (\x acc -> (replicate 2 x) ++ acc) [] -- ++ 使ってるのが微妙だしこれなら上の回答のほうが良いと思うけども。

dupli'' :: [a] -> [a]
dupli'' [] = []
dupli'' (x:xs) = x:x:(dupli'' xs)

--- 今更なんでこんな簡単な。。。
--- 解答

dupli1 :: [a] -> [a]
dupli1 [] = []
dupli1 (x:xs) = x:x:dupli xs -- 自分の dupli'' と同じ。

-- リスト内包を使う
dupli2 :: [a] -> [a]
dupli2 list = concat [[x, x] | x <- list]

-- ってことは、解答例にはないけど map も使えるね
dupli2' :: [a] -> [a]
dupli2' list = concat $ map (\x -> [x, x]) list

-- リストモナドを使う
dupli3 :: [a] -> [a]
dupli3 xs = xs >>= (\x -> [x, x])

{-
最近 >>= を使ってなかったから復習。
>>= は、`文脈付きの値（モナド） >>= 普通の値を取って文脈付きの値（モナド）を返す関数` という形で使い、
文脈付きの値を受け取って文脈付きの値として返すことができる。
今回その文脈とはリスト（非決定性）。
       > Just "hoge" >>= (\x -> Just (x ++ "!!"))
       Just "hoge!!"

実際に試してみると以下のよう。
> [1,2,3] >>= (\x -> [x,x])
[1,1,2,2,3,3]
-}

-- アプリカティブを使うこともできる
-- import Control.Applicative
dupli4 :: [a] -> [a]
dupli4 = (<**> [id, id])

{-
<**> という見慣れない記号の使い方は以下のような感じ。

> :t (<**>)
(<**>) :: Applicative f => f a -> f (a -> b) -> f b

> :t (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

だから、引数を逆にしただけ。

*Main Control.Applicative> (<**>) (Just "hoge") (Just (\x -> x ++ "!"))
Just "hoge!"

*Main Control.Applicative> (Just "hoge") <**> Just (\x -> x ++ "!")
Just "hoge!"

> [1,2,3] <**> [id, id]
[1,1,2,2,3,3]

(<**>) の型シグネチャからすると ↑ の [1,2,3] <**> [id, id] は自然。
👉これを書き換えると (<**> [id, id]) [1,2,3] になるのがいまいちわからん🤔
(<**> [1,2,3]) [id, id] じゃないのはなぜ。。

ん？？？🧐
たしかに、以下のような関数を考えると [1,2,3] <**> [id, id] は (<**> [1,2,3]) [id, id] と等価になりそうだけど、
*Main Control.Applicative> 10 - 3
7
*Main Control.Applicative> ((-) 10) 3
7

以下のような関数を考えると [1,2,3] <**> [id, id] が (<**> [id, id]) [1,2,3]  になるのはうなずける
*Main Control.Applicative> "hello" ++ " world"
"hello world"
*Main Control.Applicative> (++ " world") "hello"
"hello world"
*Main Control.Applicative> (++ "hello") " world"
" worldhello"
*Main Control.Applicative> (++) "hello" " world"
"hello world"

というか、以下の違いか？　`mod` か (mod) か
*Main Control.Applicative> 10 `mod` 3
1
*Main Control.Applicative> (`mod` 10) 3
3
*Main Control.Applicative> (`mod` 3) 10
1
*Main Control.Applicative> ((mod) 3) 10
3
*Main Control.Applicative> ((mod) 10) 3
1

たしかに、

> ((<**>) [1,2,3]) [id, id]

こうすると結果は [1,1,2,2,3,3] になるぞ！

つまり、(<**> [id, id]) [1,2,3] は <**> を中置演算子のまま書いてる。mod の例で言うと (`mod` 3) 10 というふうに書いて、それが 10 `mod` 3 と等しいのと同じ。
だから [1,2,3] <**> [id, id] が (<**> [id, id]) [1,2,3] と等価になっている。
逆に、++ の例も同じで、(++ " world") "hello" における ++ は中置演算子のまま。だからその結果は "hello" ++ " world" と等しくなる。
一方、++ を (++) として中置演算子でなくすれば ((++) "hello") " world" と書くことで "hello world" という結果が得られる（もちろん (++) "hello" "world" と書くのと同じ）。
だからあえて同じことを繰り返して言うと、((<**>) [1,2,3]) [id, id] こうやって <**> を括弧で括って中置演算子じゃなくすれば、この書き方で [1,2,3] <**> [id, id] と同じ結果になる（これも外側の括弧は不要で (<**>) [1,2,3] [id, id] こう書いても同じ）。

参考

*Main Control.Applicative> [1,2,3] <**> [id]
[1,2,3]
*Main Control.Applicative> (<**> [id]) [1,2,3]
[1,2,3]
*Main Control.Applicative> (Just "hoge") <**> (Just (\x -> (x ++ "!!")))
Just "hoge!!"
*Main Control.Applicative> (<**> (Just (\x -> (x ++ "!!")))) (Just "hoge")
Just "hoge!!"
*Main Control.Applicative> [1,2,3] >>= (\x -> [x,x])
[1,1,2,2,3,3]
*Main Control.Applicative> ([1,2,3] >>=) (\x -> [x,x])
[1,1,2,2,3,3]
*Main Control.Applicative> (>>= (\x -> [x,x])) [1,2,3]
[1,1,2,2,3,3]
-}

-- concatMap を使う解答
dupli5 :: [a] -> [a]
dupli5 = concatMap (\x -> [x, x]) -- concat $ map (\x -> [x, x]) と同じ。

dupli5' :: [a] -> [a]
dupli5' = concatMap (replicate 2) -- replicate 2 は私も使った

dupli6 :: [a] -> [a]
dupli6 = foldl (\acc x -> acc ++ [x,x]) [] -- アキュムレーションのたびに ++ を使う（リスト全走査）ヤバげな解答

dupli7 :: [a] -> [a]
dupli7 = foldr (\ x xs -> x : x : xs) [] -- 私の最初の解答と同じ

dupli8 :: [a] -> [a]
dupli8 = foldr (\x -> (x:) . (x:)) []

dupli9 :: [a] -> [a]
dupli9 = foldr ((.) <$> (:) <*> (:)) []
{-
復習：
 (+) <$> (+3) <*> (*100) $ 5 の結果は 508 になる。

((.) <$> (:) <*> (:)) 1 []
は、「1 に (:) を適用したもの（= `1:`）」と「1 に (:) を適用したもの（= `1:`）」を合成（.）した関数 (1:) . (1:) を [] に適用して [1,1] になる。
[1,1]
-}

-- here is the proof that ((.) <$> (:) <*> (:)) = (\y z -> y:y:z) 🤔

-- (.) <$> (:) <*> (:) =
-- ((.) <$> (:)) <*> (:) = -- (<$>) is infixl 4, (<*>) is infixl 4
-- ((.) . (:)) <*> (:) = -- (<$>) == (.) for functions
-- (\x -> (.) (x:)) <*> (:) = -- definition of (.)
-- \y -> (\x -> (.) (x:)) y (y:) = -- definition of (<*>) for functions
-- \y -> ((.) (y:)) (y:) = -- beta reduction (applying y to (\x -> (.) (x:)))
-- \y -> (y:) . (y:) = -- changing (.) to its prefix form
-- \y -> (\z -> (y:) ((y:) z)) = -- definition of (.)
-- \y z -> y:y:z -- making it look nicer
