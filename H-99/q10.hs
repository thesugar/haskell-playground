{-# OPTIONS -Wall -Werror #-}
import Data.List(group)
import Control.Arrow((&&&))

{-
(*) Run-length encoding of a list. 
Use the result of problem P09 to implement the so-called run-length encoding data compression method.
Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

Example in Haskell:

λ> encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}

-- Use the result of problem P09 と書いてあるので、
pack :: Eq a => [a] -> [[a]]
pack (x:xs) = let (first, rest) = span (==x) xs
              in (x:first) : pack rest
pack [] = []

{-
> pack "aaaabccaadeeee"
["aaaa","b","cc","aa","d","eeee"]
-}

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) $ pack xs


--- 解答
encode1 :: Eq a => [a] -> [(Int, a)]
encode1 xs = map (\x -> (length x,head x)) (group xs) -- group は pack と同じなので、自分の答えと同じ。

encode2 :: Eq a => [a] -> [(Int, a)]
encode2 xs = [(length x, head x) | x <- group xs] -- リスト内包表記でも書ける。

encode3 :: Eq a => [a] -> [(Int, a)]
encode3 ls = [(length (x:xs), x)| (x:xs) <- group ls]

-- (Ab)using the "&&&" arrow operator for tuples
-- Arrow 型クラスよくわからないし、abusing ってことで、本来的には以下のような計算をするための便利演算子ではないよってことなんだろうけども
-- それでも使い方としては (length &&& head) ['a', 'b', 'c'] の結果は (3,'a') になる。
-- これを map で、リスト内の各リスト（文字列）に対して適用している。
encode4 :: Eq a => [a] -> [(Int, a)]
encode4 xs = map (length &&& head) $ group xs

--Or using the slightly more verbose (w.r.t. (&&&)) Applicative combinators:
-- * w.r.t = with reference to
encode5 :: Eq a => [a] -> [(Int, a)]
encode5 = map ((,) <$> length <*> head) . group
{-
まず、<$> と <*> の使い方（アプリカティブスタイル）は以下のとおり。
*Main> (+) <$> (*2) <*> (/ 10) $ 100
210.0

(,) 関数はタプルを作るもので以下のとおり。
*Main> (,) 1 2
(1,2)
-}

encode6 :: Eq a => [a] -> [(Int, a)]
encode6 xs = (enc . group) xs
    where enc = foldr (\x acc -> (length x, head x) : acc) []

encode6' :: Eq a => [a] -> [(Int, a)]
encode6' xs = foldr (\x acc -> (length x, head x): acc) [] $ group xs
            -- map (\x -> (length x,head x)) (group xs) は foldr を使っても書き換えられるということ。この場合は map のほうがすっきりしてる気がするが

-- takeWhile, dropWhile を使った実装（group や pack 関数を使わずに書ける）
encode7 :: Eq a => [a] -> [(Int, a)]
encode7 [] = []
encode7 (x:xs) = (length $ x : takeWhile (==x) xs, x)
                 : encode7 (dropWhile (==x) xs)

-- 高階関数を使わない実装
-- 式を追っていけば意味はわかるけどあんまりわかりやすくはない
encode8 :: (Eq b, Num a) => [b] -> [(a, b)]
encode8 [] = []
encode8 (x:xs) = enc' 1 x xs
    where
        enc' n x' [] = [(n, x')]
        enc' n x' (y:ys)
            | x' == y = enc' (n+1) x' ys
            | otherwise = (n, x'): enc' 1 y ys


encode9 :: Eq a => [a] -> [(Int, a)]
encode9 xs = zip (map length l) h
    where 
        l = (group xs)
        h = map head l
{-
タプルを作るときに zip 関数を使う解法。
参考.
> zip [1..] "abc"
[(1,'a'),(2,'b'),(3,'c')]
-}

encode10 :: Eq a => [a] -> [(Int,a)]
encode10 xs = foldr f final xs Nothing
    where
        f x r (Just a@(i,q)) | x == q = r (Just (i+1,q))
                             | otherwise = a : r (Just (1, x))
        f x r Nothing = r (Just (1, x))
        
        final (Just a) = [a]
        final Nothing = []
    -- encode10 の畳み込み（foldr）において、xs の各要素に対して、final を初期アキュミュレータとして、関数 f が畳み込まれる。
    -- 関数 f の定義式の中での r が final のイメージ。
    -- また、as パターンを使った Just a@(i,q) のパターンマッチは、以下の hoge 関数のように、Just (タプル) にマッチする。

hoge :: Num a => Maybe (a, b) -> (a, (a, b), b)
hoge (Just a@(i,q)) = (i+1, a, q)
hoge Nothing = error "error!!!"
{-
*Main> hoge (Just (10, "abc"))
(11,(10,"abc"),"abc")
-}



{-
build 関数　よくわかんない
{-# INLINE encode #-}
encode :: Eq a => [a] -> [(Int,a)]
encode xs = build (\c n ->
  let
    f x r (Just a@(i,q)) | x == q = r (Just (i+1,q))
                         | otherwise = a `c` r (Just (1, x))
    f x r Nothing = r (Just (1, x))

    final (Just a@(i,q)) = a `c` n
    final Nothing = n

  in
    foldr f final xs Nothing)
-}