{-# OPTIONS -Wall -Werror #-}

{-
(**) Decode a run-length encoded list.

Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

Example in Haskell:

λ> decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
-}

-- 自分の解答
data ListItem a = Single a | Multiple Int a deriving (Show)

decodeModified :: [ListItem a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = x:(decodeModified xs)
decodeModified ((Multiple n x):xs) = (take n . repeat $ x) ++ (decodeModified xs)

decodeModified' :: [ListItem a] -> [a]
decodeModified' [] = []
decodeModified' ((Single x):xs) = x:(decodeModified' xs)
decodeModified' ((Multiple 1 x):xs) = decodeModified' ((Single x):xs) -- これを入れないと無限ループする
decodeModified' ((Multiple n x):xs) = x:(decodeModified' ((Multiple (n-1) x):xs)) -- ++ を使わない実装にした。括弧に注意。どこかの括弧が無かったりするとエラーになる。

decodeModified'' :: [ListItem a] -> [a]
decodeModified'' = foldr f []
       where
              f (Single x) acc = (x:acc)
              f (Multiple (n) x) acc = if n > 1 then x:(f (Multiple (n-1) x) acc) else (x:acc)


-- 解答
decodeModified1 :: [ListItem a] -> [a]
decodeModified1 = concatMap decodeHelper
    where
      decodeHelper (Single x)     = [x]
      decodeHelper (Multiple n x) = replicate n x
{-
なるほど concatMap か。かなりすっきり書けるな
       concatMap (\x -> x ++ "! ") ["ain't","nobody", "prayin", "for", "me"]
       "ain't! nobody! prayin! for! me! "

take n . repeat $ x は replicate n x で書ける
-}

-- 問題 10 でやった、（自作データ型を使わず、[a] 型を [(Int, a)] 型に変換するシンプルな）エンコーディングに対応するデコーダーは
-- 次のようにして書ける。
decode :: [(Int, a)] -> [a]
decode = concatMap (uncurry replicate)
-- decode [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-- "aaaabccaadeeee"
-- なお、uncurry は 2 引数関数とタプルを引数に取って、タプルを構成する 2 つの要素を、第一引数として uncurry が受け取った関数に与える
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- uncurry replicate (3, 'a')
-- "aaa"
-- ちなみに curry はその逆で、「タプルを引数に取る関数」と、さらに 2 つの値を引数に取り、その 2 つの値をタプルにしてタプルを引数に取る関数に与える。
-- curry :: ((a, b) -> c) -> a -> b -> c
-- curry (\(x,y) -> replicate x y) 3 'a'
-- "aaa"

-- ほんで、以下のような関数を考えれば、ListItem 型を (Int, a) 型に変換することは容易。
toTuple :: ListItem a -> (Int, a)
toTuple (Single x)     = (1, x)
toTuple (Multiple n x) = (n, x)

-- 以上の 2 つを組み合わせると、本問の [ListItem a] -> [a] なる型の関数も書ける。
decodeModified2 :: [ListItem a] -> [a]
decodeModified2 = concatMap (uncurry replicate . toTuple)

-- foldl を使ったナイーブな実装
-- やっぱり foldl を使うと ++ 演算が発生してしまうね
decodeModified3 :: [ListItem a]-> [a]
decodeModified3 = foldl (\x y -> x ++ decodeHelper y) []
    where
        decodeHelper (Single x)     = [x]
        decodeHelper (Multiple n x) = replicate n x

-- foldl の実装では、case 式を使って場合分けすることも可能
decodeModified4 :: [ListItem a] -> [a]
decodeModified4 = foldl (\acc e -> case e of Single x -> acc ++ [x]; Multiple n x -> acc ++ replicate n x) []


-- [(Int, a)] 型のものをデコードするシンプルな方法
decode' :: Eq a => [(Int,a)] -> [a]
decode' xs = foldr f [] xs
  where
    f (1, x) r = x : r
    f (k, x) r = x : f (k-1, x) r

-- ↑の説明にかんしてはこれで終わってるけど、これを使えば次のようにかけるってことよね
decodeModified5 :: [ListItem a] -> [a]
decodeModified5 xs = foldr f [] $ map toTuple xs
       where
              f (1, x) r = x : r
              f (k, x) r = x : f (k-1, x) r
              -- Single, Multiple のままやると、Multiple n x を Multiple (n-1) x を利用して再帰させてったとき、Multiple 1 が Single と同じだということは明示的に書かないといけないけど
              -- いったん (Int, a) 型のタプルを挟むと (n, x) を (n-1, x) を利用して再帰させていくと勝手にベースケース (1, x) に到達してくれる
              -- とはいえ、タプルを挟む手間はあるのでどっちが楽ってこともないけど 

-- 自分の解答とほぼ同じもの。
decodeModified6 :: [ListItem a] -> [a]
decodeModified6 [] = []
decodeModified6 ((Single x):xs) = x:decodeModified6 xs
decodeModified6 ((Multiple 2 x):xs) = x:x:decodeModified6 xs
decodeModified6 ((Multiple n x):xs) = x:decodeModified6 ((Multiple (n-1) x):xs)

-- その他
-- build 関数わからん
-- {-# INLINE decode #-}
-- decode :: Eq a => [(Int,a)] -> [a]
-- decode xs = build (\c n ->
--   let
--     f (1, x) r = x `c` r
--     f (k, x) r = x `c` f (k-1, x) r
--   in
--     foldr f n xs)

