{-# OPTIONS -Wall -Werror #-}
import Data.List(findIndex)

{-
(**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

Example in Haskell:

λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]
-}

--- お手上げ 🙌😫

-- 解答 1
pack :: Eq a => [a] -> [[a]]
pack (x:xs) = let (first, rest) = span (==x) xs
              in (x:first) : pack rest
pack [] = []
    -- span っていう便利関数を使ってるからといえばそうなんだけど鮮やかな解法だ
    -- `Data.List` モジュールにおける `group` 関数の実装でもある
    -- span ( > 2) [4,6,7,2,8,9] = ([4,6,7],[2,8,9]) のように、条件を満たすかどうかでリストを分ける関数。結果はタプルになる。
    -- （c.f. take, drop, splitAt, および takeWhile, dropWhile, span の挙動を整理しておこう。take や drop, splitAt は数で指定する。先頭から n 要素取るとか落とすとか。一方、takeWhile や dropWhile などは述語で指定）

-- 解答 2（span を使わずに同じことをやる）
pack2 :: Eq a => [a] -> [[a]]
pack2 [] = []
pack2 (x:xs) = (x:first) : pack2 rest
        where
            getReps [] = ([], [])
            getReps (y:ys)
                    | y == x = let (f,r) = getReps ys in (y:f, r)
                    | otherwise = ([], (y:ys))
            (first, rest) = getReps xs

-- getReps の部分は、span (== x) と同じ。ためしに以下のように切り分けてみよう。
-- mySpan 2 [2,2,3,4] とすると結果は ([2,2],[3,4]) となる。
mySpan :: Eq a => a -> [a] -> ([a], [a])
mySpan x (y:ys)
    | y == x = let (f, r) = mySpan x ys in (y:f, r)
    | otherwise = ([], (y:ys))
mySpan _ [] = ([], [])

-- mySpan 2 [2,2,3,4]
-- -> mySpan 2 (2:[2,3,4])
-- -> (2:f, r) ただし (f, r) = mySpan 2 [2,3,4]
-- -> mySpan 2 [2,3,4] は mySpan 2 (2:[3,4])
-- -> (2:f, r) ただし (f, r) = mySpan 2 [3,4]
-- -> mySpan 2 [3,4] は mySpan 2 (3:[4])
-- -> ([], (3:[4]))
-- -> これをたどると
-- -> (2:2:[], 3:[4]) で結果 ([2,2], [3,4]) になる

-- 解答 3: span を使わず、splitAt を使って（インデックス番号による操作になる）同じことをする。
-- import Data.List(findIndex)
pack3 :: Eq a => [a] -> [[a]]
pack3 [] = []
pack3 (x:xs) = (x:reps) : (pack3 rest)
    where
        (reps, rest) = maybe (xs, []) (\i -> splitAt i xs)
                         (findIndex (/= x) xs)

{-
Prelude> :t maybe
maybe :: b -> (a -> b) -> Maybe a -> b

maybe関数は，第3引数の値がJustである場合には第2引数の関数を適用し，第3引数の値がNothingである場合には第1引数に与えたデフォルトの値をそのまま返す。
findIndex 関数は、結果が Maybe 型になる（失敗するかもしれないので）ため、maybe 関数を使う。
なお、(/= x) を第一引数に与えたfindIndex 関数が失敗するということは、xs の全要素が x に等しいということ。

Prelude> maybe "failed" show (Just 3)
"3"
Prelude> maybe "failed" show Nothing
"failed"

戻り値は必ずしも文字列である必要はない。数値でも構わない。

Prelude> maybe (0 :: Int) (\x -> x*x) (Just 5)
25
Prelude> maybe (0 :: Int) (\x -> x*x) Nothing
0
-}

-- 解法その4: takeWhile, dropWhile を利用
pack4 :: (Eq a) => [a] -> [[a]]
pack4 [] = []
pack4 (x:xs) = (x: takeWhile (==x) xs): pack (dropWhile (==x) xs) -- `pack (dropWhile (==x) xs)` の部分は、その左で takeWhile してるから、その右ではもう take した部分以降を考えるため drop している。

-- 解法その5: foldr
-- 💥なるほど。foldr を使う手は考えたがよくわからなくなって諦めた。以下のように実装すればいいのか。
pack5 :: (Eq a) => [a] -> [[a]]
pack5 = foldr func []
    where func x [] = [[x]]
          func x (y:xs) =
              if x == (head y) then ((x:y):xs) else ([x]:y:xs)


-- 解法その6：シンプルな解法
pack6 :: (Eq a) => [a] -> [[a]]
pack6 [] = []
pack6 [x] = [[x]]
pack6 (x:xs) = if x `elem` (head (pack6 xs))
               then (x:(head (pack6 xs))):(tail (pack6 xs))
               else [x]:(pack6 xs)

-- 解法その7
-- 解法その6 とほぼ同じ。解法その6でいちいち head (pack6 xs) と至る所に書いてあるのを h_p_xs で表現したり、という変化が加わってるだけ
pack7 :: (Eq a) => [a] -> [[a]]
pack7 [] = []
pack7 [x] = [[x]]
pack7 (x:xs)
    | x == head h_p_xs = (x:h_p_xs):t_p_hs
    | otherwise         = [x]:p_xs
    where p_xs@(h_p_xs:t_p_hs) = pack7 xs

-- 解法その8
myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack (y:ys) = impl ys [[y]]
    where
        impl [] packed = packed
        impl (x:xs) packed 
            | x == (head (last packed)) = impl xs ((init packed) ++ [x:(last packed)])
            | otherwise = impl xs (packed ++ [[x]])

myPack' :: (Eq a) => [a] -> [[a]]
myPack' [] = []
myPack' (y:ys) = reverse $ impl ys [[y]]
    where
        impl [] packed = packed
        impl (x:xs) p@(z:zs) 
            | x == (head z) = impl xs ((x:z):zs) 
            | otherwise     = impl xs ([x]:p)
        impl (_:_) [] = error "error!"