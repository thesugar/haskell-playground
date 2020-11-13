{-# OPTIONS -Wall -Werror #-}

{-
Problem 3
(*) Find the K'th element of a list. The first element in the list is number 1.

Example in Haskell:

λ> elementAt [1,2,3] 2
2
λ> elementAt "haskell" 5
'e'
-}

-- 自分の答えその 1
elementAt :: [a] -> Int -> a
elementAt xs n = xs !! (n-1) 

-- 自分の答えその 2
elementAt' :: [a] -> Int -> a
elementAt' (x:_) 1 = x
elementAt' (_:xs) n = elementAt' xs (n-1)
elementAt' [] _ = error "empty list"
    -- 以上だと、無限リストが来てなおかつ 0 番目の要素というふうに指定されると（elementAt' [1..] 0 とした場合など）無限ループしてしまうので
    -- n が 1 未満の場合はエラー返すという処理をちゃんと書いた方がいい

------------
-- 解答

-- 自分の答えその 1 と完全に同じ
elementAt1 :: [a] -> Int -> a
elementAt1 list i = list !! (i-1)

-- 自分の答えその 2 とほぼ同じ（エラー処理が ↓ のほうがちゃんとしてる）
elementAt2 :: [a] -> Int -> a
elementAt2 (x:_) 1 = x
elementAt2 [] _ = error "Index out of bounds"
elementAt2 (_:xs) k
    | k < 1 = error "Index out of bounds"
    | otherwise = elementAt2 xs (k-1)

elementAt3 :: [a] -> Int -> a
elementAt3 xs n
    | length xs < n = error "Index out of bounds"
    | otherwise = fst . last $ zip xs [1..n]
        -- zip は zip "hoge" [1..10] とすると [('h',1),('o',2),('g',3),('e',4)] を返す関数（zip "hoge" [1..3] なら [('h',1),('o',2),('g',3)]）
        -- おもしろい

elementAt4 :: [a] -> Int -> a
elementAt4 xs n = head $ foldr ($) xs
                       $ replicate (n-1) tail
        -- うーん認知的負荷が高い。($) は $ を中置演算子としてでなくふつうの関数として使ってるだけ。
        -- イメージ的には head $ foldr ($) [1,2,3] [tail, tail] のような計算をすることになる
        -- つまり与えられたリストに対して、n のぶんだけ tail を適用して、すなわち n 回リストの先頭だけを切り落とす作業を繰り返して、
        -- そのあとに head で先頭を取り出せば求めたいものが得られるよねっていう発想。
        -- なお、replicate 関数は第一引数として負数を取ると空リストを返すため、この実装だと elementAt4 "haskell" (-1) などとすると 'h' が返ってしまう（正しくない動作）という問題がある。
        -- foldr は以下（念のため）。
        {-
            foldr f z [x1, x2, x3]
            -> f x1 (foldr f z [x2, x3])
            -> f x1 (f x2 (foldr f z [x3]))
            -> f x1 (f x2 (f x3 (foldr f z [])))
            -> f x1 (f x2 (f x3 z))        
        -}

elementAt5 :: [a] -> Int -> a
elementAt5 xs n
    | length xs < n = error "Index out of bounds"
    | otherwise = last $ take n xs -- これはわかりやすいね

elementAt6 :: [a] -> Int -> a
elementAt6 xs n
    | length xs < n = error "Index out of bounds"
    | otherwise = head . reverse $ take n xs -- elementAt5 とやってることは同じ

elementAt7 :: [a] -> Int -> a
elementAt7 xs n
    | length xs < n = error "Index out of bounds"
    | otherwise = head $ drop (n-1) xs -- drop は、drop 3 "drop-no-kimochi" とすると "p-no-kimochi" となる関数。これもわかりやすい解法

elementAt_w'pf :: [a] -> Int -> a
elementAt_w'pf = flip $ (last .) . take -- ポイントフリースタイルで書いている。あまりわかりやすくはない。。flip は、flip をつけないと問題の指定と引数の順番が逆になってしまうからつけているだけ。


