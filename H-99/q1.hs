{-# OPTIONS -Wall -Werror #-}

{-
Find the last element of a list.

Example in Haskell:

λ> myLast [1,2,3,4]
4
λ> myLast ['x','y','z']
'z'
-}

-- 自分の答え（✅）
myLast :: [a] -> a
myLast [] = error "you can't use `myLast` to empty lists!"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

-- 別解
myLast1 :: [a] -> a
myLast1 = foldr1 (const id)
 --    foldr1 f [x1, x2, x3]
 --  -> f x1 (foldr1 f [x2, x3])
 --  -> f x1 (f x2 (foldr1 f [x3]))
 --  -> f x1 (f x2 x3))
 -- const は、const 10 3 = 10、const 'A' "hoge" = 'A' などからわかるように、第一引数を定数として返す関数。
 -- つまり const は「引数を 2 つ取って 1 つ目の引数をそのまま返す関数」。
 -- 一方、const id は、「引数を 2 つ取って 2 つ目の引数をそのまま返す関数」になる。これは、const id ○○ としたとき、const の定義から `id` と `○○` の 2 引数のうち最初の引数 id を返すから。
 -- つまり const id x = id となるのである。第一引数 x を無視できるというのがポイント。
 -- そういうことで、fold1 (const) とすると（id 関数が無い）リストのヘッド要素を返す関数になる。
 -- https://stackoverflow.com/questions/48611896/foldr-and-foldr1-haskell
 -- http://yomi322.hateblo.jp/entry/2012/10/05/120836

myLast2 :: [a] -> a
myLast2 = foldr1 (flip const)
 -- flip :: (a -> b -> c) -> b -> a -> c
 -- 関数を取って、次にまずその関数の第二引数を取って、その次に関数の第一引数を取る。つまり、引数の順序を交換するコンビネータ。
 -- const foo bar は foo を返すが、flip const foo bar は bar を返す。ここまでわかれば const id を使った答えと同じ。]

myLast3 :: [a] -> a
myLast3 = head . reverse -- 直感的！

myLast4 :: [a] -> a
myLast4 = foldl1 (curry snd)
    -- curry :: ((a, b) -> c) -> a -> b -> c
    -- > curry (\(x,y) -> x ++ " save the " ++ y) "god" "queen"
    -- "god save the queen"
    -- snd を curry 化すると、2 つの引数を取って snd により（つまり内部的には \(x,y) -> snd (x, y) というふうに扱ってる感じになる、、はず）2 つ目の要素を返す関数になる。
    -- それを左畳み込みするから、最終的には最後の要素が返る。

myLast5 :: [a] -> a
myLast5 [] = error "No end for empty lists!"  
myLast5 x = x !! (length x -1)
    -- これは単純な例。length x - 1 の -1 は index が zero-based だからというだけ。