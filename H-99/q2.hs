{-# OPTIONS -Wall -Werror #-}

{-
(*) Find the last but one element of a list.

Example in Haskell:

λ> myButLast [1,2,3,4]
3
λ> myButLast ['a'..'z']
'y'
-}

-- 自分の答え（✅）
myButLast :: [a] -> a
myButLast (x:xs)
    | length xs == 1 = x
    | otherwise = myButLast xs
myButLast [] = error "can't use to empty lists."

-- 別解
myButLast1 :: [a] -> a
myButLast1 = last . init -- わかりやすい。なお、init [1,2,3] = [1,2]

myButLast2 :: [a] -> a
myButLast2 x = reverse x !! 1 -- これも単純な解法

myButLast3 :: [a] -> a
myButLast3 [x, _] = x -- [x, y] みたいなパターンマッチもできるんだ。盲点だった。例えば [1,2] を [x,_] にパターンマッチすると x は 1 に、_ は 2 になる。
myButLast3 (_:xs) = myButLast3 xs
myButLast3 [] = error ""

myButLast4 :: [a] -> a
myButLast4 (x:(_:[])) = x -- これもパターンマッチを題意とおりに書いたという感じでわりとわかりやすい
myButLast4 (_:xs) = myButLast4 xs
myButLast4 [] = error ""

myButLast5 :: [a] -> a
myButLast5 = head . tail . reverse -- tail は先頭以外の要素を返す。これも単純な解答。

lastbut1 :: Foldable f => f a -> a
lastbut1 = fst . foldl (\(_, b) x -> (b, x)) (err1, err2) -- 引数が空リストのときは foldl が実質行われなくて、初期値 (err1, err2) のまま fst (err1, err2) が評価されて err1 が返る。
                                                          -- 引数が要素数 1 のリストの場合は、ラムダ式で表されている関数が 1 回のみ実行されて (err2, リストの中のその 1 つだけの要素) となり、fst を取ると err2 が返る。
                                                          -- 引数の要素数が 2 以上のときは、最終的には fst (リストの最後から 2 番目の要素、リストの最後の要素) となり、題意にしたがう。
    where
        err1 = error "lastbut1: Empty list"
        err2 = error "lastbut1: Singleton"

lastbut1safe :: Foldable f => f a -> Maybe a
lastbut1safe = fst . foldl (\(_, b) x -> (b, Just x)) (Nothing, Nothing)

-- これは自分の解答とほぼ同じ。
myButLast6 :: [a] -> a
myButLast6 [] = error "Empty list" 
myButLast6 [_] = error "Too few elements"
myButLast6 (x:xs) =
    if length xs == 1 then x
    else myButLast6 xs

myButLast7 :: [a] -> a
myButLast7 = head . reverse . init -- これも単純
