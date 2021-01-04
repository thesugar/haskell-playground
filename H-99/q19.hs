{-# OPTIONS -Wall -Werror #-}


{-
(**) Rotate a list N places to the left.

Hint: Use the predefined functions length and (++).

Examples in Haskell:

λ> rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc"

λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
"ghabcdef"
-}

-- これでもいいが、rotate "abcdefgh" 10 などと、リストの長さを超える数字を与えられたときに "abcdefgh" という結果を返してしまい、ちょっと期待する挙動（"cdefghab" が返ってきてほしい）とずれる
rotate :: [a] -> Int -> [a]
rotate ls n
       | n >= 0 = drop n ls ++ take n ls
       | n < 0 = rotate ls (length ls + n) -- let n' = length ls + n in drop n' ls ++ take n' ls としてもよい
       | otherwise = error "unknown error"

-- 解答
-- この再帰が書けなかったのは反省したほうがよい
rotate1 :: [a] -> Int -> [a]
rotate1 [] _ = []
rotate1 xs 0 = xs
rotate1 (x:xs) n | n > 0 = rotate1 (xs ++ [x]) (n-1) -- あ〜そっか、rotate (x:xs) n = rotate xs (n-1) ++ [x] と書くと、"defghcba" のように後ろの順番が題意と異なってしまってどうしたもんかと思ってたけど、こうやって書けばいいだけか
rotate1 xs n = rotate1 xs (length xs + n)

-- rotate という関数名のとおり、円形をイメージすればよい
-- というところまでは思いついたのだが、それよりもマイナスの場合を `mod` (length xs) で処理できるのが思いつかなかった、、
-- たとえば (-2) `mod` 8 は 6 になる
rotate2 :: [a] -> Int -> [a]
rotate2 xs n = take len . drop (n `mod` len) . cycle $ xs
    where len = length xs


-- リスト内包
-- この解法は、型シグネチャにもあるとおり Enum 型クラスを実装している要素からなるリストに対してしか機能しない
-- rotate3 ["foo", "bar", "baz", "nya", "myo"] 3 とかは不可能
rotate3 :: (Enum a) => [a] -> Int -> [a]
rotate3 xs n = [(f n) .. last xs] ++ [head xs .. (f (n-1))]
       where f k = xs !! (k `mod` length xs)

-- mod を使わず
rotate4 :: [a] -> Int -> [a]
rotate4 xs n = take (length xs) $ drop (length xs + n) $ cycle xs -- cycle を使って length xs + n を使えばこれでも n の正負で場合分けせずワンライナーで書けるんだ

-- これが一番自分の解答と似ている
-- これでもいいが、rotate "abcdefgh" 10 などと、リストの長さを超える数字を与えられたときに "abcdefgh" という結果を返してしまい、ちょっと期待する挙動（"cdefghab" が返ってきてほしい）とずれる
rotate5 :: [a] -> Int -> [a]
rotate5 xs n = if n >= 0 then drop n xs ++ take n xs 
              else let l = ((length xs) + n) in drop l xs ++ take l xs

rotate6 :: [a] -> Int -> [a]
rotate6 xs n | n >= 0 = drop n xs ++ take n xs
             | n < 0 = drop len xs ++ take len xs
                       where len = n + length xs
rotate6 _ _ = error "error!"

-- n の正負による n を反転するしないの処理を最初に行っているだけ
rotate7 :: [a] -> Int -> [a]
rotate7 xs n = let i = if n < 0 then length xs + n else n
               in drop i xs ++ take i xs

rotate8 :: [a] -> Int -> [a]
rotate8 xs n = drop nn xs ++ take nn xs
    where 
      nn = n `mod` length xs

-- これも面白い。与えられたリストの長さを超える数字や、マイナスの数字がきたら n - len、n + len などの処理をする。
-- rotate9 "abcdefgh" 100 などときたら、そのリストの長さを超えるときの処理（2 行目）が再帰的に何度か繰り返される。
rotate9 :: [a] -> Int -> [a]
rotate9 xs n
    | n < 0 = rotate xs (n+len)
    | n > len = rotate xs (n-len)
    | otherwise = let (f,s) = splitAt n xs in s ++ f
    where len = length xs

-- A much simpler solution without using length that is very similar to the first solution:
rotate10 :: [a] -> Int -> [a]
rotate10 [] _ = []
rotate10 x 0 = x
rotate10 x y
       | y > 0 = rotate (tail x ++ [head x]) (y-1)
       | otherwise = rotate (last x : init x) (y+1)

-- rotate11 "abcdefgh" (-2) とすると "hhabcdef" が返ってくるんだけどちゃんと解答校閲してるのか？？
rotate11 :: [a] -> Int -> [a]
rotate11 xs n = rot n xs
    where
           rot 0 = id
           rot 1 = \xs' -> case xs' of [] -> []; ls -> tail ls ++ [head ls]
           rot (-1) = \xs' -> case xs' of [] -> []; ls -> (last xs):init ls
           rot num
              | num > 0 = (rot (num - 1)) . (rot 1) -- ここらへんトリッキー
              | num < 0 = (rot (num + 1)) . (rot (-1))
           rot _ = error "pattern match are non-exhaustive って言われるので"