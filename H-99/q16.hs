{-# OPTIONS -Wall -Werror #-}

{-
(**) Drop every N'th element from a list.

Example in Haskell:

λ> dropEvery "abcdefghik" 3
"abdeghk"
-}

-- n-1 個取って、その次の、先頭から n 番目にあたるやつまで捨てて、また n-1 個取って、を繰り返す
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery ls n = take (n-1) ls ++ dropEvery ((drop n) ls) n  

-- zip 関数で [(1, 'a'), (2, 'b'), (3, 'c'), ...] のようなペアのリストを作って、n の倍数とペアになっているものは捨てる
dropEvery' :: [a] -> Int -> [a]
dropEvery' ls n = foldr picker [] (zip [1..] ls)
       where
              picker (num, elm) acc
                     | num `mod` n == 0 = acc
                     | otherwise = elm:acc

-- 解答

-- dropEv というヘルパー関数を使う。
-- dropEv は、（今回の問題でいう）3 のような、drop したいステップ数にあたる数をとる (-> num)。
-- それで、i という引数も取るが i には 1 を渡している。
-- i `mod` num を計算して、余りが 0 なら []（つまりそのときの要素は drop させる）、そうでないならそのときの要素は拾う（drop しない）。
-- i は 1 から始めている。今回の問題の例でいうと、num = 3 なので、i が 1 のときは 1 `mod` 3 = 1 (≠ 0) ゆえ拾う、i が 2 のときも拾う、3 のときは落とす、4 のときは拾う、、、という感じで進んでいく。
dropEvery1 :: [a] -> Int -> [a]
dropEvery1 [] _ = []
dropEvery1 (x:xs) n = dropEv (x:xs) n 1 where
       dropEv (y:ys) num i = (if (num `divides` i) then [] else [y]) ++ (dropEv ys num (i+1))
       dropEv [] _ _ = []
       divides a b = b `mod` a == 0

-- あーなるほど。dropEvery n と言われると、先頭から 1 つ目は残す、2 つ目も残す、...、n 個目は捨てる、というような発想になりがちだけど
-- これは先頭のとき n、2 つ目は (n-1)、と番号づけしていくイメージで、n 番目にあたるとき（helper (_:xs) count 1）は要素を捨てて巻き直す、みたいな感じ。
-- まあ、上の dropEvery1 では dropEv という関数に n 1 の部分で 1 を渡して、dropEv の実装では ~~ ++ ~~ (i+1) となってるから数字をのぼっていく再帰になってるけど
-- dropEvery2 では cnt cnt というふうに、本問でいう 3 のような数字をそのまま渡してそこから降りていく再帰になっている。どっちでも書けるけど、好みの問題かな。
dropEvery2 :: [a] -> Int -> [a]
dropEvery2 list cnt = helper list cnt cnt
  where helper [] _ _ = []
        helper (_:xs) count 1 = helper xs count count
        helper (x:xs) count n = x : (helper xs count (n - 1))

-- これも似た解答。
-- 上記 dropEvery1, 2 に出てきたヘルパー関数はそれ単体で完結していたけど
-- ここに出てくるヘルパー関数は dropEvery3 が取る引数の n に依存している（helper 関数自体は n という引数を取らずに、でも処理の中で n を使う）
-- まあでもすっきり書けていいんじゃないかなー。
dropEvery3 :: [a] -> Int -> [a]
dropEvery3 ls n = helper ls n
       where
              helper [] _ = []
              helper (_:xs) 1 = helper xs n
              helper (x:xs) k = x : helper xs (k-1)

-- これも、helper 関数のガードで i == n の判定に使う n を dropEvery4 がもらう引数に依存している。
-- i == n かどうかの処理で場合分け、という書き方もわかりやすい（i == n に達したらまた i を 1 に戻す、ということで mod を使わずに同じことをできる）
dropEvery4 :: [a] -> Int -> [a]
dropEvery4 ls n = helper ls 1
       where
              helper [] _ = []
              helper (x:xs) i
                     | i == n  = helper xs 1
                     | i /= n  = x:helper xs (i + 1)
              helper _ _ = error "unknown error" -- コンパイルエラー回避

-- 自分の最初の解答と同じ。drop させる実装なのに Prelude の drop 関数使ったらそりゃきれいに書けるわという話でもあるが、まあきれいだよね。
dropEvery5 :: [a] -> Int -> [a]
dropEvery5 [] _ = []
dropEvery5 list count = (take (count-1) list) ++ dropEvery (drop count list) count

-- [] _ じゃなくて　length xs < n という場合分けにしたということね
dropEvery6 :: [a] -> Int -> [a]
dropEvery6 xs n
       | length xs < n = xs
       | otherwise     = take (n-1) xs ++ dropEvery6 (drop n xs) n

-- 書き方がテクニカルだけど、そんなに難しくもない。引数を省いたスタイルで書かれていることに注意。
-- cycle [1..n] は、たとえば cycle [1,2,3] なら [1,2,3,1,2,3,1,2,3,...] の無限リストを作ってくれる。
-- 自分の zip を使う解答では [1..] のリストを使って mod で n (= 3) の倍数を判定したけど、cycle を使うと = か /= かを見ればよくなる。
dropEvery7 :: [a] -> Int -> [a]
dropEvery7 = flip $ \n -> map snd . filter ((n/=) . fst) . zip (cycle [1..n])

-- zip とリスト内包表記を使った例。リスト内包もシンプルに書けるな〜。
dropEvery8 :: [a] -> Int -> [a]
dropEvery8 xs n = [ x | (x,c) <- ( zip xs [1,2..]), (mod c n) /= 0]

-- A more complicated approach which first divides the input list into sublists that do not contain the nth element, 
-- and then concatenates the sublists to a result list (if not apparent: the author's a novice):
-- 作者は初心者です。とか書かれてるの何？🤔　まあ読みづらいコードであることはたしか。。
dropEvery9 :: [a] -> Int -> [a]
dropEvery9 [] _ = []
dropEvery9 xs n = concat (split n xs)
 where
  split _ [] = []
  split cnt ys = fst splitted : split cnt ((safetail . snd) splitted)
   where
    splitted = splitAt (n-1) ys
    safetail ls
       | null ls = []
       | otherwise = tail ls

-- まず思いつくような解法。といっても素直で直観的でいいけどね
dropEvery10 :: [a] -> Int -> [a]
dropEvery10 xs n = map fst $ filter (\(_,i) -> i `mod` n /= 0) $ zip xs [1..]

-- filter 関数
-- というか、引数を省略してないからそれに伴って flip を使わなかったり、snd が fst になったり fst が snd になったりしてるだけで dropEvery7 と同じじゃん
dropEvery11 :: [a] -> Int -> [a]
dropEvery11 xs n = map fst $ filter ((n/=) . snd) $ zip xs (cycle [1..n])

-- あー、fold を使う際、アキュムレータとして (n, []) のようなペアを使うと。んで、畳み込むごとに n を減算していって、その数字が 1 になったらまた n に戻すと（dropEvery2 などと同じ）。
dropEvery12 :: Int -> [a] -> [a]
dropEvery12 n xs = snd $ foldl (\acc e -> if fst acc > 1 then (fst acc - 1, snd acc ++ [e]) else (n, snd acc)) (n, []) xs

-- length ls を使っている。
dropEvery13 :: [a] -> Int -> [a]
dropEvery13 ls n = fst $ foldr (\x (xs, i) -> ((if mod i n == 0 then xs else x:xs), i - 1)) ([], length ls) ls

-- unzip を使っている
-- unzip [(1,'a'), (2, 'b'), (3, 'c')]
-- ([1,2,3],"abc")
dropEvery14 :: [a] -> Int -> [a]
dropEvery14 xs k = snd . unzip . filter (\(i, _) -> i `mod` k /= 0) . zip [1..] $ xs

-- これも 1 からはじめて、drop する数（本問では 3）になったら 1 に戻すっていう発想
-- foldl を使うとやっぱり ++ が必要になる
dropEvery15 :: [a] -> Int -> [a]
dropEvery15 lst n = snd $ foldl helper (1, []) lst
       where helper (i, acc) x = if n == i
              then (1, acc)
              else (i+1, acc++[x])