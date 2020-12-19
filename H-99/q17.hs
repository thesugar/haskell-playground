{-# OPTIONS -Wall -Werror #-}
import Control.Arrow

{-
(*) Split a list into two parts; the length of the first part is given.

Do not use any predefined predicates（事前定義された述語は使わないこと）.

Example in Haskell:

λ> split "abcdefghij" 3
("abc", "defghij")
-}

-- 自分の解答
split :: [a] -> Int -> ([a], [a])
split ls n = helper n ([], []) ls
       where
              helper cnt (acc1, acc2) list@(x:xs)
                     | cnt > 0 = helper (cnt-1) (x:acc1, acc2) xs
                     | otherwise = (reverse acc1, list)
              helper _ _ [] = ([], [])

{-
もし reverse も使わないとしたら
       reverse1 :: [a] -> [a]
       reverse1 = foldl (flip (:)) []
として自分で実装できる。

素朴な reverse は以下だが、++ を使うので効率悪い。
       myReverse :: [a] -> [a]
       myReverse [] = []
       myReverse (x:xs) = (myReverse xs) ++ [x]
-}

-- 解答
-- を見る前に

-- ヒント：take と drop を使う
-- 問題文にある Predicates を使うなっていう述語ってなんだろう
-- https://jutememo.blogspot.com/2008/12/haskell-prelude-2.html

mySplit :: [a] -> Int -> ([a], [a])
mySplit xs n = (take n xs, drop n xs)

-- OK!
-- 別解のヒント: splitAt を使う
-- いや〜こんなの許容したら問題の意味ないでしょ
mySplit' :: [a] -> Int -> ([a], [a])
mySplit' xs n = splitAt n xs

-- あ〜やっぱり。解説ページにも But these should clearly be considered "predefined predicates". （これらは明らかに「定義済み述語」と考えるべきである）と書かれている。

-- 解答

-- 再帰を使う点や n > 0 の場合分けなど自分の解答にも似ているが reverse を使わなくてよかったりとこっちのほうがシンプルでいいな〜
split1 :: [a] -> Int -> ([a], [a])
split1 [] _ = ([], [])
split1 l@(x:xs) n
       | n > 0 = (x:ys, zs)
       | otherwise = ([], l)
              where (ys, zs) = split1 xs (n-1)

-- さらにシンプルな解答✨
split2 :: [a] -> Int -> ([a], [a])
split2 (x:xs) n
       | n > 0 = let (f, l) = split2 xs (n-1) in (x:f, l)
split2 xs _ = ([], xs) -- n = 0 の場合や空リスト、単一要素リストに対して適用する場合など。エラー回避目的のみならず、再帰の base case にもなっている。

{-
split2 "abc" 2 を考えると、最初は n = 2 > 0 だから
split2 "abc" 2 = let (f, l) = split2 "bc" 1 in ('a':f, l)
となる。

じゃあ split2 "bc" 1 を計算しないと、となる。
split2 "bc" 1 = let (f', l') = split2 "c" 0 in ('b': f', l')

となったら split2 "c" 0 を計算しないといけない。
split2 "c" 0 = ([], "c") -- base case のほう

ではそれを逆にたどって、
split2 "bc" 1 = let (f', l') = split2 "c" 0 in ('b': f', l') より ('b': [], "c") つまり ("b", "c")

split2 "abc" 2 = let (f, l) = split2 "bc" 1 in ('a':f, l) より ('a': "b", "c") つまり ("ab", "c")

これが答えになる。
-}

-- Or (ab)using the "&&&" arrow operator for tuples:
split3 :: [a] -> Int -> ([a], [a])
split3 (x:xs) n | n > 0 = (:) x . fst &&& snd $ split3 xs (n - 1)
split3 xs _ = ([], xs)

{-
~~~ &&& の使い方 ~~~
id &&& reverse $ "abc"
("abc","cba")

今回の場合だと、以下のような計算が出現することになる。
> ((:) 'a') . fst &&& snd $ ("b", "c")
("ab","c")
-}

-- foldl を使った解法
-- 求めたい結果は (split 前半の要素、split 後半の要素) というタプルだが、
-- この解法は、n も使って (split 前半の要素、 split 後半の要素, n) をアキュムレータにして畳み込んでいく。
-- それで最後にこのタプルの最初の 2 つを取り出すというわけだ。
-- foldl を使っていて効率とかは悪そうな気がするけど面白い
split4 :: [a] -> Int -> ([a], [a])
split4 [] _ = ([], [])
split4 list n
  | n < 0 = (list, [])
  | otherwise  = (myfirst output, mysecond output)
    where output = foldl (\acc e -> if mythird acc > 0 then (myfirst acc ++ [e], mysecond acc, mythird acc - 1) else (myfirst acc, mysecond acc ++ [e], mythird acc)) ([], [], n) list

myfirst :: (a, b, c) -> a  
myfirst (x, _, _) = x  
  
mysecond :: (a, b, c) -> b  
mysecond (_, y, _) = y  
  
mythird :: (a, b, c) -> c  
mythird (_, _, z) = z

-- タプル抽出関数を定義しない解法
-- まずこの q16 でも話題になった（自分の中で）n を利用するときに n-1 として 0 まで降ろしていくタイプの再帰か、0 からはじめて n まであげていくタイプの再帰にするか、ここでは上げていくタイプにしている
-- 今回は (インデックス, (最終ほしいペア)) をアキュムレータにして、最後に snd (インデックス, (最終ほしいペア)) によりほしい結果を取り出している。
split5 :: [a] -> Int -> ([a],[a])
split5 lst n = snd $ foldl helper (0, ([], [])) lst
    where helper (i, (l, r)) x = 
           if i >= n
              then (i+1, (l, r ++ [x]))
              else (i+1, (l ++ [x], r))

--A solution that dequeues onto a stack and then reverses at the end:
-- 自分の解答にも似てると言っていいんじゃないかな　reverse を使っているし、、
split6 :: [a] -> Int -> ([a], [a])
split6 xs n = let (a, b) = helper [] xs n in (reverse a, b)
  where helper lft rgt@(r:rs) num
         | num == 0 = (lft, rgt)
         | otherwise = helper (r:lft) rs (num-1)
        helper _ _ _ = error "unknown"


-- fst, snd を使うことで split 関数による再帰をタプルの左にも右にも書いてるんだ。。
split7 :: [a] -> Int -> ([a],[a])
split7 [] _ = ([],[])
split7 (x:xs) n
  | n > 0  = (x: (fst (split7 xs (n-1))), snd (split7 xs (n-1)))
  | n <= 0 = (fst (split xs 0), x:(snd (split xs 0)))
split7 _ _ = error "unknown"

-- これもシンプル？　といえばシンプル。
split8 :: [a] -> Int -> ([a], [a])
split8 xs n = if n < 0 then ([], xs) else splitR n xs []
    where
        splitR 0 ys accum = (reverse accum, ys)
        splitR _ [] accum = (reverse accum, [])
        splitR num (y:ys) accum = splitR (num-1) ys (y:accum)