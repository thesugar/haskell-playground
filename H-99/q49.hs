{-# OPTIONS -Wall -Werror #-}

import Control.Monad (replicateM)

{-
(**) Gray codes.

An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,

n = 1: C(1) = ['0','1'].
n = 2: C(2) = ['00','01','11','10'].
n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
Find out the construction rules and write a predicate with the following specification:

% gray(N,C) :- C is the N-bit Gray code
Can you apply the method of "result caching" in order to make the predicate more efficient, when it is to be used repeatedly?

Example in Haskell:

λ> gray 3
["000","001","011","010","110","111","101","100"]

-}

gray :: Int -> IO () -- 返り値が IO () になってしまう
gray n = print $ replicateM n ['0', '1']

gray' :: Int -> [String]
gray' 0 = [""]
gray' n = ['0','1'] >>= (\x -> map (x:) (gray' (n-1))) -- 順番も問題の指定どおりにするなら、このラムダ式を where とかで切り出して、x が 1 なら gray' 〜 の返り値として得られるリストに reverse をかけるようにすればいい
                     -- モナドを使って書いてみた。リストは非決定性計算！

-- 解答
gray1 :: Int -> [String]
gray1 0 = [""]
gray1 n = let xs = gray1 (n-1) in map ('0':) xs ++ map ('1':) (reverse xs) -- こうすると、順番も問題の指定どおり（自分の解答だと、順番は再現できていなかった）

-- 別解
gray2 :: Int -> [String]
gray2 0 = [""]
gray2 n = [ '0' : x | x <- prev ] ++ [ '1' : x | x <- reverse prev ]
  where prev = gray2 (n-1)


-- さらに別解
-- 返り値の順番は問題文のとおりにはなっていない。
gray3 :: Int -> [String]
gray3 0 = [""]
gray3 n = foldr (\s acc -> ("0" ++ s):("1" ++ s):acc) [] $ gray3 (n-1)