{-# OPTIONS -Wall -Werror #-}

{-
(**) Calculate Euler's totient function phi(m).

Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

Example in Haskell:

λ> totient 10
4
-}

coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

-- わざわざ special case って書かれてるんだからそれも書く！！
totient :: Int -> Int
totient 1 = 1 -- これ忘れてた。それか、以下の totient m のところで、[1..(m-1)] を [1..m] と書けばこのパターンマッチはいらなくなる（し、他の引数でも正しく動作する）
totient m = length $ filter (\x -> coprime x m) [1..(m-1)] -- これは length $ filter (coprime m) [〜] と書くのと同じ

totient' :: Int -> Int
totient' 1 = 1 -- 忘れてた
totient' m = length [x | x <- [1..(m-1)], coprime m x]

-- 解答
-- 自分の解答とほぼ同じ（totient 1 = 1 のケースを自分は忘れたけど）
totient1 :: Int -> Int
totient1 1 = 1
totient1 a = length $ filter (coprm a) [1..a-1]
    where coprm x y = gcd x y == 1

totient2 :: Int -> Int
totient2 n = length [x | x <- [1..n], coprime x n] -- [1..n] としてることで、引数が 1 の場合もそれ以外の場合も正しい解が得られる

-- 大きい数に対しては、上記のアルゴリズムは遅くなる

-- よくわからない。Euler のトーシェント関数にかんする Wikipedia とか見ればわかるかも。
-- primeFactors はたぶん、引数 n の素因数を返す関数（かな？）。実装がのってないけど。
-- import Data.List (nub)
-- import Data.Ratio
-- totient3 :: (Integral a) => a -> a
-- totient3 1 = 1
-- totient3 n = numerator ratio `div` denominator ratio
--  where ratio = foldl (\acc x -> acc * (1 - (1 % x))) (n % 1) $ nub (primeFactors n)