{-# OPTIONS -Wall -Werror #-}

{-
(**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.

Example in Haskell:

λ> [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
[9,3,3]
-}

{-
ユークリッドの互除法:
  割り算の等式：a=bq+r において，
  「a と b の最大公約数」
  ＝「b と r の最大公約数」
-}

myGCD :: Int -> Int -> Int
myGCD n m = let remainder = n `mod` m
            in if remainder == 0 then abs m
               else myGCD m remainder

-- 解答
-- 素直な gcd' を作って、符号処理を含めて myGCD' で wrap してる
gcd' :: Int -> Int -> Int
gcd' 0 y = y
gcd' x y = gcd' (y `mod` x) x

myGCD' :: Int -> Int -> Int
myGCD' x y | x < 0     = myGCD (-x) y
          | y < 0     = myGCD x (-y)
          | y < x     = gcd' y x
          | otherwise = gcd' x y


-- 別解
-- 自分の解答と同じ。でも自分の解答だと remainder という変数を用意してそれが 0 かどうかで条件分岐したけど
-- 以下のように b == 0 ならで場合分け（ガード）するのもスマートだな。。再帰だから同じことになる。
-- あと、自分の解答 myGCD だと、myGCD (何か) 0 とするとゼロ割りエラーが出る（パターンマッチやガードなどで防げばいいのだが）し、
-- myGCD' も、myGCD' のガードから、myGCD' 20 0 のような場合は gcd' 0 20 が処理されてゼロ割りにならないのだけど（第 2 引数が割る数だから）、
-- myGCD' (-20) 0 のような場合は gcd' (-20) 0 が処理されるためゼロ割りになる。
-- でも、以下の解答だと b == 0 は即 abs a を返すからゼロ割りは起こらない。
myGCD'' :: Integer -> Integer -> Integer
myGCD'' a b
      | b == 0     = abs a
      | otherwise  = myGCD'' b (a `mod` b)