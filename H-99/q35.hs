


{-
(**) Determine the prime factors of a given positive integer. 
Construct a flat list containing the prime factors in ascending order.

Example in Haskell:

λ> primeFactors 315
[3, 3, 5, 7]
-}

-- 全然ダメ、以下は正しい答えがまったく出ない
primeFactors_ :: Int -> [Int]
primeFactors_ n = foldr (\m acc -> if (n `div` product acc) `mod` m == 0 then m:(acc >>= primeFactors_) else (acc >>= primeFactors_)) [] [2..(sqrtInt n)] 
    where 
        sqrtInt num | num < 4 = 1
        sqrtInt num = head $ filter (\p -> p*p <= num && num < (p+1)*(p+1)) [2..num] -- これは floor sqrt (fromIntegral 〜)でもいいよね普通に


-- 解答
-- ん〜力技感ある。。あとコーナーケースではバグがあるとかなんとか
primeFactors :: Int -> [Int]
primeFactors a = let (f, f1) = factorPairOf a
                     f' = if prime f then [f] else primeFactors f
                     f1' = if prime f1 then [f1] else primeFactors f1
                 in f' ++ f1'
    where
        factorPairOf p = let f = head $ factors p
                         in (f, p `div` f)
        factors q      = filter (isFactor q) [2..q-1]
        isFactor r s   = r `mod` s == 0
        prime t = null $ factors t

-- 自分が書きたかったのはこういうやつ！
-- ヘルパー関数つくって、そこに n と 2 を与えちゃうんだ、、それで再帰させると
-- は〜なるほど。たしかに、素因数分解を再帰的にするなら絶対はじめは 2 で割れるかどうか試すしなあ
-- [2..√n] みたいなリストを用意して順に処理していかないといけないかと思っていたけど間違いだった。
primeFactors' :: Int -> [Int]
primeFactors' n = primeFactorsHelper n 2
  where
    primeFactorsHelper 1 _ = []
    primeFactorsHelper num f
      | num `mod` f == 0 = f : primeFactorsHelper (num `div` f) f
      | otherwise      = primeFactorsHelper num (f + 1) -- ここで f + 1 で再帰かけることで、次は 3 で試し割りする、と試せる。まあでも数字が大きくなったら無駄も多そうだな〜。無駄な試し割りをめっちゃすることになる

primeFactors'' :: Int -> [Int]
primeFactors'' n = primeFactorsHelper' n 2
  where
    primeFactorsHelper' num f
      | f * f > num      = [num] -- たとえば、3*11 (=33) の素因数分解を考えてみると、f は 2 から start して、3 で割り切れるので二段目のパターンマッチが適用され、3:[pFH' 11 3] を計算することになり、
                                 -- 3 では 11 を割り切れないので otherwise から pFH' 11 4 を計算することになり、このとき 4*4 > 11 なのでもうそれ以上試し割りをすることなく [11] が返り、答えが 3:[11] (つまり [3:11]) になる。
      | num `mod` f == 0 = f : primeFactorsHelper' (num `div` f) f
      | otherwise        = primeFactorsHelper' num (f + 1)

primeFactors1 :: Int -> [Int]
primeFactors1 n = factor primes n
  where 
    factor ps@(p:pt) m | p*p > m      = [m]               
                       | rem m p == 0 = p : factor ps (quot m p) 
                       | otherwise    =     factor pt m
    factor [] _ = []
    primes = 2 : filter (\num -> num == head (factor primes num)) [3,5..] -- ここかなり認知負荷高いんだけど、まあ 2 は素数だから決め打ちとして、それ以降の奇数に関しては、
                                                                          -- num（[3,5..]内の、いま注目している要素）を素因数分解したときに head にくるものであれば素数だよねっていう意味だろう
    --primes = 2 : filter isPrime [3,5..]      -- isPrime of Q.31

{-
mod と rem (remainder; 剰余) は、第二引数が負数だった場合の挙動が異なる。
*Main> 2 `mod` (-3)
-1
*Main> 2 `rem` (-3)
2
https://stackoverflow.com/questions/5891140/difference-between-mod-and-rem-in-haskell

quot は div と似たようなものだが、違いもある。
http://yomi322.hateblo.jp/entry/2012/10/18/204707

-}

primeFactors2 :: Int -> [Int]
primeFactors2 1 = []
primeFactors2 n = let prime = head $ dropWhile ((/= 0) . mod n) [2 .. n]
                  in (prime :) $ primeFactors2 $ n `div` prime

primeFactors2' :: Int -> [Int]
primeFactors2' 1 = []
primeFactors2' n = let divisors = dropWhile ((/= 0) . mod n) [2..(ceiling $ sqrt (fromIntegral n))]
                    in let prime = if null divisors then n else head divisors -- primeFactors2 関数では [2..n] でやってたからそのまま n を拾えたけど、
                                                                              -- 今回は上の行で、ceiling √n までで打ち切ってるから、その場合は divisors が空リストになるのでその場合を if で判定して n を返すようにしてる
                    in (prime :) $ primeFactors2' $ n `div` prime