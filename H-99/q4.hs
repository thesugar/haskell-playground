{-# OPTIONS -Wall -Werror #-}

{-
Problem 4
(*) Find the number of elements of a list.

Example in Haskell:

λ> myLength [123, 456, 789]
3
λ> myLength "Hello, world!"
13
-}

-- 自分の答えその 1 
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = myLength xs + 1

-- 自分の答えその 2
myLength' :: [a] -> Int
myLength' = foldr (\_ -> (+ 1)) 0

-- 自分の答えその 3 
myLength'' :: [a] -> Int
myLength'' x = fst . last $ zip [1..] x

-- 解答
-- 再帰を使ったシンプルな答え（自分の答えその 1 と同じ）
myLength1           :: [a] -> Int
myLength1 []        =  0
myLength1 (_:xs)    =  1 + myLength xs

-- やっていることは同じだけど、「アキュムレータ」を定義して、リストから要素を取り出してアキュムレータに +1 ずつ加算していく（まさにアキュムレートする）ことを再帰的にやる感じ
myLength2 :: [a] -> Int
myLength2 list = myLength_acc list 0
    where myLength_acc [] n = n
          myLength_acc (_:xs) n = myLength_acc xs (n+1)


myLength3_1 :: [a] -> Int
myLength3_2 :: [a] -> Int
myLength3_3 :: [a] -> Int
myLength3_4 :: [a] -> Int
myLength3_5 :: [a] -> Int
myLength3_6 :: [a] -> Int

myLength3_1 = foldl (\n _ -> n + 1) 0 -- 0 を初期値として、要素と畳み込むたびに +1 していくという発想で、自分の答えその2と発想は同じ
myLength3_2 = foldr (\_ n -> n + 1) 0 -- myLength3_1 と myLength3_2 はどっちから畳み込むかの違いだけ
myLength3_3 = foldr (\_ -> (+1)) 0 -- 自分の答えその 2 とまったく同じ
myLength3_4 = foldr ((+) . (const 1)) 0 -- 「\_ -> (+1)」（何がきても +1 する関数として扱う）は (+) . (const 1) と同じ。const foo bar は foo になる。
myLength3_5 = foldr (const (+1)) 0 -- これも。関数合成 (+) . (const 1) は「何がきても 1 を返し、それを加算と合成する」というものだったが、なにがきても (+1) 関数を返すという書き方をしたもの
myLength3_6 = foldl (const . (+1)) 0 -- これも、foldr と foldl の違いによる記法の違いだけで、中身は同じ。たとえば func = const . (+1) として func (func 0 'a') 'b' とすれば 2 が結果として返ってくる。

-- 無限リストとの zipping（自分の答えその 3 と同じ）
myLength4_1 :: [a] -> Int
myLength4_2 :: [a] -> Int
myLength4_3 :: [a] -> Int
myLength4_1 xs = snd $ last $ zip xs [1..] -- xs と [1..] の順番が違うだけで自分の答えその 3 とオナjい
myLength4_2 = snd . last . (flip zip [1..]) -- ポイントフリースタイル
myLength4_3 = fst . last . zip [1..] -- 同じ。flip する必要がないのでこっちのほうが 4_2 より楽。

-- すべての要素を 1 に mapping
myLength5 :: [a] -> Int
myLength5 = sum . map (\_ -> 1) -- foldl/foldr を使った解法と基本的な発想は同じ。