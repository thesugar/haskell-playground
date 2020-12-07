{-# OPTIONS -Wall -Werror #-}
import Data.List(group)

{-
(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. 
Only elements with duplicates are transferred as (N E) lists.

Example in Haskell:

λ> encodeModified "aaaabccaadeeee"
[Multiple 4 'a', Single 'b', Multiple 2 'c',
 Multiple 2 'a', Single 'd', Multiple 4 'e']
-}

-- 自分の解答たち

data Duplable a = Single a | Multiple Int a deriving (Show)

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) $ group xs

encodeModified :: Eq a => [a] -> [Duplable a]
encodeModified xs = map (\x -> if length x == 1 then (Single (head x)) else (Multiple (length x) (head x))) $ group xs

encodeModified' :: Eq a => [a] -> [Duplable a]
encodeModified' ls = [if xs == [] then Single x else (Multiple (length xs + 1) x) | (x:xs) <- group ls]

encodeModified'' :: Eq a => [a] -> [Duplable a]
encodeModified'' xs = foldr f [] $ group xs
    where
        f (y:[]) acc = (Single y):acc
        f ls@(y:_) acc = (Multiple (length ls) y):acc
        f [] _ = []



-- 解答

-- encode :: Eq a => [a] -> [(Int, a)]
-- encode xs = map (\x -> (length x, head x)) $ group xs
encodeModified1 :: Eq a => [a] -> [Duplable a]
encodeModified1 = map encodeHelper . encode
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x
      -- encode 関数の考え方を使うっていうより、encode 関数の結果をそのまま利用してしまえば早いわけか。。

{-
Again, like in problem 7, we need a utility type because lists in haskell are homogeneous. Afterwards we use the encode function from problem 10 and map single instances of a list item to Single and multiple ones to Multiple.
問題 7 でもそうだったが、Haskell におけるリストは同型のものしか受け付けないので、自分で型を定義する必要がある。その後、問題 10 で作成した encode 関数を使って、リストに含まれる単一のインスタンスは Single 値に、複数のインスタンスは Multiple 値にマッピングする。

The ListItem definition contains 'deriving (Show)' so that we can get interactive output.
対話的な出力を得られるよう、ListItem （自分の解答では Duplable）の定義には 'deriving (Show)' を含めるようにする。

This problem could also be solved using a list comprehension like so:
この問題は a list comprehension（リスト内包表記）を使って以下のようにも解ける:
-}

encodeModified2 :: Eq a => [a] -> [Duplable a]
encodeModified2 xs = [y | x <- group xs, let y = if (length x) == 1 then Single (head x) else Multiple (length x) (head x)]
