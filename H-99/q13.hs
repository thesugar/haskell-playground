{-# OPTIONS -Wall -Werror #-}

{-
(**) Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method directly. 
I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. 
As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

Example in Haskell:

λ> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

-- 自分の解答
data ListItem a = Single a | Multiple Int a deriving (Show)

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = foldr f []
       where
              f x [] = (Single x):[]
              f x ((Single y):xs)
                     | x == y = (Multiple 2 x):xs
                     | otherwise = (Single x): (Single y): xs
              f x acc@((Multiple n y):xs)
                     | x == y = (Multiple (n+1) y) : xs
                     | otherwise = (Single x) : acc

--- 解答

-- サブリストを明示的に作るなと言われているため、group 関数のように
-- group "aaaabccaadeeee"
-- ["aaaa","b","cc","aa","d","eeee"]
-- となるようなものを経由してはだめ。だが、[(4, 'a'), (1, 'b'), ...] のようなタプルのリストを経由することは可能。
encode' :: Eq a => [a] -> [(Int,a)]
encode' = foldr helper []
    where
      helper x [] = [(1,x)]
      helper x (y@(a,b):ys)
        | x == b    = (1+a,x):ys
        | otherwise = (1,x):y:ys

encodeDirect1 :: Eq a => [a] -> [ListItem a]
encodeDirect1 = map encodeHelper . encode'
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x

-- 別解
encodeDirect2 :: (Eq a) => [a] -> [ListItem a]
encodeDirect2 [] = []
encodeDirect2 (x:xs) = encodeDirect2' 1 x xs

encodeDirect2' :: Eq a => Int -> a -> [a] -> [ListItem a]
encodeDirect2' n y [] = [encodeElement n y] -- ここで、n=1 なら Single 値を使って、そうでないなら Multiple 値を使う、というのを、encodeElement 関数を挟むことでその場合分けを外出ししている
encodeDirect2' n y (x:xs)
       | y == x = encodeDirect2' (n+1) y xs
       | otherwise = encodeElement n y : (encodeDirect2' 1 x xs)

encodeElement :: Int -> a -> ListItem a
encodeElement 1 y = Single y
encodeElement n y = Multiple n y

---

encodeDirect3 :: (Eq a)=> [a] -> [ListItem a]
encodeDirect3 [] = []
encodeDirect3 (x:xs)
    | count == 1  = (Single x) : (encodeDirect xs)
    | otherwise = (Multiple count x) : (encodeDirect rest)
    where
        (matched, rest) = span (==x) xs
        count = 1 + (length matched)
{-
前にも出てきたが span 関数は
       > span (== 'a') "aaabc"
       ("aaa","bc")
なる関数。かなり明瞭に書けますね
-}

---

encodeDirect4 :: Eq a => [a] -> [ListItem a]
encodeDirect4 [] = []
encodeDirect4 (x:xs) = encodeDirectHelper 1 x xs

encodeDirectHelper :: Eq a => Int -> a -> [a] -> [ListItem a]
encodeDirectHelper n x [] = [encodeHelper' (n,x)]
encodeDirectHelper n x xs = 
       if x == (head xs)
            then encodeDirectHelper (n+1) x (tail xs)
            else [encodeHelper' (n,x)] ++ (encodeDirect4 xs)

encodeHelper' :: (Int, a) -> ListItem a
encodeHelper' (1,x) = Single x
encodeHelper' (n,x) = Multiple n x

---

encodeDirect5 :: Eq a => [a] -> [ListItem a]
encodeDirect5 [] = []
encodeDirect5 (x:xs) = let (group, rest) = span (==x) xs in
       convertIfSingle (Multiple (1 + length group) x) : encodeDirect5 rest
       where convertIfSingle (Multiple 1 y) = Single y
             convertIfSingle y = y