{-# OPTIONS -Wall -Werror #-}

{-
Collect the internal nodes of a binary tree in a list

An internal node of a binary tree has either one or two non-empty successors. 
Write a predicate internals/2 to collect them in a list.

Example in Haskell:

λ> internals tree4
[1,2]
-}

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

tree4 :: Tree Int
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x left right) = x:(internals left ++ internals right)

-- 解答
-- 自分の答えとまったく同じ！
internals' :: Tree a -> [a]
internals' Empty                  = []
internals' (Branch _ Empty Empty) = []
internals' (Branch a left right ) = a : internals' left ++ internals' right