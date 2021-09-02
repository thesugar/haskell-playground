{-# OPTIONS -Wall -Werror #-}

{-
Count the leaves of a binary tree

A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.

Example in Haskell:

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

λ> countLeaves tree4
2
-}

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

tree4 :: Tree Int
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ left right) = countLeaves left + countLeaves right