{-# OPTIONS -Wall -Werror #-}

{-
(*) Check whether a given term represents a binary tree

Non-solution:

Haskell's type system ensures that all terms of type Tree a are binary trees: 
it is just not possible to construct an invalid tree with this type. Hence, it is redundant to introduce a predicate to check this property: it would always return True.
-}

-- Tree 型を使って tree1 や tree2 といったような二分木を書いて、それが本当に二分木か確かめる問題。
-- Lisp などだと isTree 関数というので確かめるようだが、Haskell だとコンパイルが通れば型システムがその正しさを保証してくれる。

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

tree1 :: Tree Char
tree1 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                               (Branch 'e' Empty Empty))
                   (Branch 'c' Empty
                               (Branch 'f' (Branch 'g' Empty Empty)
                                           Empty))

-- leaf の定義は型シノニム等ではなく、関数。
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

tree1' :: Tree Char
tree1' = Branch 'a' (Branch 'b' (leaf 'd')
                                (leaf 'e'))
                    (Branch 'c' Empty
                                (Branch 'f' (leaf 'g')
                                            Empty))

tree2 :: Tree Char
tree3 :: Tree a
tree4 :: Tree Int
-- A binary tree consisting of a root node only
tree2 = Branch 'a' Empty Empty

-- An empty binary tree
tree3 = Empty

-- A tree of integers
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)
