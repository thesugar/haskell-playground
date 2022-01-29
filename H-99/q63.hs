{-# OPTIONS -Wall -Werror #-}
import Data.List (group)

{-
Construct a complete binary tree

A complete binary tree with height H is defined as follows:

The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". 
This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors (the nil's which are not really nodes!) come last.
Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.

We can assign an address number to each node in a complete binary tree by enumerating the nodes in level-order, starting at the root with number 1. 
For every node X with address A the following property holds: The address of X's left and right successors are 2*A and 2*A+1, respectively, if they exist. 
This fact can be used to elegantly construct a complete binary tree structure.

Write a predicate complete_binary_tree/2.

Example in Haskell:

λ> completeBinaryTree 4
Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)

λ> isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
True
-}

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

-- 解答
-- import Data.List

filled :: Tree a -> [[Bool]]
filled Empty = repeat [False]
filled (Branch _ l r) = [True] : zipWith (++) (filled l) (filled r)

-- The address of X's left and right successors are 2*A and 2*A+1 がポイント
--   X
-- l  r
-- について、l が 2A で r が 2A + 1 という意味。
-- これを利用すれば書ける。
completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = generate_tree 1
    where generate_tree x
            | x > n = Empty
            | otherwise = Branch 'x' (generate_tree (2*x))
                                     (generate_tree (2*x+1))
                                 
isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree Empty = True
isCompleteBinaryTree t = and $ last_proper : zipWith (==) lengths powers -- lengths と powers の比較は、各階層のノード数が [1,2,4,8,16,...] と 2 の n 乗になっていることを確かめようとしている
    where levels = takeWhile or $ filled t
          -- The upper levels of the tree should be filled.                                                                                                      
          -- Every level has twice the number of nodes as the one above it,                                                                                      
          -- so [1,2,4,8,16,...]      
          lengths = map (length . filter id) $ init levels
          powers = iterate (2*) 1
          -- The last level should contain a number of filled spots,                                                                                             
          -- and (maybe) some empty spots, but no filled spots after that!    
          last_filled = map head $ group $ last levels
          last_proper = head last_filled && (length last_filled) < 3