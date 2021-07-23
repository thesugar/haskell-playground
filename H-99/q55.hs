{-# OPTIONS -Wall -Werror #-}

{-
(**) Construct completely balanced binary trees

In a completely balanced binary tree, the following property holds for every node: 
The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.

Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes. 
The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.

Example in Haskell, whitespace and "comment diagrams" added for clarity and exposition:

λ> cbalTree 4
[
-- permutation 1
--     x
--    / \
--   x   x
--        \
--         x
Branch 'x' (Branch 'x' Empty Empty) 
           (Branch 'x' Empty 
                       (Branch 'x' Empty Empty)),

-- permutation 2
--     x
--    / \
--   x   x
--      /
--     x
Branch 'x' (Branch 'x' Empty Empty) 
           (Branch 'x' (Branch 'x' Empty Empty) 
                       Empty),

-- permutation 3
--     x
--    / \
--   x   x
--    \
--     x
Branch 'x' (Branch 'x' Empty 
                       (Branch 'x' Empty Empty)) 
           (Branch 'x' Empty Empty),

-- permutation 4
--     x
--    / \
--   x   x
--  /
-- x
Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) 
                       Empty) 
           (Branch 'x' Empty Empty)
]

-}

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

-- 解答
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n - 1) `quotRem` 2 -- quotRem は商 q と余り r を返す。2 で割った余りだから r は 0 （n が奇数（n-1 は偶数）のとき）か 1（n が偶数のとき）。
                                              -- n が奇数の場合、左右の深さは一致し、偶数の場合、片方が 1 だけ深くなる。
    in [Branch 'x' left right | i     <- [q .. q + r],
                                left  <- cbalTree i,
                                right <- cbalTree (n - i - 1)]

-- A slightly more efficient solution, which never creates the same tree twice
-- どの部分が efficient なのかはいまいちわからない
cbalTree' :: Int -> [Tree Char]
cbalTree' 0 = [Empty]
cbalTree' 1 = [Branch 'x' Empty Empty]
cbalTree' n = if n `mod` 2 == 1 then -- n が奇数の場合。左右の深さは一致する。
             [ Branch 'x' left right | left  <- cbalTree' ((n-1) `div` 2),
                                       right <- cbalTree' ((n-1) `div` 2)]
              else                   -- n が偶数の場合。どちらか片方（以下の変数 deeper）が 1 だけ深くなる。
              concat [ [Branch 'x' shallower deeper, Branch 'x' deeper shallower] | shallower <- cbalTree' ((n-1) `div` 2),
                                                                                    deeper <- cbalTree' (n `div` 2)]