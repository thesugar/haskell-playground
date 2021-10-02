{-# OPTIONS -Wall -Werror #-}

{-
Collect the leaves of a binary tree in a list

A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list.

Example in Haskell:

λ> leaves tree4
[4,2]
-}

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

tree4 :: Tree Int
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

sampleTree :: Tree Int
sampleTree = Branch 3 (Branch 10 Empty Empty) (Branch 20 Empty Empty)

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ left right) = countLeaves left + countLeaves right

-- 解答例を見たところ、題意をまったく汲み取れていなかったっぽい
-- leaves :: Tree a -> [Int]
-- leaves Empty = [0, 0]
-- leaves (Branch _ Empty Empty) = [1, 1]
-- leaves (Branch _ left right) = let l1:l2:[] = leaves left
--                                    r1:r2:[] = leaves right
--                                in [l1+r1+1, l2+r2]

-- もう一回
-- ✅ ok
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ left right) = leaves left ++ leaves right

-- 解答
leaves' :: Tree a -> [a]
leaves'  Empty                 = []
leaves' (Branch a Empty Empty) = [a]
leaves' (Branch _ left  right) = leaves' left ++ leaves' right

-- 別解
leaves'' :: Tree a -> [a]
leaves'' t = leavesHelper t []
    -- 以下では xs は [] が入る
    -- 結局やってることあんまり変わらないのでは？ 今回は xs に [] が入ることはわかってるんだから、x:xs は [x] って書くのと同じだし。。
    where leavesHelper Empty xs = xs
          leavesHelper (Branch x Empty Empty) xs = x:xs
          leavesHelper (Branch _ l r) xs = leavesHelper l (leavesHelper r xs)