{-# OPTIONS -Wall -Werror #-}

{-
Collect the nodes at a given level in a list

A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. 
Write a predicate atlevel/3 to collect all nodes at a given level in a list.

Example in Haskell:

λ> atLevel tree4 2
[2,2]
-}

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

tree4 :: Tree Int
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

-- 各ノードのレベルを保存するタプルを作るのか？とか考えてよくわからず、解答見た。
-- めっちゃ簡単。恥。。。
atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x l r) n
    | n == 1 = [x]
    | n > 1 = atLevel l (n-1) ++ atLevel r (n-1)
    | otherwise = []

-- 別解
-- まず、木をレベルごとにリスト化する
-- e.g.) levels tree4 = [[1],[2,2]]
levels :: Tree a -> [[a]]
levels Empty          = [] -- atlevel (木の深さより大きい数字) として [] を出したいなら、ここを repeat [] とすればよい。ただそうすると、levels 関数だけで使う場合はつねに take を使うなりしないと無限に処理が終わらない
levels (Branch a l r) = [a] : zipWith (++) (levels l) (levels r)

atlevel :: Tree a -> Int -> [a]
atlevel t n = levels t !! (n-1)