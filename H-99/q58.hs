{-# OPTIONS -Wall -Werror #-}

{-
Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.

Example in Haskell:

λ> symCbalTrees 5
[Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]
-}

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

symmetric :: (Eq a) => Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ left right) = mirror left right
    where
        mirror Empty Empty = True
        mirror (Branch _ l r) (Branch _ l' r') = mirror l r' && mirror l' r
        mirror _ _ = False

-- q55 より
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n - 1) `quotRem` 2
    in [Branch 'x' left right | i     <- [q .. q + r],
                                left  <- cbalTree i,
                                right <- cbalTree (n - i - 1)]

symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = filter symmetric $ cbalTree n

-- 解答
-- こちらのほうが効率がよい。
-- 偶数の場合はシンメトリックになりえないためすぐに [] を返す
-- 奇数の場合、Branch 'x' t (reverseTree t) を返す。t は要素数が n `div` 2（n は奇数なので (n-1) / 2）の平衡木。
symCbalTrees' :: Int  -> [Tree Char]
symCbalTrees' n = if n `mod` 2 == 0 then [] else
    [ Branch 'x' t (reverseTree t) | t <- cbalTree (n `div` 2)]
    where
        reverseTree Empty = Empty
        reverseTree (Branch x l r) = Branch x (reverseTree r) (reverseTree l)