{-# OPTIONS -Wall -Werror #-}

{-
(**) Binary search trees (dictionaries)

Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers.

Example in Haskell:

λ> construct [3, 2, 5, 7, 1]
Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))

λ> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
True

λ> symmetric . construct $ [3, 2, 5, 7, 1]
True
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

-- 使わない
-- quickSort :: [Int] -> [Int]
-- quickSort [] = []
-- quickSort (x:xs) = lessOrEqualTo x ++ [x] ++ greaterThan x
--     where
--         lessOrEqualTo t = quickSort [y | y <- xs, y <= t]
--         greaterThan   t = quickSort [y | y <- xs, y >  t] 

-- 解答
add :: Ord a => a -> Tree a -> Tree a
add x Empty = Branch x Empty Empty
add x t@(Branch y l r) = case compare x y of
                            LT -> Branch y (add x l) r
                            GT -> Branch y l (add x r)
                            EQ -> t

construct :: [Int] -> Tree Int
construct xs = foldr add Empty $ reverse xs
-- もしくは foldl (flip add) Empty xs