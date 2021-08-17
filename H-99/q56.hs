{-# OPTIONS -Wall -Werror #-}

{-
(**) Symmetric binary trees

Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. 
Write a predicate symmetric/1 to check whether a given binary tree is symmetric. 

Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.

Example in Haskell:

λ> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
False
λ> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
True
-}

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

symmetric :: (Eq a) => Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ left right) = mirror left right
    where
        mirror Empty Empty = True
        mirror (Branch _ l r) (Branch _ l' r') = mirror l l' && mirror r r' -- ❌ 惜しい！ mirror（鏡像）判定だから、 mirror l l' && mirror r r' ではなく mirror l r' && mirror l' r が正しい。
        mirror _ _ = False

-- 解答
-- 自分の答えとほぼ同じ（鏡像判定の部分は自分の解答が間違い）
mirror' :: Tree a -> Tree a -> Bool
mirror' Empty          Empty          = True
mirror' (Branch _ a b) (Branch _ x y) = mirror' a y && mirror' b x
mirror' _              _              = False

symmetric' :: Tree a -> Bool
symmetric' Empty          = True
symmetric' (Branch _ l r) = mirror' l r


-- even simpler is:
symmetric'' :: Tree a -> Bool
symmetric'' t = mirror' t t