{-# OPTIONS -Wall -Werror #-}

{-
Given a binary tree as the usual Branch x l r (or Empty).
As a preparation for drawing the tree, a layout algorithm is required to determine the position of each node in a rectangular grid. 
Several layout methods are conceivable, one of them is shown in the illustration below:

In this layout strategy, the position of a node v is obtained by the following two rules:

x(v) is equal to the position of the node v in the inorder sequence
y(v) is equal to the depth of the node v in the tree
Write a function to annotate each node of the tree with a position, where (1,1) in the top left corner or the rectangle bounding the drawn tree.

Here is the example tree from the above illustration:
-}

tree64 :: Tree Char
tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

{-
Example in Haskell:

λ> layout tree64
Branch ('n',(8,1)) (Branch ('k',(6,2)) (Branch ('c',(2,3)) ...
-}

-- イメージ図
-- https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/p64.gif

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

-- 解答
-- 難しい。。

type Pos = (Int, Int)

layout :: Tree a -> Tree (a, Pos)
layout t = fst (layoutAux 1 1 t) -- 最初は、サブツリー全体の左端は、座標全体の左端だから x=1。サブツリーのノードの y 座標は、一番上なので 1。
    where layoutAux :: Int -> Int -> Tree a -> (Tree (a, Pos), Int) -- layoutAux 関数は、サブツリーの左端のノードの x 座標とサブツリーのルートの y 座標、それとサブツリー自体を引数に取り、Tree (a, Pos) とサブツリー内の分岐ノードの数を返す。
          layoutAux x _ Empty = (Empty, x) 
          layoutAux x y (Branch a l r) = (Branch (a, (x', y)) l' r', x'')
            where (l', x')  = layoutAux x (y+1) l
                  (r', x'') = layoutAux (x'+1) (y+1) r

-- The auxiliary function is passed the x-coordinate for the left-most node of the subtree, the y-coordinate for the root of the subtree, and the subtree itself. 
-- It returns the subtree annotated with positions, plus the count of Branch nodes in the subtree.
-- 補助関数には、サブツリーの左端のノードの x 座標、サブツリーのルートの y 座標、サブツリー自体を渡します。この関数は，位置が注釈されたサブツリーと，サブツリー内の分岐ノードの数を返します．