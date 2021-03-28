{-# OPTIONS -Wall -Werror #-}

-- q42 〜 q45 はありません。

{-
(**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations;
e.g. and(A,B) will succeed, if and only if both A and B succeed.

A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).

Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.

Example in Haskell:

λ> table (\a b -> (and' a (or' a b)))
True True True
True False True
False True False
False False False
-}

not' :: Bool -> Bool
not' True  = False
not' False = True

and', or', nand', nor', xor', impl', equ' :: Bool -> Bool -> Bool

and' True True = True
and' _ _ = False

or' False False = False
or' _ _ = True

nand' a b = not' $ and' a b

nor' a b = not' $ or' a b

xor' True True = False
xor' False False = False
xor' _ _ = True

impl' a b = or' (not' a) b

equ' a b = not' $ xor' a b
-- もしくは
-- equ' True True = True
-- equ' False False = True
-- equ' _ _ = False



table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- [True, False], b <- [True, False]]