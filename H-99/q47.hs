{-# OPTIONS -Wall -Werror #-}

{-
(*) Truth tables for logical expressions (2).

Continue problem P46 by defining and/2, or/2, etc as being operators. 
This allows to write the logical expression in the more natural way, as in the example: A and (A or not B). Define operator precedence as usual; i.e. as in Java.

Example in Haskell:

λ> table2 (\a b -> a `and'` (a `or'` not b))
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

table2 :: (Bool -> Bool -> Bool) -> IO ()
table2 f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- [True, False], b <- [True, False]]