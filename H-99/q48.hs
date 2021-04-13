{-# OPTIONS -Wall -Werror #-}

import Control.Monad (replicateM)

{-
(**) Truth tables for logical expressions (3).

Generalize problem P47 in such a way that the logical expression may contain any number of logical variables. 
Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List.

Example in Haskell:

λ> tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- infixl 3 `equ'`
True  True  True  True
True  True  False True
True  False True  True
True  False False True
False True  True  True
False True  False True
False False True  True
False False False True

-- infixl 7 `equ'`
True  True  True  True
True  True  False True
True  False True  True
True  False False False
False True  True  False
False True  False False
False False True  False
False False False False
-}

infixl 3 `equ'`

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

-- tablen の書き方がわからなかったので答え見た。
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [toStr a ++ " " ++ show (f a) | a <- args n]
    where args num = replicateM num [True, False]
          toStr = unwords . map (\x -> show x ++ space x)
          space True = "  "
          space False = " "

{-
🐛 replicateM 関数

       ✅ アクションに対して使う replicate。指定された回数だけアクションから値を取り出してリスト化する。

       import Control.Monad
       import System.Random

       dice :: IO Int
       dice = getStdRandom $ randomR (1, 6)

       main = do
              print $   replicate  5 1
              print =<< replicateM 5 (return 1)
              print =<< replicateM 5 dice

       -- 結果（dice の結果は毎回異なる）
       [1,1,1,1,1]
       [1,1,1,1,1]
       [4,6,2,1,2]

🔠 unwords

    unwords :: [String] -> String
    unwords ["i", "love", "you"]
    "i love you"

    文字列のリストをくっつけてひとつの文字列にする
-}