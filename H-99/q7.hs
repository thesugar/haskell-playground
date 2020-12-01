{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE DeriveFoldable #-}
import Data.Foldable
{-
(**) Flatten a nested list structure.

Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

Example in Haskell:

We have to define a new data type, because lists in Haskell are homogeneous.
[1, [2, [3, 4], 5]] is a type error. Therefore, we must have a way of representing a list that may (or may not) be nested.
Our NestedList datatype is either a single element of some type (Elem a), or a list of NestedLists of the same type. (List [NestedList a]).

```
data NestedList a = Elem a | List [NestedList a]
```

λ> flatten (Elem 5)
[5]
λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
λ> flatten (List [])
[]
-}

data NestedList a = Elem a | List [NestedList a]

-- 自分の答え（❌　わからなかった）
flatten :: NestedList a -> [a]
flatten (List []) = [] -- 🆗
flatten (List [x]) = flatten x
flatten (Elem x) = [x] -- 🆗
flatten (List [Elem x, List xs]) = x:(flatten (List xs))
flatten _ = error "hoge"

-- 解答
flatten1 :: NestedList a -> [a]
flatten1 (Elem a) = [a]
flatten1 (List (x:xs)) = flatten1 x ++ flatten1 (List xs) -- 🔥これ書けなかった
flatten1 (List []) = []
 -- NestedList つまり Elem か List で記述しないといけないと思ってしまい List [Elem 1, Elem2, ...] のような式を List (x:xs) でパターンマッチするという発想に至れなかった（かなりアホ）。
 -- これを書くと List [Elem 1, ELem2, ...] などだけでなく List [Elem1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]] のようなものも再帰で捉えることができる。


flatten2 :: NestedList a -> [a]
flatten2 (Elem x) = [x]
flatten2 (List x) = concatMap flatten2 x
{-
    concatMap はリストの各要素に関数を適用したものを concat する
    > concatMap (++ ", and ") ["bonnie", "clyde", "nikita", "clarence", "alabama"] 
    "bonnie, and clyde, and nikita, and clarence, and alabama, and "
-}

-- concatMap 自体は使わないが concatMap のように振る舞うものを使う例
flatten2' :: NestedList a -> [a]
flatten2' (Elem x) = return x
flatten2' (List x) = x >>= flatten2' -- ちなみにこれは flatten3 <<= x とも書けるようだ（解答例ではそうなっていた）。

flatten2'' :: NestedList a -> [a]
flatten2'' (Elem x) = [x]
flatten2'' (List x) = foldMap flatten x
 -- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
 -- > foldMap (\x -> x ++ "!") ["hoge", "fuga"]
 -- "hoge!fuga!"
 -- うーん foldMap 完全に忘却の彼方だなあ、、すごい H 本第 12 章

flatten2''' :: NestedList a -> [a]
flatten2''' a = flt a []
    where flt (Elem x) xs = x:xs
          flt (List (x:ls)) xs = flt x (flt (List ls) xs)
          flt (List []) xs = xs


--------
flatten3 :: NestedList a -> [a]
flatten3 (Elem x) = [x]
flatten3 (List xs) = foldr (++) [] $ map flatten3 xs -- concatMap の解法と似た発想だと思うけどわかりやすい

-- 以下も、acc (accumulator) という変数名を使っているくらいだから foldr と同じ発想なんだろうけど
-- 定義している rec 関数の rec って何？ receive？　とか reverse 使うことになっちゃう点（具体的にたどっていけばわかるけども）とか認知的負荷高い
flatten4 :: NestedList a -> [a]
flatten4 = reverse . rec []
    where
        rec acc (List []) = acc
        rec acc (Elem x) = x:acc
        rec acc (List (x:xs)) = rec (rec acc x) (List xs)


-- NestedList を Foldable 型クラスのインスタンスにする解法
instance Foldable NestedList where
  foldMap f (Elem x)  = f x
  foldMap _ (List []) = mempty
  foldMap f (List (x:xs)) = foldMap f x `mappend` foldMap f (List xs)

flatten5 :: NestedList a -> [a]
flatten5 = foldMap (\x -> [x])


----
-- DeriveFoldable 拡張を使う。また、toList 関数を使うために Data.Foldale を import すること。
-- {-# LANGUAGE DeriveFoldable #-}
-- import Data.Foldable
data NestedList' a = Elem' a | List' [NestedList' a] deriving Foldable

flatten6 :: NestedList' a -> [a]
flatten6 = toList

-- > flatten6 (List' [Elem' 1, List' [Elem' 2, List' [Elem' 3, Elem' 4], Elem' 5]])
-- [1,2,3,4,5]