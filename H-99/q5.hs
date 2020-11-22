{-# OPTIONS -Wall -Werror #-}

{-
(*) Reverse a list.

Example in Haskell:

λ> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
λ> myReverse [1,2,3,4]
[4,3,2,1]
-}

test :: (String -> String) -> ([Int] -> [Int]) -> Bool
test solution solution' = and [test1 solution, test2 solution']
    where test1 sol = ((sol "A man, a plan, a canal, panama!") == "!amanap ,lanac a ,nalp a ,nam A")
          test2 sol' = ((sol' [1 :: Int, 2 :: Int, 3 :: Int, 4 :: Int]) == [4 :: Int, 3 :: Int, 2 :: Int, 1 :: Int])
    -- myReverse 関数をテストしたいときは test myReverse myReverse と書く。
    -- （こんなアホコードを書くしかないのか。。？？　test myReverse だけでテストできてほしいけど（エラーが出る））

-- 自分の答えその 1
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 自分の答えその 2
myReverse' :: [a] -> [a]
myReverse' = foldl (\acc x -> x:acc) []

--- 解答
-- Prelude の実装も以下のようになっているらしい。
reverse1 :: [a] -> [a]
reverse1 = foldl (flip (:)) [] -- flip は引数の順番を入れ替えるから、`:` がふつう 要素:リスト という順番で取るところを リスト:要素 とできて foldl が使えるようになる（foldl は acc x の順番で取るから）

-- 自分の答えその 1 と同じ。
-- 可読性は高いが、計算の効率は悪い。アキュムレートされるたびに繰り返しリストを結合する（そのときに以下の xs に相当するリストがたぶんすべて走査されてしまう）から。
reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (x:xs) = reverse xs ++ [x]

-- この計算効率の悪さを回避する書き方は以下
-- 理解までにちょっと考えるけど、そんなに難解でもない。おもしろい。++ を使わず : でやれてるからよさそう
reverse3 :: [a] -> [a]
reverse3 list = reverse3' list []
    where
        reverse3' [] reversedList = reversedList
        reverse3' (x:xs) reversedList = reverse3' xs (x:reversedList)

reverse4 :: [a] -> [a]
--reverse4 xs = foldr (\x fId empty -> fId (x : empty)) id xs $ []
reverse4 xs = foldr (\x id' accumulatedList -> id' (x : accumulatedList)) id xs $ []
    -- foldr (\x fId empty -> fId (x : empty)) id xs は関数を返す。それを [] に適用している。
    -- foldr :: (a -> b -> b) -> b -> [a] -> b
    -- かなりわかりづらい。。まだ理解できてない。。。

myReverse'' :: [a] -> [a]
myReverse'' xs = foldr (\x acc -> acc . (x:)) id xs [] -- うーーん。
-- 基本的（かつ具体例で言うと）には、1:2:3:[] を (3:).(2:).(1:) という関数に変換して [] に適用するって言う発想。
-- xs から 1 つずつ要素を取って、(蓄積された関数たち) . (x:) として合成していく。
-- まあなんだかわかりかけるようなそうでないようなモヤモヤ感。。。

{-
参考：https://stackoverflow.com/questions/55672079/foldr-with-4-arguments


Because foldr ... xs returns a function, which takes a list and then populates it with elements from xs.
Initially this function is id (which means that it doesn't change the list).
Then each foldr iteration modifies the function: it adds the current element and then performs the rest (i.e. adds already seen elements).

このように考えてみよう。すべての関数は正確にただ 1 つだけの引数を受け取る。それは別の関数（1 つだけ関数を受け取る）を返すかもしれない。
複数の引数による呼び出しに見える以下のような記述は

```
f a b c
```

正確には次のように解釈される。

```
((f a) b) c
```

つまり、単一引数による関数適用のチェーンだということだ。関数の型

```
f :: a -> b -> c -> d
```

は以下のように分解される。

```
f :: a -> (b -> (c -> d))
```

すなわち、関数を返す関数を返す関数だということである。我々はふつうそれを 3 引数の関数とみなす。
しかしその 3 引数の関数が 3 つ以上の引数を受け取ることはあるだろうか？　ある。`b` が別の関数の型だった場合だ。

これがまさに今回の `fold` の例で起こっていることだ。`foldr` へ最初の引数として渡した関数は 3 つの引数を取る。
これは 2 つの引数を取って別の関数を返すということと同じである。今、簡略化した `foldr` の型は以下のようである:

```
foldr :: (a -> b -> b) -> b -> [a] -> b
```

しかしその最初に引数に注目すると、それが 3 引数の関数だということがわかる。それは、我々が見てきたように、2 つの引数を受け取って関数を返す関数と同じである。
だから `b` は関数の型なのである。`b` は 3 引数に適用されたとき `foldr` の型を返す（？）

```
foldr (\x fId empty -> fId (x : empty)) id
```

それは関数なので、別の引数に適用できる。

```
(foldr (\x fId empty -> fId (x : empty)) id xs) []
```

`b` が実際になんであるかわかっただろう。（？？？？？？？？）

-}

reverse5 :: [a] -> [a]
reverse5 = foldl (\a x -> x:a) [] -- 自分の答えその 2 と同じ。