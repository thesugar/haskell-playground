{-# OPTIONS -Wall -Werror #-}

---_/_/_/_/_/_/_/_/_/_/_/_/---
--      Hello 再帰！       --
---_/_/_/_/_/_/_/_/_/_/_/_/---

{- 再帰とは: 関数の定義の中で自分自身を呼び出す、という関数の定義のしかた -}

{- 関数を再帰的に定義するには
    👉  解こうとする問題を同じ種類のより小さな問題に分解し、それら部分問題を解くことを考える。
        必要なら、それらをさらに分解していく。
        最終的には、問題の **基底部**、つまりこれ以上分解できなくて解を明示的に（再帰を使わずに）定義しなければならないケース（複数あるかも）にたどり着く。

    🧭　数学における定義には再帰がよく出てくる
    👉　例えばフィボナッチ数列・・・
        F(0) = 0, F(1) = 1（これらが基底部になる）
        F(n) = F(n-1) + F(n-2)

    🔥　Haskell では再帰が重要
    👉　命令型言語とは違い、どうやって計算をするかを指定するのではなく、求めるものが何であるかを宣言して計算を行うから。
        つまり、計算を実行するステップを示すのではなく、ほしい結果が何であるかを直接定義する必要があるから。
-}

-------------------------------
-- 最高に最高！（Maximum awesome）
-------------------------------

{-
    maximum 関数は順序のついた値（Ord 型クラスのインスタンス）のリストを受け取り、その中で一番大きな値を返す。これは再帰を使うとエレガントに実装できる。

    🙄 ちなみに・・・命令的に定義したらどういう実装になるか？
        👉　現在の最大値を保持するための変数を用意し、リストのすべての要素についてループして、現在の要素がこれまでの最大値よりも大きければ最大値を現在の値で更新する。

    🤓 では、再帰的に定義する方法は？
        1️⃣　最初に基底部を定義する！
            単一要素のリストに対する最大値は、その唯一の要素と同じ値
        2️⃣　もっと要素がたくさんある場合は？
            リストの先頭要素（head）と残りのリスト（tail）の最大値とでどちらが大きいかを調べる
-}

-- 再帰的に定義した maximum 関数
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

{-
    💡上記のとおり、パターンマッチは再帰関数の定義にもってこいである。合致と値の分解ができるので、最大値を見つけるという問題を、関連するいくつかのケースと部分問題に簡単に分解できる。
-}

-------------------------------
-- さらにいくつかの再帰関数（A few more recursive functions）
-------------------------------

-- 🔍 replicate
--      - replicate は Int と値を取り、複製（replicate）という関数名のとおり、その値を（指定された数だけ）繰り返したリストを返す。
--        例えば replicate 3 5 は、3 つの 5 からなるリスト [5, 5, 5] を返す。

replicate' :: Int -> a -> [a]
replicate' num x
    | num <= 0 = [] -- 引数の構造で場合分けするのがパターン、引数の値の性質（範囲など）で分けるのがガード。今回は負のケースに対応するためガードを使用。
--    | num == 1 = [x]　❌ これはいらない（あっても正しく動くが。。）
    | otherwise = [x] ++ replicate' (num-1) x -- これは x : replicate' (num-1) x でもよい。

-- 🔍 take
--      - この関数は指定されたリストから指定された数の要素を返す。
--      - たとえば take 3 [5, 4, 3, 2, 1] は [5, 4, 3] を返す。

take' :: Int -> [a] -> [a]
take' _ [] = []
take' num (x:xs)
    | num <= 0 = []
    | otherwise = x : take' (num-1) xs

{- 上記で OK。テキストの模範解答は以下。
        take' :: Int -> [a] -> [a]
        take' n _
            | n <= 0 = []
        take' _ [] = []
        take' n (x:xs) = x : take' (n-1) xs
-}

-- 🔍 reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- 🔍 repeat
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- 🔍 zip
----- ❌zip' :: [a] -> [a] -> [(a, a)] // 自分の回答。引数にとる 2 つのリストの型が同じ型である必要はない
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x, y)] ++ zip' xs ys -- これは (x, y) : zip' xs ys でもよい。

-- 🔍 elem

elem' :: (Eq a) => a -> [a] -> Bool -- ここは最初は elem' :: a -> [a] -> Bool と書いておくで問題なくて、コンパイル時に Eq a つけろってエラー出るので気付ける。
_ `elem'` [] = False
y `elem'` (x:xs) = (y == x) || (y `elem'` xs)

{- 上記で OK。テキストの模範解答は以下
    elem' :: (Eq a) => a -> [a] -> Bool
    elem' a [] = False
    elem' a (x:xs)
        | a == x = True
        | otherwise = a `elem'` xs
            -- 自分の回答と本質的には同じ。ガードにしたほうが可読性は高い気もするけど 1 行で書いてしまうのも悪くないよね。。
-}

-------------------------------
-- クイック、ソート！
-------------------------------

{-  順序づけされた要素（数など）のリストをソートする問題は、自然と再帰的な解決方法へ行き着く。
    リストの再帰的なソートにはいろいろなアプローチがあるが、ここではクイックソートを見ていこう。 -}

-- ¶　クイックソートのアルゴリズム

{-
    ソートしたいリストを [5, 1, 9, 4, 6, 7, 3] としよう。
    最初の要素である 5 を選択して、それから、残りのリストの中で 5 以下の要素を左に置く。
    次に、 5 より大きい要素を右に置く。
    この操作によって [1, 4, 3, 5, 9, 6, 7] というリストが得られる。

    この例における 5 はピボット（軸）と呼ばれる。
    ピボットに最初の要素を使う唯一の理由は、パターンマッチで簡単に取り出せるからである。
    実際にはどの要素をピボットにしてもかまわない（所要時間の期待値を O(NlogN) で抑えるためには、ピボットをランダムに選ぶなどの工夫が必要）。

    さて、5 をピボットにして [1, 4, 3, 5, 9, 6, 7] へソートするところまで終わったら、[1, 4, 3] と [9, 6, 7] をそれぞれ同じ方法でソートする。
    [1, 4, 3] をソートするのに、最初の要素 1 をピボットとして選択し、1 以下の要素からなるリスト（→ これは空のリスト [] になる）を作る。
    ピボット（1）の右に置くのは、1 より大きい要素からなる [4, 3] である。

    今度は [4, 3] をまた同じようにソートする。

    このように、最終的に空のリストになるまでバラバラにしてくっつけ直す。
-}

--- 😭独力で書けなかった⤵︎　リスト内包表記使うのわかればぜんぜん難しくない
quicksort:: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smallerOrEqual ++ [x] ++ quicksort larger
    where   smallerOrEqual = [a | a <- xs, a <= x]
            larger = [a | a <- xs, a > x] 
{-
    where でなく let 束縛を使ってもよい。
        quicksort (x:xs) = 
            let smallerOrEqual = [a | a <- xs, a <= x]
                larger = [a | a <- xs, a > x]
            in quicksort smallerOrEqual ++ [x] ++ quicksort larger
-}


----- まとめ ------
-- 😉再帰を書く際の定石
--  ✅ まず、再帰に頼らない自明な解を持つ基底部（edge cases）から始める
--     （たとえば、空のリストをソートしたものは空のリストである、など）
--  ✅ それから、問題を 1 つもしくはそれ以上の部分問題に分解し、自分自身を適用することによって、それらの部分問題を再帰的に解く。
--  ✅ 最後に、最終的な解を部分問題の解から構築する。
--     （たとえばソートの場合なら、リストを 2 つのリストとピボットに分解する。それらのリストを再帰的にソートして、
--       結果が得られたら 1 つにつなげて大きなソート済みリストにする）

-- 😉 基底部（edge case）の特徴
--  👉 同一性（an identity）。単位元てきな。
--     `quicksort [] = []` のように、その関数を適用しても、引数と同じ結果が得られる（maximum [x] = x のように、つねに厳密な同一性が成り立つわけではない）