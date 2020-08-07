{-# OPTIONS -Wall -Werror #-}

---_/_/_/_/_/_/_/_/_/_/_/_/---
--      　モジュール　       --
---_/_/_/_/_/_/_/_/_/_/_/_/---

{-
    Haskell のモジュールは、いくつかの関数や型、型クラスなどを定義したファイルである。
    Haskell のプログラムはモジュールの酒豪である。

        💡Haskell には便利なモジュールがたくさんある。Hackage は Haskell のパッケージ群であり
        　cabal をインストールしてあれば `cabal install [パッケージ名]` でさまざまなパッケージをインストールできる。

    モジュールには複数の関数と型を定義でき、そのうちのいくつかもしくはすべてをエクスポートできる。
    エクスポートとは、もクールの中のものを外の世界からも見えるように、使えるようにすること。

    コードを複数のモジュールに分割することにはたくさんの利点がある。
    十分に汎用的なモジュールなら、エクスポートする関数を多くの異なるプログラムで使える。

    自分のコードを相互に強く依存しない（疎結合な）モジュールに分割しておけば、後でモジュールごとに再利用できる。
    コードを複数の部分に分割すれば、それだけ管理もしやすくなる。

    Haskell の標準ライブラリは複数のモジュールに分割されていて、それぞれのモジュールに含まれる関数と型にはなんらかの関係があり、共通の目的で結びついている。
    リストを走査するためのモジュール、並行プログラミングのためのモジュール、複素数を扱うためのモジュールなどがある。
    これまでの章で使ってきたすべての関数、型、型クラスはデフォルトでインポートされる Prelude というモジュールの一部。

    この章では便利なモジュールとその関数をいくつか見ていくが、その前にモジュールをインポートする方法を覚えよう。
-}

-------------------------------
-- モジュールをインポートする
-------------------------------

{-
Haskell のソースコードからモジュールをインポートする構文は `import <ModuleName>` である。
インポート構文はすべての関数定義よりも前に書く必要がある。そのため、インポート文はふつうファイルの先頭にある。

複数のモジュールをインポートしたい場合は、import 文を 1 行に 1 つずつ書いていくこと。

便利なモジュールの例として `Data.List` を見てみよう。
このモジュールはリストに対する関数をエクスポートしている。
このモジュールの関数を使って、リストに一意な要素がいくつあるか数える関数を作ってみよう。
-}

import Data.List
import Data.Char
import qualified Data.Map as Map
import Geometry
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube

numUniques :: Eq a => [a] -> Int
numUniques = length . nub

{-
    Data.List をインポートすると、Data.List がエクスポートするすべての関数が使えるようになり、スクリプトのどの場所からでも呼べるようになる。
    nub は Data.List がエクスポートする関数の 1 つで、リストから重複する要素を取り除く関数である。
    （なお、上記の式では前の章で学んだポイントフリースタイルが登場している。`length . nub` は length と nub の関数合成で `\xs -> length (nub xs)` と等価な関数である。

        💡探している関数がどこにあるか知りたいときは、Hoogle を使うとよい。関数名、モジュール名、型シグネチャから検索できる優れもの。
-}

{-
    モジュールで定義された関数は、GHCi からも使える。GHCi の中から Data.List がエクスポートしている関数を呼びたいなら、次のようにタイプする。
        :m + Data.List
    （GHCi 上で import 文を使うこともできる）

    GHCi から複数のモジュールにアクセスしたい場合、:m + を何回もタイプする必要はなくて、スペース区切りで列挙すればよい。
        :m + Data.List Data.Map Data.Set

    なお、モジュールをインポートするスクリプトをロードしている場合には、そのモジュールにアクセスするために :m + を使う必要はない。
    モジュール中の特定の関数のみが必要な場合には、その関数のみを選んでインポートできる。
    たとえば、Data.List から nub と sort のみをインポートしたいのなら、次のようにする。
        import Data.List (nub, sort)

    読み込みたくない関数を指定し、それ以外を全部インポートすることもできる。
    これは、複数のモジュールが同じ名前の関数をエクスポートしていて、不要なほうを取り除きたいときなどに使う。
    例えば、すでに nub という名前の関数が存在するので、Data.List から nub を除くすべての関数をインポートしたい場合は次のようにする。
        import Data.List hiding (nub)

    名前の競合を避けるもう 1 つの方法は、修飾付きインポート（qualified インポート）である。
    Data.Map モジュールを例に見てみよう。このモジュールは、キーに対応する値を検索するためのデータ構造を提供する。
    filter や null のような、Prelude にある関数と同じ名前の関数をたくさんエクスポートしている。
    なので、Data.Map をインポートして filter を呼び出すと、Haskell はどちらの関数を使えばいいかわからなくなってしまう。
    これを解決するには次のように修飾付きインポートする。
        import qualified Data.Map

    この状態で Data.Map の filter 関数を参照したいなら、Data.Map.filter を使わなければならない。
    単に filter とタイプすると、おなじみの filter 関数のことを指す。
    しかし、Data.Mao をすべての関数の前に付けるのはうんざりである。そこで、修飾付きインポートしたモジュールには何かしら短い別名をつけられるようになっている。
        import qualified Data.Map as M
    これで、Data.Map の filter 関数を参照するには単に M.filter とタイプするだけで済むようになった。

    「.」は関数合成演算子としても使用されていたが、qualified されたモジュール名と関数名の間に空白を開けずに置かれた場合には、インポートされた関数であるとみなされる。
    そうでなければ関数合成として扱われる。
    （モジュール名は大文字で始まり、関数名は小文字で始まることに注意）
-}

-------------------------------
-- 標準モジュールの関数で問題を解く
-------------------------------

--- ¶　単語を数える
{-
    Data.List に含まれる words 関数
    👉文字列を空白で区切られた文字列（単語）のリストに変換する。
-}

wrds :: [String]
wrds = words "hey these are the words in this sentence" -- ["hey","these","are","the","words","in","this","sentence"]

wrds' :: [String]
wrds' = words "hey these      are    the    words in this sentence  " -- ["hey","these","are","the","words","in","this","sentence"]

{-
Data.List に含まれる group 関数
👉同じ単語をグループ化するのに使う。この関数はリストを引数に取り、隣接する要素が同じものをまとめる。
-}

grp :: [[Int]]
grp = group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7] -- [[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]

--- 同じ要素が隣接していない場合は？

grp' :: [[String]]
grp' = group ["boom", "bip", "bip", "boom", "boom"] -- [["boom"],["bip","bip"],["boom","boom"]]
                        -- 同じリストに含まれているのに、"boom" を含むリストが 2 つ得られた。
                        -- どうすればいいか？ → 単語のリストをあらかじめソートしておけばいい。

{-
    Data.List に含まれる sort 関数
    👉順序づけされた要素からなるリストを受け取り、昇順に並び替えた新しいリストを返す
-}

sorted :: [Int]
sorted = sort [5, 4, 3, 7, 2, 1] -- [1,2,3,4,5,6,7]

sorted' :: [String]
sorted' = sort ["boom", "bip", "bip", "boom", "boom"] -- ["bip","bip","boom","boom","boom"]

{-  
    以上で材料は揃った！　では、これらを組み合わせて単語を数えてみよう。
    文章を引数に取り、[("hoge",3), ("fuga",1)] のように各単語の出現回数をカウントするようにする。
-}

wordNums :: String -> [(String,Int)]
wordNums = map (\xs -> (head xs, length xs)) . group . sort . words
    -- なお、これを関数合成なしで書くと wordNums xs = map (\xs -> (head xs, length xs)) (group (sort (words xs))) となる


--- ¶ 干し草の山から針を探す
{-
    次のミッションは、2 つのリストを受け取り、1 つ目のリストが 2 つ目のリストのどこかに含まれているかを調べる関数を作ること。
    例えば、[3,4] は [1,2,3,4,5] に含まれている。一方、[2,5] は含まれていない。
    検索対象のリストを haystack （干し草の山）、検索したいリストを needle（針）と呼ぶことにする。
-}

{-  この問題を解くために、Data.List の tails 関数を使う。
    tails はリストを受け取り、そのリストに対して tail 関数を繰り返し適用するもの。
-}

-- tails の使用例
ta :: [String]
ta = tails "party" -- ["party","arty","rty","ty","y",""]

ta' :: [[Int]]
ta' = tails [1,2,3] -- [[1,2,3],[2,3],[3],[]]

{-
    まず、文字列 "party" から文字列 "art" を探してみよう。
    最初に、tails 関数を使ってリストの tail をすべて取得する。
    それから各 tail を調べて、そのうちのどれかが art で始まっていれば干し草の山（heystack）から針（needle）を探し当てたということである。
    "party" の中で "boo" を探していても、"boo" から始まる tail は見つからないだろう。

    ある文字列が別の文字列から始まっているかどうかを調べるために、Data.List に含まれている isPrefixOf 関数を使う。
    これは 2 つのリストを引数に取り、2 つ目のリストが 1 つ目のリストで始まっているかどうかを教えてくれる。
-}

tf1 :: Bool
tf1 = "hawaii" `isPrefixOf` "hawaii joe" -- True

tf2 :: Bool
tf2 = "haha" `isPrefixOf` "ha" -- False

tf3 :: Bool
tf3 = "ha" `isPrefixOf` "ha" -- True

{-
    あとは haystack の tail に needle から始まるものがあるか調べるだけ。
    これには Data.List にある any 関数が使える。述語とリスtおを受け取り、要素のどれかが述語を満たすかどうかを返す関数である。
    たとえば以下のように使う。
-}

tf4 :: Bool
tf4 = any (> (4 :: Int)) [1, 2, 3] -- False

tf5 :: Bool
tf5 = any (=='F') "Frank Sobotka" -- True

tf6 :: Bool
tf6 = any (\x -> x > (5 :: Int) && x < (10 :: Int)) [1, 4, 11] -- False

{- これらの関数を組み合わせて、heystack に needle があるかどうか判定する関数を作ってみよう。 -}

isIn :: Eq a => [a] -> [a] -> Bool
needle `isIn` haystack = any (==True) $ map (\hay -> needle `isPrefixOf` hay) (tails haystack)
    -- これで間違ってはいないけど、冗長。
-- 以下のように書けば better
isIn' :: Eq a => [a] -> [a] -> Bool
needle `isIn'` haystack = any (needle `isPrefixOf`) (tails haystack)

{- 💡なお、ここで作った `isIn` 関数は、同じ挙動をするものが Data.List の isInfixOf 関数として用意されている　-}

--- ¶ シーザー暗号サラダ

{-
    Data.Char の関数をいくつか使って、シーザー暗号でメッセージを暗号化し、他人には読めないようにしよう。
    シーザー暗号とは文字列を暗号化する原始的な方法で、それは各文字をアルファベット上で一定の数だけシフトするというものである。
    シーザー暗号は簡単に作れて、それもアルファベットに限らず Unicode 文字全体に対するものが作れる。

    文字を前方向および後ろ方向にシフトするために、Data.Char モジュールにある文字を対応する数に変換する ord 関数と、その逆を行う chr 関数を使う。
-}

charHoge :: Int
charHoge = ord 'a' -- 97（'a' は Unicode テーブル上の 97 番目に位置するので、ord 'a' は 97 を返す）

charFuga :: Char
charFuga = chr 97 -- 'a'

charPiko :: [Int]
charPiko = map ord "abcdefgh" -- [97,98,99,100,101,102,103,104]

{- 
    2 つの文字の ord の値の差は、Unicode テーブル上で文字が何文字離れているかと同じ。
    文字をシフトする数と文字列を受け取り、文字列中の各文字をアルファベット上で指定された数だけ前方向に（プラス方向に；例えば 97 -> 100 のように。）シフトする関数を書いてみよう。
-}

encode :: Int -> String -> String
encode offset msg = map (chr . (+) offset . ord) msg

{-
    メッセージの複合は、基本的には元の文字をシフトした数だけ単純に逆にシフトすればよい。
-}

decode :: Int -> String -> String
decode shift = encode (- shift) -- decode shift = encode (negate shift) でもいい。negate を使うか - を使うかの違い。

--- ¶　正格な左畳み込みにて
{-
    前の章で foldl がどのように動作するのかを見てきたが、foldl にはまだよく調べていない問題がある。
    foldl はコンピュータのメモリの特定の領域を使いすぎたときに起こる、スタックオーバーフローエラーを引き起こすことがあるのである。

    これを示すために、foldl と + 関数を使って 100 個の 1 からなるリストを合計してみよう。
-}

fo :: Int
fo = foldl (+) 0 (replicate 100 1)

{-  では、100 万個の 1 を突っ込んだリストの総和を foldl1 で求めようとしたらどうなるか？ -}
fo' :: Integer
fo' = foldl (+) 0 (replicate 1000000000 1)
                    -- stack overflow になる

{-
    Haskell は遅延評価で、だから実際の値の計算は可能な限り後まで引き延ばされる。
    foldl を使うとき、Haskell は各ステップにおけるアキュムレータの計算（すなわち評価）を実際には行わない。
    そのかわり、評価を先延ばしにする。
    その次のステップでもアキュムレータを評価することはなく、評価を先延ばしにする。
    このとき、新しい計算で前の計算結果を参照するかもしれないので、以前に先延ばしにしていた計算もメモリ上に保持し続ける。
    こうして畳み込みでは、それぞれバカにならない量のメモリを消費する先延ばしにした計算が積み上がっていく。
    そしてついにはスタックオーバーフローエラーを引き起こしてしまうのである。
-}

-- Haskell がどのように式 foldl (+) 0 [1,2,3] を評価するのかを見てみる。

{-
    foldl (+) 0 [1,2,3] =
        foldl (+) (0 + 1) [2,3] =
            foldl (+) ((0 + 1) + 2) [3] =
                foldl (+) (((0 + 1) +2) + 3) [] =
                    ((0 + 1) + 2) + 3 =
                        (1 + 2) + 3 =
                            3 + 3 =
                                6

見てのとおり、先延ばしにした計算による大きなスタックがまず構築され、空リストに到達したところで、先延ばしにしていたそれらの計算が実際に開始される。
これは小さなリストでは問題にならないが、100 万の要素を含むような大きなリストでは、先延ばしにしていた計算がすべて再帰的に行われるのでスタックオーバーフローになる。

計算を先延ばしにしない、以下のような関数 foldl' があればいいのだが。

    foldl' (+) 0 [1, 2, 3] =
    fold' (+) 1 [2, 3] =
    fold' (+) 3 [3] =
    fold' (+) 6 [] =
    6

❗️このような foldl の正格バージョンの関数、まさしく foldl' という名前の関数が Data.List にある。さっそくその fold' を使って大量の 1 の総和を計算してみよう。
-}

fo'' :: Integer
fo'' = foldl' (+) 0 (replicate 1000000000 1) -- やや時間はかかるが 1000000000 と計算できる。

-- ¶　かっこいい数を見つけよう
{-
    問題: 各桁の数の合計が 40 になる最初の自然数を求めよ。

    はじめに、数を引数として受け取り、各桁の合計を求める関数を作る。
    まず、show を使って数を文字列に変換する。文字列が得られたら、それぞれの文字を数に変換して、その数のリストを合計する。
    文字を数に変換するには Data.Char モジュールの digitToInt という手軽な関数を作る。これは Char を受け取り Int を返す。
    （※ 復習：今回は使わないが、read 関数を使うと String（[Char]）を受け取り Int を返すことができるのだった）

    digitToInt は '0' から '9' および 'A' から 'F'（小文字でもよい）の文字に対して動作する。
    
    数を引数に取って、その各桁の数の合計を返す関数は次のように書ける。
-}

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

{-
    文字列に変換し、その文字列に対して digitToInt をマップして、その結果の数のリストを合計する。

    さて、digitSum を適用した結果が 40 になる最初の数を探さないといけない。
    それには Data.List にある find 関数を使う。
    これは述語関数とリストを引数に取り、リストの中で条件に合致する最初の要素を探す。
    この関数は型がちょっと変わっている。

    find :: (a -> Bool) -> [a] -> Maybe a

    最初の引数は述語で、2 つ目の引数はリスト。ここまでは大丈夫。
    返り値は？　Maybe a と言っている。
    見たことのない型である。
    Maybe a 型の値は、リスト型 [a] に似ている。
    リストが 0 個、1 個、あるいはもっとたくさんの要素を持てるのに対し、Maybe a 型の値は、0 個かちょうど 1 個の要素だけを持てる。

    この型は、失敗する可能性があることを表現するのに使う。
    何も持っていないという値を作るには Nothing を使う。これは空リストに似ている。
    何かを、例えば "hey" を保持している値を作るときは、Just "hey" と書く。
-}

nothing :: Maybe a 
nothing = Nothing

hey :: Maybe String
hey = Just "hey"

three :: Maybe Int
three = Just 3

mTrue :: Maybe Bool
mTrue = Just True

{-
    ご覧のとおり、Just True の型は Maybe Bool である。これは、真理値を保持するリストの型が [Bool] であるようなもの。
    もし find で述語を満たす要素が見つかったら、その（最初の）要素を Just でラップしたものが返される。
    見つからなければ Nothing が返される。

        find (>4) [3, 4, 5, 6]
        > Just 5

        find odd [0, 2, 121, 3]
        > Just 121

        find (=='z') "mjonir"
        > Nothing
-}

-- では、目的の関数の実装に移ろう。
firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

-- ❌
firstTo40' :: Maybe Int
firstTo40' = find (== 40) $ map digitSum [1..] -- こうしてしまうと、結果（出力）は 40 になる。
                                                -- それもそのはずで、find (==40) hoge は hoge から、40 と一致する最初の要素を返すもので、その要素は当然 40 になる。
                                                --　（インデックスを返すわけではない）
                                                -- 一方、find (\x -> digitSum x == 40) [1..] とすると、x を 1 から 1 つずつ大きくしていったとき、
                                                -- その x に digitSum 関数を適用してはじめて 40 と一致するそのときの x はなにかということを求められる。

-- 合計値の希望が 40 に固定されていない、引数として与えられるもっと一般的な関数がほしければ次のように変更できる。
firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

-------------------------------
-- キーから値へのマッピング
-------------------------------

{-
    集合のようなデータを扱うときは、その順序を気にしたくない場合もある。単にキーでアクセスしたい場合など。
    たとえば、ある住所に誰が住んでいるか知りたいときは、住所で名前を検索したい。

    この節では、のぞみの値（誰かの名前）をなんらかのキー（その人の住所）で検索する話をしよう。
-}

--- ¶ だいたい大丈夫（連想リスト）
{-
    key / value を対応づける方法はいくつかある。そのうちの 1 つは連想リストである。連想リスト（あるいは辞書）は、 key と value のペアを順序を気にせずリストにしたものである。
    たとえば電話番号を格納する連想リストなら、電話番号が値で、人の名前がキーになる。格納されている順番は気にしない。
    正しい人名に対して正しい電話番号が得られればそれでよい。

    Haskell で連想リストwお表現sる一番あからさまな方法は、ペアのリストであろう。
    ペアの 1 つ目の要素をキーに、 2 つ目の要素を値にする。電話番号の例なら以下のような連想リストになる。
-}

phoneBook :: [(String, String)]
phoneBook = 
        [("betty", "555-2938")
        , ("bonnie", "452-2928")
        , ("patsy", "493-2928")
        , ("lucille", "205-2928")
        , ("wendy", "939-8282")
        , ("penny", "853-2492")
        ]

{-
連想リストに対する一番よくある操作は、キーによる値の検索である。与えられたキーに対して値を検索する関数を作ってみよう。
-}

-- 自分の答え。
findKey'' :: String -> String
findKey'' key = snd $ filter (\x -> fst x == key) phoneBook !! 0

-- テキストの模範解答。一般の連想リスト（辞書）を引数に取れるようにしている。
-- また、!! 0 ではなく head を使っている。
-- `!! 0` はなんか ad-hoc な印象があるけど、まあ head も head で空リストを引数に与えると実行時例外が出るので似たようなもん（か）。
findKey' :: Eq k => k -> [(k, v)] -> v
findKey' key = snd . head . filter (\(k, _) -> key == k) -- (\k, _) の _ は value のイメージ。

-- 👀上で「head も空リストを引数に与えると実行時例外が出るので〜」と書いたけど、テキストにもちゃんとそこのフォローがあった！
{-
    連想リストに探しているキーがなかったら、空リストの head を取ろうとしてランタイムエラーが投げられる。
    そんなに簡単にクラッシュしてしまうプログラムを書くべきではない。
    そこで Maybe 型を使うことにする。
    キーが見つからなかったときは Nothing を返す。
    見つかったときは `Just 何か`（`何か` はキーに対応する値）を返す。
-}

findKey :: Eq k => k -> [(k, v)] -> Maybe v
findKey _ [] = Nothing
findKey key ((k, v):xs)
    | key == k = Just v
    | otherwise = findKey key xs

{-
    上記は畳み込みの典型的なパターンなので、畳み込みとして実装する方法を見てみよう。
-}

findKeyFold :: Eq k => k -> [(k, v)] -> Maybe v
findKeyFold key = foldl (\acc (k, v) -> if k == key then Just v else acc) Nothing
            -- ok!　テキストでは foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing と foldr を使っている。

--- ¶ Data.Map に潜入せよ
{-
    いま実装したのは、Data.List にある lookup 関数。キーに対応した値が欲しければ、見つかるまですべての要素を走査する必要がある。
    実は、はるかに高速な連想リストと豊富なユーティリティ関数が Data.Map モジュールには存在する。
    これからは「連想リストを使う」と言う代わりに「Map を使う」と言うことにする。

    Data.Map は Prelude や Data.List と競合する名前をエクスポートしているため、修飾付きインポートする。
        import qualified Data.Map as Map

    Data.Map の fromList 関数を使って、連想リストを Map に変換する。
    fromList は連想リスト（リストの形をしている）を受け取り、同じ対応関係を持つ Map を返す。
    まずは fromList で遊んでみよう。
-}

mapfromlist :: Map.Map Int String
mapfromlist = Map.fromList [(3, "shoes"), (4, "trees"), (9, "bees")] -- fromList [(3,"shoes"),(4,"trees"),(9,"bees")]

mapfromlist' :: Map.Map String String
mapfromlist' = Map.fromList [("kima", "greeggs"), ("jimmy","mcnulty"), ("jay", "landsman")] -- fromList [("jay","landsman"),("jimmy","mcnulty"),("kima","greeggs")]

{- 
    Data.Map の Map は、もはやリストでもなんでもないが、ターミナルに表示されるときは fromList に続けてその Map を表現する連想リストが出力される。
    元の連想リストに重複したキーがあった場合、後のほうの要素が使われる。
-}

mapfromlist'' :: Map.Map String Int
mapfromlist'' = Map.fromList [("MS", 1), ("MS", 2), ("MS", 3)] -- fromList [("MS",3)]

{-
    fromList の型シグネチャは以下。
        Map.fromList :: Ord k => [(k, v)] -> Map.Map k v

    これは、型 k と v のペアのリストを受け取り、型 k をキー、型 v を値とする Map を返すと読める。
    普通のリストによる連想リストでは、キーは等値比較だけできればよかった（つまり　Eq 型クラスに属する型であればよかった）が、Map では順序比較が必要！
    これは Data.Map モジュールの本質的な制約である。
    Data.Map モジュールは、キーが順序づけされていることを李ゆおして、効率よくキーを配置したりキーにアクセスしたりできるのである。
    （なお、String や Int は Ord 型クラスに属する）
-}

{-
    これで phoneBook を、連想リストから Map による実装へと変更できる。
-}

phoneBook' :: Map.Map String String
phoneBook' = Map.fromList phoneBook

{-
    少し遊んでみる。
    lookup を使えば検索ができる。lookup はキーと Map を受け取り、対応する値を Map から探す。
    成功したら値を Just で包んで返し、失敗したら Nothing を返す。

    Map.lookup :: (Ord k) => k -> Map.Map k a -> Maybe a
-}

bettyNumber :: Maybe String
bettyNumber = Map.lookup "betty" phoneBook' -- Just "555-2938"

takeshiNumber :: Maybe String
takeshiNumber = Map.lookup "takeshi" phoneBook' -- Nothing

{-
    お次は、phoneBook に電話番号を挿入して新しい Map を作る。
    insert はキーと値、それに Map を受け取り、その Map にキーと値を挿入した新しい Map を返す。

    Map.insert :: Ord k => k -> a -> Map.Map k a -> Map.Map k a
-}

newBook :: Map.Map String String
newBook = Map.insert "grace" "341-9021" phoneBook' -- fromList [("betty","555-2938"),("bonnie","452-2928"),("grace","341-9021"),("lucille","205-2928"),("patsy","493-2928"),("penny","853-2492"),("wendy","939-8282")]

graceNumber :: Maybe String
graceNumber = Map.lookup "grace" newBook -- Just "341-9021"

{-
    電話番号がいくつあるか調べてみよう。Data.Map の size 関数は、Map を受け取り、そのサイズを返す。
    Map.size :: Map.Map k a -> Int
-}

sizeOfMap :: Int
sizeOfMap = Map.size phoneBook' -- 6

sizeOfMap' :: Int
sizeOfMap' = Map.size newBook -- 7

{-
    この電話帳では、電話番号を文字列として表現している。
    いま、文字列ではなく Int のリストで電話番号を表現したいとしよう。
    つまり、 "939-8282" ではなく [9,3,2,8,2,8,2] のように電話番号を持つのである。
    最初に、電話番号の文字列を Int のリストに変換する関数を作る。
    Data.Char の digitToInt を文字列に対して map すればよさそう。
    しかしこの関数は文字列にダッシュ（`-`）が出てきたらお手上げとなる。そのため、事前に数字以外を取り除いておく必要がある。
    それには、文字が数字かどうかを教えてくれる Data.Char の isDigit 関数が使える。
    文字列をフィルタしてしまえば、あとは digitToInt をマップするだけである。
-}

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

-- では、**Data.Map の map 関数**を使って、phoneBook を string2digits でマップしよう。
---- 📣Data.Map の map は、関数と Map を受け取り、その関数を Map の中の各値に適用（map）する。
intBook :: Map.Map String [Int]
intBook = Map.map string2digits phoneBook' -- fromList [("betty",[5,5,5,2,9,3,8]),("bonnie",[4,5,2,2,9,2,8]),("lucille",[2,0,5,2,9,2,8]),("patsy",[4,9,3,2,9,2,8]),("penny",[8,5,3,2,4,9,2]),("wendy",[9,3,9,8,2,8,2])]

betty :: Maybe [Int]
betty = Map.lookup "betty" intBook -- Just [5,5,5,2,9,3,8]

{-
    電話帳を拡張してみよう。一人が複数の番号を持っていて、連想リストが次のように与えられたとする。
-}

phoneBookEx :: [(String, String)]
phoneBookEx =
        [("betty", "555-2938")
        , ("betty", "342-2492")
        , ("bonnie", "452-2928")
        , ("patsy", "493-2928")
        , ("patsy", "943-2929")
        , ("patsy", "827-9162")
        , ("lucille", "205-2928")
        , ("wendy", "939-8282")
        , ("penny", "853-2492")
        , ("penny", "555-2111")
        ]

{-  これ（↑）に対して単純に fromList を使ってしまうと、番号がいくつか失われてしまう。
    代わりに Data.Map にある別の関数、fromListWith を使う。
    この関数は fromList と似ているが、キーの重複を削除しない。
    そのかわり、重複時にどうするかを決める関数を受け取る。    
-}

phoneBookToMap :: Ord k => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith add
    where add number1 number2 = number1 ++ ", " ++ number2 -- add は where でなくてもそのままワンラインで書いちゃってもいいが、かえってわかりづらくなりそう

    {- fromListWIdh は、すでに存在するキーを見つけると、与えられた結合関数にそれら競合する 2 つの値を渡し、得られた値で古い値を置き換える。-}

phoneMapEx :: Map.Map String String
phoneMapEx = phoneBookToMap phoneBookEx -- fromList [("betty","342-2492, 555-2938"),("bonnie","452-2928"),("lucille","205-2928"),("patsy","827-9162, 943-2929, 493-2928"),("penny","555-2111, 853-2492"),("wendy","939-8282")]

{-
    あらかじめ連想リストの値を単一要素のリストにしておけば、電話番号を連結するのに ++ が使える。
-}

phoneBookToMap' :: Ord k => [(k, v)] -> Map.Map k [v]
phoneBookToMap' = Map.fromListWith (++) . map (\(k, v) -> (k, [v]))
                                -- map (\(k, v) -> (k, [v])) phoneBookEx で、その出力は
                                -- [("betty",["555-2938"]),("betty",["342-2492"]),("bonnie",["452-2928"]),("patsy",["493-2928"]),("patsy",["943-2929"]),("patsy",["827-9162"]),("lucille",["205-2928"]),("wendy",["939-8282"]),("penny",["853-2492"]),("penny",["555-2111"])]
                                -- となる。
                                -- phoneBookToMap（= Map.fromListWith add）では文字列連結を使ったけど、この例だとまずすべてのペアのバリューを文字列のリストにして、そのリストを連結している。

patsy :: Maybe [String]
patsy = Map.lookup "patsy" $ phoneBookToMap' phoneBookEx -- Just ["827-9162","943-2929","493-2928"]

{-
    fromListWith の応用例
-}

-- 番号の連想リストから Map を作って、そのキーに対する値の中の最大値を保持したいとき
maxMap :: Map.Map Int Int
maxMap = Map.fromListWith max [(2,3), (2,5), (2,100), (3,29), (3,22), (3,11), (4,22), (4,15)] -- fromList [(2,100),(3,29),(4,22)]

-- 同じキーの値を足し合わせることもできる
sumMap :: Map.Map Int Int
sumMap = Map.fromListWith (+) [(2,3), (2,5), (2,100), (3,29), (3,22), (3,11), (4,22), (4,15)] -- fromList [(2,108),(3,62),(4,37)]