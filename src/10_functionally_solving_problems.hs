{-# OPTIONS -Wall -Werror #-}

---_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/---
--   　　　　関数型問題解決法　　　　    --
---_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/---

-------------------------------
--　逆ポーランド記法電卓
-------------------------------
{-
    ふつう、算数では中置記法で式を書く。10 - (4 + 3) * 2 という具合に。
    足し算（+）、掛け算（*）、引き算（-）などは、Haskell の中置関数（+ とか `elem`とか）と同じく、中置演算子である。
    人間は中置記法の数式をいとも簡単に脳内で parse できる。
    欠点は、演算の優先順位を指定するのに括弧が必要になるということだ。

    数式を書く別の方法として **逆ポーランド記法（reverse polish notation）**、略して RPN がある。
    RPN では、演算子は数に挟まれるのではなく、数の後に来る。
    4 + 3 と書く代わりに 4 3 + と書くわけである。
    では複数の演算子を含む数式はどのように書くのだろう？　答えは簡単。 4 3 + 10 * と書く。ここで 4 3 + は 7 なので、数式全体は 7 10 * と等価になる。
-}

--- ¶　RPN 記法の式を計算
{-
    RPN 記法を計算する方法を感覚的につかむには、数のスタックをイメージする。RPN 記法は、左から右に（＝普通に）読む。数を読み込んだら、それをスタップのてっぺんに積む（push）。
    演算子を読み込んだら、スタックのてっぺんから 2 つの数を取り出し（pop）、その 2 つの数に演算子を施して、演算結果をスタックに積み直す。
    式の末尾にたどり着いたら、計算結果を示す数が 1 つだけスタックに残っているはずである。

    例えば、RPN 式 10 4 3 + 2 * - をどうやって評価するのか見ていこう。
    →   1. まず 10 をスタックに積み、スタックは 10 が 1 つ入った状態になる。
        2. 次のアイテムは 4 なので、これもスタックに積む。今のスタックは 10, 4 である。
        3. 3 にも同じことをしてスタックは 10, 4, 3 になる。
        4. 演算子が出てきた。+。そこでスタックから 2 つの数を取り出し（スタックは 10 だけになる）、取り出した 2 つの数を加算し、計算結果をスタックに戻す。
        　　こうしてスタックは 10, 7 になる。
        5. 次にスタックに 2 を積み、スタックは 10, 7, 2 になる。
        6. また演算子がきた。7 と 2 をスタックから取り出し、掛け算して、結果をスタックに戻す。7 * 2 は 14 だから、スタックは 10, 14 になる。
        7. 最後に - がある。10 と 14 をスタックから取り出し、10 から 14 を引いて、スタックに戻す。
        8. スタックに乗っている数は -4。与えられた式にはもう数も演算子も残っていないので、これが答えである。 

    これが RPN 式を手で計算する方法である。では、Haskell で同じことをするにはどうすればいいか？
-}

--- ¶ RPN 関数を書く
{-
    "10 4 3 + 2 * -" のような RPN 式を文字列で受け取って、その式の結果を返す関数を書こう。
    この関数の型はどうなるか？　「文字列を引数に取って、数を結果として返す関数」である。
    割り算もできるようにしたいので、結果は倍精度浮動小数がいいだろう。というわけで、型は String -> Double とするのがいいかな。
    　👉 関数の実装に取り掛かる前に、まず関数んお型宣言がどうなるか考えるのはとても役に立つ習慣である！

    では、RPN を解く関数をどう書けばいいか考えていく。手計算したときは、空白で区切られた数や演算子のそれぞれを 1 つのアイテムとして扱った。
    ということは、 "10 4 3 + 2 * -" のような文字列を、まずはアイテムのリストに分割することから始めるとよさそう。
        ["10", "4", "3", "+", "2", "*", "-"]
    それから、このリストを左から右へと走査して、その間スタックを更新し続けたんだった。
    これは畳み込み（リストを一要素ごとに走査しながら何らかの結果を積み上げていく（アキュムレート））に似ている！

    今回の場合、リストを左から右へ走査するので、左畳み込みを使おう。
    アキュムレータ値はスタックなので、畳み込みが返す結果もスタックになるはずである。
    ただし、すでに見たように値が 1 つしか入っていないスタックである。

    もう 1 つ、スタックをどのように表現するかも考える必要がある。
    リストを使って、リストの先頭をスタックの先頭に対応させるのがいいだろう。リストの先頭（head）に要素を追加するのは、末尾に追加するよりずっと高速だからだ。
    例えば 10, 4, 3 というスタックは 3:4:10:[] すなわち [3,4,10] というリストとして表現することになる。

    これで作りたい関数の姿がおぼろげながら見えてきた。まずその関数は "10 4 3 + 2 * -" といった文字列を取り、それを words を使ってアイテムのリストに分解する。
    次に、そのリストに左畳み込みを使って、単一の要素が入ったスタック（この例の場合は、[-4]）にたどりつく。
    その単一要素をリストから取り出して、それがファイナルアンサーである！

    以下がその関数のスケッチである。
        solveRPN :: String -> Double
        solveRPN expression = head $ foldl foldingFunction [] (words expression)
            where foldingFunction stack item = ...

    この関数は expression を取って、まずアイテムのリストに変える。それからそのアイテムのリストを関数 foldingFunction で畳み込む。
    [] はアキュムレータの初期値である。アキュムレータはスタックなので、[] は空のスタックを表している。
    そして単一要素の入った最終状態のスタックを受け取ったら、head を使ってアイテムを取り出す。

    さて、あとは畳み込み処理を行う関数を書くだけである。
    その関数は、たとえばスタック [4,10] とアイテム "3" を受け取って、新しいスタック [3,4,10] を返す。
    また、スタックが [4,10] で、受け取ったアイテムが "*" なら、関数は [40] を返すべきである。

    とりあえず、畳み込み関数を書く前に、全体の関数をポイントフリースタイルで書き直しておく。

        solveRPN :: String -> Double
        solveRPN = head . foldl foldingFunction [] . words
            where foldingFunction stack item = ...

    畳み込み関数は、スタックとアイテムを受け取って新しいスタックを返すようにする。
    関数定義構文でパターンマッチを使い、スタックの上側にあるアイテムを取り出す処理と "*" や "-" のような演算子を識別する処理を一気にやろう。

        foldingFunction (x:y:ys) "*" = (y * x):ys
        foldingFunction (x:y:ys) "+" = (y + x):ys
        foldingFunction (x:y:ys) "-" = (y - x):ys
        foldingFunction xs numberString = read numberString:xs
-}

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (y*x):ys
            foldingFunction (x:y:ys) "+" = (y+x):ys
            foldingFunction (x:y:ys) "-" = (y-x):ys
            foldingFunction xs numberString = read numberString: xs

{-
    この関数で遊んでみよう。

    *Main> solveRPN "10 4 3 + 2 * -"
    -4.0

    *Main> solveRPN "2 3.5 +"
    5.5

    *Main> solveRPN "90 34 12 33 55 56 + * - +"
    -3617.0

    *Main> solveRPN "90 34 12 33 55 66 + * - +"
    -3947.0

    *Main> solveRPN "90 34 12 33 55 66 + * - + -"
    4037.0

    *Main> solveRPN "90 3.8 -"
    86.2
-}

--- ¶　演算子を追加しよう
{-
    この解法の何が良いかというと、他のいろんな演算を簡単にサポートできるところである。
    二項演算子である必要もない。
    例えば、数を 1 つだけ取り出してその対数を積む "ln" という演算も作れる。
    可変長引数を取る演算子だって作れる。
    例えば "sum" はスタックからすべての数を取り出してその総和を積む演算である。

    RPN 電卓関数を修正してもっと多くの演算子をサポートするようにしよう。
-}

solveRPN' :: String -> Double
solveRPN' = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (y * x):ys
            foldingFunction (x:y:ys) "+" = (y + x):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction (x:y:ys) "/" = (y / x):ys
            foldingFunction (x:y:ys) "^" = (y ** x):ys
            foldingFunction (x:xs) "ln" = log x:xs
            foldingFunction xs "sum" = [sum xs]
            foldingFunction xs numberString = read numberString: xs

{-
    *Main> solveRPN' "1 2 sum"
    3.0

    *Main> solveRPN' "2.71828 ln"
    0.999999327347282

    *Main> solveRPN' "10 10 10 10 sum 4 /"
    10.0

    *Main> solveRPN' "10 2 ^"
    100.0

    任意の浮動小数点数の RPN 式を計算できて、しかも用意に拡張できる関数がたったの 10 行で書けるというのはかなり驚異的！
-}

-------------------------------
--　ヒースロー空港からロンドンへ
-------------------------------
{-
    最短経路を探す問題

    A -- A1 -- A2 -- A3 -- A4
          |    |      |    |
          |    |      |    |
    B -- B1 -- B2 -- B3 -- B4

    1. 幹線道路 A のヒースロー空港から 1 つ目の交差点までの最短経路は何かを求める。選択肢は、直接 A を進むか、B からスタートして乗り換える（渡る）かの 2 つ。
    2. 同じ手続きで、幹線道路 B の最初の交差点までの最短経路を求めて記録する。
    3. 幹線道路 A の、さらに次の交差点までの経路は、幹線道路 A の直前の交差点からまっすぐ進むのが速いか、直前の B の交差点からまっすぐ進んで乗り換えるのが早いか調べて、
    　　早いほうを採用する。逆側の交差点についても同じことをする。
    4. これを目的地まで繰り返す。
    5. 目的地まで繰り返したら、2 つの経路のうち早い方が我々のとるべき最短経路である。
-}

--- ¶　道路網を Haskell で表現する
{-
    どうすればこの道路網を Haskell で表現できるだろうか？
    3 つの所要時間を 1 組として調べればよい。
    幹線道路 A のあるパーツ、幹線道路 B の同じパーツ、そして 2 つのパーツと接続し橋渡しをしているパーツ C である。

    つまり、

    --
      |
      |
    --

    これ単位で考えるということ。

    データ型はできるだけシンプルにしよう（でもシンプルならいいというわけではない）。
-}

data Section = Section { getA :: Int, getB :: Int, getC :: Int}
    deriving (Show)

type RoadSystem = [Section]
    -- 道路網 RoadSystem は道路 Section のリストである、という型シノニム

-- ヒースロー空港からロンドンまでの道路網は次のように表せる。
heathrowToLondon :: [Section]
heathrowToLondon = [ Section 50 10 30
                   , Section 5 90 20
                   , Section 40 2 25
                   , Section 10 8 0
                   ]


--- ¶　最短経路関数を求めよ！
{-
    どんな道路網に対しても最短経路が計算できる関数は、どんな型宣言にすればよいだろう？
    その関数は、道路網を引数として取って経路を返すべきである。
    経路もリストとして表現しよう。
    A, B, C を列挙しただけの Label 型を導入することにする。それから、Path という名前の型シノニムを作ろう。
-}

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

{-
    ということで、これから作る関数 optimalPath の型は次のようになるはず。

        ```
        optimalPath :: RoadSystem -> Path
        ```

    heathrowToLondon という名前の道路網を引数にして呼び出したら、以下のような経路を返すべきである。

        [(B,10), (C,30), (A,5), (C,20), (B,2), (B,8)] // これの型は Path つまり [(Label, Int)]

    道路 A での最短経路と道路 B での最短経路を保持しながら、セクションのリストを左から右へと辿っていこう。
    リストをたどりながら最善の経路をためていく。これってなんだろう？　正解は左畳み込み！

    手計算で解を求めたときは、あるステップを何度も繰り返した。直前の A と B までの最適経路、それに現在の道路セクションをもとに、
    A と B の新しい最適経路を求めるというステップである。開始時点で A と B に対応する最適経路は [] と [] である。
    そして Section 50 10 30 というセクションを読み込み、A1 への最適経路は [(B,10), (C,30)] であり、B1 への最適経路は [(B,10)] である、ということを求めた。
    この手続きを関数視点で見ると、経路のペアとセクションを引数に取って新しい経路のペアを返す関数になっている。
    ということは、その型は以下のようである。

        ```
        roadStep :: (Path, Path) -> Section -> (Path, Path)
        ```
    この関数を実装してみよう。
-}

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let timeA = sum (map snd pathA)
        timeB = sum (map snd pathB)
        forwardTimeToA = timeA + a
        crossTimeToA = timeB + b + c
        forwardTimeToB = timeB + b
        crossTimeToB = timeA + a + c
        newPathToA = if forwardTimeToA <= crossTimeToA
                        then (A, a):pathA -- なぜ pathA ++ [(A,a)] とせずに要素を前から追加している？→リストの先頭に追加するほうが、末尾に追加するよりもずっと速い！
                        else (C, c):(B, b):pathB
        newPathToB = if forwardTimeToB <= crossTimeToB
                        then (B, b):pathB
                        else (C, c):(A, a):pathA
    in (newPathToA, newPathToB)

{-
    上の関数 roadStep については、関数を読んでのとおりだが、不明点があればテキスト p222-223 へ。
    💡なお、この roadStep の実装では、timeA = sum (map snd patA) などとするときに経路の所要時間を毎回計算している。
    　 A と B の最短経路だけでなく、最短の所要時間を引数に取ったり返したりするしようにしておけば、この計算は不要。

    では、この関数を heathrowToLondon の最初のセクションについて走らせてみよう。
    最初のセクションなので、直前の A と B への最短経路に対応する引数は空リストのペアになる。
-}

firstRoute :: (Path,Path)
firstRoute = roadStep ([], []) $ head heathrowToLondon -- ([(C,30),(B,10)],[(B,10)])

{-
    経路は逆転しているので、右から左に読む。つまり、次の交差点 A までの最短距離は、B からスタートして A に乗り換える（C:Cross する）ことだとわかる。
    一方、次の B までの最短経路は、単に B を直進することである。

    経路のペアとセクションを取って最適経路を返す関数ができたところで、セクションのリストに左畳み込みを施すのは簡単である。
    roadStep は、([], []) と最初のセクションを引数にして呼ばれ、そのセクションまでの最適経路を求める。
    次に、その最適経路と次のセクションを引数に roadStep がまた呼ばれて、…と続く。
    すべてのセクションを歩き終わったら、最後に最適経路のペアが残る。
    そして、2 つのうち短いほうが答えである。これを念頭に optimalPath を実装しよう。
-}

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
    in if sum (map snd bestAPath) <= sum (map snd bestBPath)
            then reverse bestAPath -- この時点での経路は逆転している（(A,a):pathA というふうに前に前に追加していったから）
            else reverse bestBPath

{-
    この関数では、空リストのペアを初期アキュムレータとして、roadSystem（これはセクションのリストだった）を左畳み込みしている。
    その結果は経路のペアだから、パターンマッチを使って 2 つの経路の本体を取り出す。
    それからどちらの所要時間が短いかを判定して返す。
    経路リストの広報ではなく前方へ追記していく実装を選んだことから、この時点での経路は逆転しているので、返す前に逆順にしている。
    では試してみよう！
-}

optPath :: Path
optPath = optimalPath heathrowToLondon -- [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)]

{-
    この結果がまさに欲しかった結果である！
-}

--- ¶　入力から道路網を受け取る
{-
    最短経路を求める関数はできた。あとはテキスト形式で表現された道路網を標準入力から読み込んで RoadSystem 型に変換し、optimalPath 関数にかけて、得られた経路を表示するだけである。
    まず、リストを取り、ある要素数のグループごとに分割する関数を作ろう。名付けて groupsOf である。
-}

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs) -- やっぱりこれも再帰的。n 個ずつのグループに分けるので、n 個取ったらまたグループ分け、さらに n 個取ったらまたグループ分け、、とやっていく。

-- 例
groupsOf3 :: [[Int]]
groupsOf3 = groupsOf 3 [1..10] -- [[1,2,3],[4,5,6],[7,8,9],[10]]

{-
    準備は整った！
    いよいよ以下が、標準入力を読み取って RoadSystem を作り、最短経路を出力する main 関数である。
-}

-- import Data.List
main :: IO ()
main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathTime = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "Time taken: " ++ show pathTime

{-
    $ stack runhaskell src/10_functionally_solving_problems.hs < src/paths.txt
    The best path to take is: BCACBBC
    Time taken: 75

    👀 stack runhaskell ってコマンド今まで知らなかった。。！
-}

{-
    🎆 　Tips
        （もっと長い道路網を生成して試してみるときのためなど）スタックオーバーフローが出たら foldl を foldl' に変え、sum を foldl' (+) 0 に変えてみよう。
        または、実行前に `ghc -O hogehoge.hs` という具合にコンパイルする。
        コンパイル時に -O フラグを含めると、最適化が働いて、foldl や sum といった関数がオーバーフローしにくくなる。
-}
