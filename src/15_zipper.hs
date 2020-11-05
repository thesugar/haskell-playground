{-# OPTIONS -Wall -Werror #-}


---_/_/_/_/_/_/_/_/_/_/_/_/---
--  　　    Zipper　　　　   --
---_/_//_/_/_/_/_/_/_/_/_/_/---

{-
    Haskell の純粋性は数多くの恵みをもたらすが、ある種の問題については、非純粋な言語では使わないような手段で解決する必要も生じる。
    Haskell は参照透過性を持つので、同じ値を表している 2 つの式の間に区別はない。
    ここで例えば、要素が 5 ばかりの木構造があり、そのどれか 1 つの要素を 6 に変えたいとする。
    すると、木の中の _どの 5_ を変更したいのか、明確に表現する方法が必要になる。木の中における変更点の位置を指定する必要があるのである。
    非純粋な言語では、該当の 5 が存在するメモリ上の位置を指定して書き換えれば済んだだろう。
    ところが Haskell では、どの 5 も他の 5 と同じで、純粋な「5 という値」でしかなく、メモリ上の位置をもとにした識別は不可能である。

    そもそも、Haskell では何かを**変える**ことはできない。「木を更新する」とは、実際には木を取ってそれとそっくりだけど少し異なっている木を返すことを意味する。

    1 つの手は、木のルート（根）から変更したい要素までの経路を覚えておくことである。
    「この木を取って、まず左に、次に右、また左に行って、そこの要素を変えてくれ」など。これは確かにちゃんと動くのだが、効率が悪くなりがちである。
    もし後で、さっき変更した要素の近くの要素を更新したくなっても、木をもう一度ルートから辿り直さなくてはならない。

    この章では、いくつかのデータ構造に、そのデータ構造の一部分に注目するための **Zipper** を備える方法を紹介する。
    Zipper はデータ構造の要素の更新を簡単にし、データ構造を辿るという操作を効率的にしてくれるのである。
-}

-------------------------------
--　歩こう
-------------------------------

{-
    木にはいろいろな種類がある。これから使う木の型を定義しておこう:
-}

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

{-
    この木は、空であるか、1 つ要素と 2 つの部分木を持つノードであるかのいずれかである、として定義されている。
    では、そんな木の一例を見てみよう。
-}

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

{-
    そして、この木を図にしたものが以下の図である。

                    　P
                　　／　　＼
            　　　O       　L
            　　／＼　　　　／＼
        　　　　L   Y   　W    A
        　　　／＼　／＼　／＼　／＼
    　　　  　N  T S  A C  R A   C

    木の中の W を P に変えたい。どうすればできるだろうか？
    たぶん、1 つの方法は、その要素にたどり着くまでパターンマッチを繰り返すことである。まずは右に、次は左に。以下のようになる:
-}

-- changeToP' :: Tree Char -> Tree Char
-- changeToP' (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

{-
    このコードは醜いどころの話ではないし、かなり認知的負荷が高い。
    まずルートの要素を x と名付け（freeTree のルートにある 'P' に合致する）、その左部分木を l とする。
    右部分木には、名前をつけるかわりに、さらに深いパターンマッチを使う。
    こうしてパターンマッチを繰り返して行って、やっと 'W' をルートに持つ部分木（の位置）にたどり着いたら、
    今度は集めた部品から木を再構成するわけだが、このときもともとルートに 'W' があった部分だけは 'P' を使う。

    なんというか、もとマシな方法はないのだろうか？　例えば、この関数が木と、方向のリストを引数に取るようにしたらどうだろう？
    方向とは L か R のいずれかで、それぞれ左、右に対応し、方向指示にしたがってたどり着いた位置の値を更新するという具合である。以下のように:
-}

data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r 
changeToP _ Empty = Empty -- これも付け加えないとパターンが網羅的でないというエラーが出る

{-
    方向リストの先頭要素が L なら、元の木に似ているが、左部分木のどこかの要素が 'P' に置き換わっている木を作って返す。
    このとき changeToP を再帰的に呼び出すにあたって、方向リストの tail 部分を渡す。　先頭要素が R の場合も同様。
    もし方向リストが空だったら、目的の場所に到着したということなので、元の木のルート要素を 'P' に置き換えただけの木を返す。

    木全体を出力するのも大変だから、方向リストを取って、目的地にある要素を返す関数も作ろう。
-}

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x
elemAt _ Empty = error "there is no elements."

{-
    この関数、よく見たら changeToP にそっくりである。違いといえば、経路に沿って出会ったものを覚えておいて木を再構築するのではなく、
    目的物以外のすべてを無視するようにできていることくらいである
    （changeToP のパターンマッチでは、右辺において Node x (changeToP ds l) r のように、ここでは x や r など、すべての要素を覚えておいて再構築している）。

    では、'W' を 'P' に書き換えたうえで、書き換えがちゃんと動いているか確かめてみよう。
-}

newTree :: Tree Char
newTree = changeToP [R,L] freeTree
    -- Node 'P' (Node 'O' (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)) (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty))) (Node 'L' (Node 'P' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))

newElem :: Char
newElem = elemAt [R,L] newTree
    -- 'P'

{-
    正しく動いているようだ。この 2 つの関数では、方向リストが、与えられた木の特定の部分木、いわば _注目点_ を指定する役割を果たしている。
    例えば [R] という方向リストは、ルートの右側の部分木を表している。空の方向リストは木全体を表す。

    この技はかっこいいと感じただろうか？　だが、これだと効率が悪い場合がある。特に要素の更新を何度もしたい場合は。
    もしも、とても巨大な木の末端のほうを長い長い方向リストで指定して、やっと書き換えたところに、また似たような位置の要素を書き換えたくなったら？
    またはじめから辿り直しになる。

    次の節では、部分木に注目するためのもっとよい方法を探すことにする。ある部分木から近場の部分木へと効率的に切り替えられるような方法である。
-}

--- ¶　背後に残った道標
{-
    部分木に注目するのに、毎回ルートから辿り直す必要のある方向リスト以外の手段を考えた方がよさそう。
    ルートから左、右、と進むときに、道標となるパンくずを残したらどうだろう？
    左へ行ったときには「さっきは左だった」、右へ行ったときには「あそこで右だった」と、往路の履歴を覚えておけば、逆方向の移動が作れそう。やってみよう。

    パンくずを表現するのにも方向値（L/R）を使おう。
    ただし、Directions と呼ぶ代わりに Breadcrumbs という名前にしよう。
    木構造を下るときにパンくずを残していくと、方向としては逆向きになるので。
-}

type Breadcrumbs' = [Direction]

{-
    以下は、木とパンくずリストを受け取って、木を左部分木へと辿りながら L をパンくずリストの先頭に追記する関数である。
-}

goLeft' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
goLeft' (Node _ l _, bs) = (l, L:bs)
goLeft' (Empty, _) = error ""

{-
    ルート要素と右部分木は無視して、左部分木と、L が先頭に追加されたパンくずリストを返している。
    右に行く関数は以下。
-}

goRight' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
goRight' (Node _ _ r, bs) = (r, R:bs)
goRight' (Empty, _) = error ""

{-
    これらの関数を使って、例の freeTree を右、続いて左に辿ってみよう。
-}

restTree :: (Tree Char, Breadcrumbs')
restTree = goLeft' (goRight' (freeTree, [])) -- (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])

{-
    これで 'W' をルートに、'C' を左部分木のルートに、'R' を右部分木のルートに持つ木が出てきた。
    パンくずリストは [L,R] である。まず右に、次に左に行ったから。

    木を辿る操作をもっときれいに書くために、第 13 章で定義した関数 -: を使おう。
    関数 -: の定義は次のとおりだった。
-}

(-:) :: a -> (a -> b) -> b
x -: f = f x

{-
    これで関数を値に適用するとき、「値 -: 関数」と書くことができるようになる。
    たとえば (goRight (freeTree, [])) と書くかわりに、(freeTree, []) -: goRight と書ける。
    この形式を使ってさっきの例を書き直すと、まず右、次に左に行った、というのがわかりやすくなる。
-}

restTree' :: (Tree Char, Breadcrumbs')
restTree' = (freeTree, []) -: goRight' -: goLeft' -- (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])

