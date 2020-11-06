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

--- ¶ 来た道を戻る
{-
    木を逆方向に辿り直すにはどうすればよいのだろう？
    [L,R] というパンくずリストからは、今の木が親の木の左部分木だったこと、そしてその親は、親の親の木の右部分木だったことはわかる
    （パンくずリストは L:R:[] だから、R に行って → L に行った、という順番になることに注意）。が、それがすべてである。
    パンくずリストだけでは、木構造を上向きに辿るための十分な情報がない。
    一つ一つのパンくずの中に、木を辿った方向だけでなく、木構造を戻るために必要なデータをすべて備えておく必要があるようだ。
    今回の場合、それは親の木のルート要素および右部分木（← 今回の restTree の例では）である。

    一般的に言って、1 つのパンくずには親ノードを構築するのに必要なすべてのデータを蓄えておく必要がある。
    つまり、辿った方向だけじゃなく、辿る可能性があった経路の情報も必要である。
    ところが、今注目している部分木の情報まで含んで胃はいけない。なぜなら注目している部分木の情報はタプルの第一要素に既出だからである。
    これまでもがパンくずに含まれていると情報が重複してしまう。

    情報が重複するのは避けたい。なぜなら、注目している部分木になんらかの変更が加わった場合、パンくずリストの中にある情報との間で一貫性が壊れてしまうからである。
    注目点の中で少しでも変更があると、重複している情報は期限切れで無効になる。しかも、もし木の要素数が多かったらメモリもバカ食いしてしまう。
    （🧵🦕ここらへんの議論、状態をすべて値として持つ（他言語のように参照で管理しない）Haskell らしさを強烈に感じるところだ。。）

    では、パンくずリストを改良し、これまで左右に移動するときに無視してきた情報をすべて含むようにしよう。
    Directions に変わる新しいデータ型を作る。
-}

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

{-
    今度は、ただの L に代わって LeftCrumb というコンストラクタがあり、移動元のノードに含まれていた要素と、辿らなかった右部分木をも持つようになっている。
    また R に代わって RightCrumb があり、やはり移動元のノードに含まれていた要素と、辿らなかった左部分木を持っている。

    この新しいパンくずリストなら、辿ってきた木を再構築できるだけの情報をすべて含んでいる。
    もはやただのパンくずリストというより、分岐点に行き当たるたびに残してきたフロッピーディスクのようなものである。
    辿った方向よりもずっと多くの情報を含んでいるわけだから。

    新しいパンくずは、本質的には「穴」のついた木のノードのようなものである（ここでいう穴とは、情報が落ちている（欠落させている（わざと））部分ということ）。
    木を 1 階層辿るごとに、パンくずには出発点のノードの情報のうち「いま注目することを選んだ部分 **を除く** （＝移動元のノードに含まれていた情報と、辿らなかった右or左部分木の情報）すべての情報」が書き込まれる。
    あと、穴がどこにあったのかも記録する必要がある。LeftCrumb の場合では、左に移動したことは知っているので、穴は左部分木のところに空いているのである。

    型シノニム Breadcrumbs にもこの更新を反映させよう。
-}

type Breadcrumbs a = [Crumb a]

{-
    続いて、goLeft 関数と goRight 関数も修正し、辿らなかった経路の情報を捨てるのではなくパンくずリストに記録するようにしないといけない。
    まず、以下が goLeft である。
-}

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, (LeftCrumb x r):bs)
goLeft (Empty, _) = error ""

{-
    この関数では、引数にきた木が Empty じゃないことを仮定している点に注意（といっても、Werror を設定していると、Pattern match are non-exhaustive エラーが出るので、いちおうエラーを出すようパターンマッチ書いてる）。
    空の木には部分木がないから、空の木から左に行こうとしたらエラーが出る。

    goRight も似たようなもの。
-}

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, bs) = (r, (RightCrumb x l):bs)
goRight (Empty, _) = error ""

{-
    さて、左や右に行くことなら、これまでも可能だった。今、さらに親ノードの情報と辿らなかった経路の情報を揃えたことで、
    経路を戻る能力を手に入れたはずだ。以下が goUp 関数である:
-}

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, (LeftCrumb x r):bs) = (Node x t r, bs)
goUp (t, (RightCrumb x l):bs) = (Node x l t, bs)
goUp (_, []) = error ""

{-
    木 t に注目している状態で、最新の Crumbs を調べる。それが leftCrumb であれば、（木 t にたどり着くためには親の木から左に進んだよ、という形跡としてのパンくずなので）現在の木である t を左部分木に使い、
    Node の残りの部分は、パンくずからの右部分木とルート要素の情報を使って埋める。
    そして、「履歴を戻る」操作をするためにリストの先頭のパンくずを拾ってしまったわけだから、新しいパンくずリストからは先頭要素を除いておく。

    木のてっぺんにいる場合に、さらに上に戻ろうとしてこの関数を使うと、やはりエラーになる（goUp (_, []) の部分）。
    あとで Maybe モナドを使って、注目点を移動しようとしたときに起こりうる失敗を表現できるようにする。

    Tree a と Breadcrumbs a のペアは、元の木全体を復元するのに必要な情報に加えて、ある部分木に注目した状態というのを表現している。
    このスキームなら、木の中を上、左、右へと自由自在に移動できる。

    あるデータ構造の注目点、および周辺情報を含んでいるデータ構造は Zipper と呼ばれる。
    注目点をデータ構造に沿って上下させる操作は、ズボンのジッパーを上下させる操作に似ているからである。というわけで、以下のような型シノニムを定義しておく。
-}

type Zipper a = (Tree a, Breadcrumbs a)

{-
    Zipper じゃなくて Focus という名前にしてもいい。データ構造の一部分に注目しているということをよく表す名前になるから。
    だが、このような設計を表す名前としては　Zipper のほうが広く普及しているので、ここは大勢に従っておこう。
-}

--- ¶　注目している木を操る

{-
    さて、これで上下に動ける（上：goUp, 下:（左）goLeft, （右）goRight）ようになったから、ジッパーが注目している部分木のルート要素を書き換える関数を作ろう。
-}

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify _ (Empty, bs) = (Empty, bs)

{-
    注目点がノードである場合、関数 f を使ってそのノードのルート要素を書き換える。もし注目点が空の木なら、そっとしておく。
    これでどんな木でも、好きなところへ辿っていき、要素を修正しつつも常に注目点を忘れず、いつでもまた上下に移動できるように保てる。
    例えば、「まず左へ行き、次に右へ行き、ルート要素を 'P' で置き換えるという修正を施す」という操作なら、以下のように書けるわけである。
-}

newFocus :: Zipper Char
newFocus = modify (\_ -> 'P') $ (freeTree, []) -: goLeft -: goRight
    -- (Node 'P' (Node 'S' Empty Empty) (Node 'A' Empty Empty),[RightCrumb 'O' (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)),LeftCrumb 'P' (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))])
    -- newFocus = (freeTree, []) -: goLeft -: goRight -: modify (\_ -> 'P') と書いてもいい。

{-
    望みとあらば、さらに 1 つ上へ行き、そこの要素を謎めいた 'X' に置き換えることもできる。
-}

newFocus2 :: Zipper Char
newFocus2 = modify (\_ -> 'X') (goUp newFocus)
    -- (Node 'X' (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)) (Node 'P' (Node 'S' Empty Empty) (Node 'A' Empty Empty)),[LeftCrumb 'P' (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))])

{-
    上への移動がここまで簡単になったのは、残してきたパンくずリストがデータ構造のうちの現在注目していない点を構成しており、しかも「逆向きに」、いわば裏返した靴下のようになっているからである。
    だからこそ上に移動するときにも、表向き煮直し、現在の注目点に継ぎ足せばいいのである。
    どのノードも 2 つの部分木を持つが、それは空かもしれない。だから、空の部分木に注目している場合、注目点を空でない部分木で置換すれば木を別の木の末端に継ぎ足す操作が作れる。
    そのためのコードもシンプル。
-}

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

{-
    木とジッパーを取って、注目点を与えられた木で置き換えた新しいジッパーを返している。
    これは、空の木を置換して木を伸ばすことだけでなく、既存の部分木を置き換えるのにも使える。
    それでは、おなじみの freeTre の一番左に新しい木を取り付けてみよう。
-}

farLeft :: Zipper Char
farLeft = (freeTree, []) -: goLeft -: goLeft -: goLeft -: goLeft
    -- (Empty,[LeftCrumb 'N' Empty,LeftCrumb 'L' (Node 'T' Empty Empty),LeftCrumb 'O' (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty)),LeftCrumb 'P' (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))])

newFocus' :: Zipper Char
newFocus' = farLeft -: attach (Node 'Z' Empty Empty)
    -- (Node 'Z' Empty Empty,[LeftCrumb 'N' Empty,LeftCrumb 'L' (Node 'T' Empty Empty),LeftCrumb 'O' (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty)),LeftCrumb 'P' (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))])
{-
    これで、newFocus は取り付けたばかりの新しい木に注目していて、木の残りの部分は裏返しでパンくずリストに入っている状態が作れた。
    ここから goUp で木のてっぺんまで戻ると、freeTree に対して左端に 'Z' が増えている木が作れるはずだ。
-}

--- ¶　まっすぐ、てっぺんまで行って、新鮮でおいしい空気を吸おう！
{-
    今どこに注目しているかによらず、木のてっぺんまで移動する簡単はすごく簡単に作れる。以下である。
-}

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)

{-
    パンくずリストが空なら、すでにルートにいるということだから、現状の注目点をそのまま返せばよい。
    そうでなければ、goUp で親ノードに注目したうえで、再帰的に topMost を使う。

    これで、木の中を歩き回り、左に右に、また上に行ったり、道すがら modify や attach を適用してまわったりできるようになった。
    そうして修正が済んだら、topMost を使ってルートまで上がり、これまで加えてきた変更を一望しよう。
-}

topView :: Zipper Char
topView = newFocus' -: topMost
 -- (Node 'P' (Node 'O' (Node 'L' (Node 'N' (Node 'Z' Empty Empty) Empty) (Node 'T' Empty Empty)) (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty))) (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty))),[])
 -- 左端に 'Z' が追加されていることがわかる。

