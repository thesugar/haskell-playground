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

-------------------------------
--　リストに注目する
-------------------------------
{-
    ジッパーは、ほぼどんなデータ型に対しても作れるので、リストと部分リストに対して作れるといっても不思議ではないだろう。
    なにしろ、リストだって木のようなものなのだから。

    ただ、一般的な木構造は各ノードに要素（のない木もあるが）と複数の部分木を持たせたものであるのに対し、
    リストは各ノードに 1 つの要素と 1 つの部分リストを持たせたものである。

    第 7 章で自作のリストを作ったときも、そのようにデータ構造を定義した。

        data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord) // Cons は : のことですよ

    これを、先ほどの二分木の定義と比べると、まさにリストは先頭要素（head）と先頭以外の要素（tail）からなる「一分木」であると理解できよう。

    例えば、[1,2,3] というリストは 1:2:3:[] と書き直せる。これは、head である 1 と、tail である 2:3:[] からなっている。
    2:3:[] にも head である 2 と、tail である 3:[] がある。そして 3:[] にとっては、3 が head で、tail は空リスト [] である。

    では、リストのジッパーを作ってみよう。部分リストの間での注目点の移動は前、後方向と呼ぶことにしよう（木の場合は上、（下のうち）左、右が移動可能な方向だった）。
    注目点は部分リストであり、それに加えて、前に進むたびにパンくずリストを残していくことにしよう。

    さて、リストに関するパンくずには、1 かけらあたりどんな情報を入れればよいだろうか？
    二分木を扱うパンくずのときは、親ノードのルート要素の情報に加えて、選ばれなかった部分木全体の情報も持たせる必要があった。
    それから、左へ行ったのか右へ行ったのかも記録していおく必要があった。
    要するに、親ノードに含まれていた情報のうち、選択して注目することにした部分木に含まれない情報はすべて保持しておく必要があったわけである。

    リストは木よりも単純である。
    まず、左右どちらに分岐したかは記録する必要がない。リストを深い方に辿る選択肢は 1 つしかないからである。
    しかもノードの部分木（つまり部分リスト）も 1 つしかないので、選ばなかった経路を記録する必要もない。
    どうやら、親ノードが持っていた情報だけを記録すれば済みそう。

    リスト [3,4,5] があったときに、直前の要素は 2 だったことさえ知っていれば、その要素をリストの head に戻して [2,3,4,5] を復元できる。

    二分木のジッパーを作ったときは、パンくずを表す Crumb というデータ型を作ったが、リストにとって 1 かけらのパンくずは単なる要素なので、わざわざデータ型に包むまでもない。
-}

type ListZipper a = ([a], [a])

{-
    第一のリストは注目しているリストを表し、第二のリストはパンくずリストに対応する。
    では、リストを前後に移動する関数を作ろう。
-}

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)
goForward ([], _) = ([], [])

goBack :: ListZipper a -> ListZipper a
goBack (xs, x:bs) = (x:xs, bs)
goBack ([], _) = ([], [])
goBack (xs, []) = (xs, [])

{-
    リストを前に進むときは、現在のリストの tail を新しい注目点とし、head をパンくずとして残す。
    リストを後ろに戻るときは、最新のパンくずを取り出して、現在のリストの先頭にくっつける。
    この 2 つの関数を動かしてみよう。
-}

myXs :: [Int]
myXs = [1,2,3,4]

fooXs :: ListZipper Int
fooXs = goForward (myXs, []) -- ([2,3,4],[1])

fooXs' :: ListZipper Int
fooXs' = goForward fooXs -- ([3,4],[2,1])

fooXs'' :: ListZipper Int
fooXs'' = goForward fooXs' -- ([4],[3,2,1])

fooXs''' :: ListZipper Int
fooXs''' = goBack fooXs'' -- ([3,4],[2,1])

{-
    このように、リスト構造に対するパンくずリストというのは、元のリストを逆順にしたものに他ならない。
    注目しているリストから取り除いた要素は、パンくずリストの先頭に継ぎ足されていく。
    そうしておけば、パンくずリストの先頭から要素を取り出しては注目しているリストに継ぎ足すだけで、いとも簡単にデータ構造を逆戻りできる。

    たとえばもしもテキストエディタを作ることになったら、今開いているファイルの各行を文字列のリストとして表現するのも 1 つの手である。
    そうしてジッパーを使えば、カーソルのある行を表現することができ、テキストの任意の箇所に新しい行を挿入したり削除したりする操作も簡単に書ける。
-}

-------------------------------
--　超シンプルなファイルシステム
-------------------------------

{-
ジッパーを使ったデモとして、ごく単純化したファイルシステムを木で表現してみよう。
そのファイルシステムに対するジッパーを作り、本物のファイルシステムのようにフォルダ間を移動できるようにしよう。

よくある階層的なファイルシステムはファイルとフォルダからなる。
ファイルは名前のついたデータの塊で、フォルダはそれらファイルを整理するためのもので、複数のファイルやフォルダを含むことができる。
今回の単純な例では、ファイルシステム内のアイテムは次のいずれかであるとしよう。

    - ファイル：名前がついていて、データが入っている
    - フォルダ：名前がついていて、複数のファイルやフォルダをアイテムとして含む

状況がよくわかるように、型シノニムとデータ型を作ろう。
-}

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

{-
    ファイルは 2 つの文字列からなる。1 つ目はファイル名で、2 つ目が中身のデータを表している。
    フォルダは、フォルダ名を表す文字列と、中身のアイテムのリストからなる。中身リストが空だったら、それは空フォルダということになる。
    （データ型の定義が再帰的になっていることに注目、、といっても List の定義も Tree の定義も右辺に自身（それぞれ List, Tree）を含んでいたし珍しいことではない。
    　→再帰的なデータ構造に関しては 7 章参照）

    いくつかのフォルダやサブフォルダが入っているフォルダを以下に例示する。
-}

myDisk :: FSItem
myDisk = Folder "root"
            [ File "KOHH.mp3" "After all, my local!!"
            , File "TheSmiths.mp3" "Queen is Dead"
            , Folder "pics" [
                File "Copenhagen.jpg" "the town with water"
                , File "Stockholm.jpg" "a old street named Gamura-Stan"
                , File "Oslo.png" "love your coffee"
                ]
            , File "secret.doc" "xxxxxxxxxx"
            , Folder "programs"
                [
                File "model.py" "def main(): df_train, df_test = load.data() if __name__ == '__main__': main()"
                , File "index.js" "console.log('hoge')"
                , File "not_a_virus.exe" "really not a virus"
                , Folder "haskell"
                    [File "best_hs_prog.hs" "main = print (fix error)"
                    , File "random.hs" "main = print 4"
                    ]
                ]
            ]

--- ¶　このファイルシステムのジッパーを作ろう！
{-
    これでファイルシステムはできたので、あとはジッパーさえあれば、ザッピングもズームインも自由、ファイルの追加・編集・消去も自在にできる。
    二分木やリストでもやったように、パンくずリストは、ここに行く！　と決めたもの以外のすべての情報を含む必要がある。
    1 かけらのパンくずには、今注目している部分木以外のすべてを保存するべきである。
    また、「穴」（欠落）の位置も覚えておかないと、上に戻ったときに直前の注目点を適切な位置に埋め戻すことができない。

    今回の場合、パンくずはフォルダにそっくりなデータ構造になるはず（ただし、今選択されているフォルダは含まれない）。
    「あれ、ではファイルは？」と思っただろうか？　ひとたびファイルに注目したら、ファイルシステムをそれ以上深く辿ることはできない。
    だから、「ファイルからきました」というパンくずが残されることはないはずである。ファイルは、あたかも空の木（Empty）のような役割なのである（=末端にあるもの）。

    例えば、"root" フォルダに注目している状態から "secret.doc" に注目している状態に移るときは、どんなパンくずを残せばいいだろうか？
    まず親フォルダの名前と、注目しているアイテムの前後にくるべき他のアイテムたちである。
    だから、Name がひとつと、アイテムのリストが 2 つ必要である。
    現在のアイテムの前にあったアイテムのリストと、後ろにあったアイテムのリストを分けておくことで、戻るときに現在のアイテムをフォルダのどこに戻せばいいか、ちゃんとわかる。
    そうやって穴の位置を記録するわけである。

    以下が我々のファイルシステムのパンくずの型である。
-}

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show) -- パンくずの型

-- そして、以下がジッパー。

type FSZipper = (FSItem, [FSCrumb]) -- [FSCrumb] は パンくず*リスト* の型。二分木の例でも、左へ行った、右へ行った、という記録を残すために、ジッパーはパンくずのリストを持っていた。

{-
    階層構造を上に戻るのはとても簡単。最新のパンくずを取って、現在の注目点とそのパンくずとから、以下のように新しい注目点を作る。
-}

fsUp :: FSZipper -> FSZipper
fsUp (item, (FSCrumb name ls rs):bs) = (Folder name (ls ++ [item] ++ rs), bs)
fsUp (_, []) = error "cannot go up"

{-
    パンくずには、フォルダの名前、フォルダの中で注目点より前にあったアイテムのリスト（その名は ls）、注目点より後ろにあったアイテムのリスト（その名は rs）が入っているから、上に戻るのは簡単。

    ではファイルシステムを奥に辿るのはどうだろう？
    今 "root" にいるとして、ファイル "secret.doc" に注目したいときは、後に残すパンくずにはフォルダ名 "root" と、"secret.doc" よりも前にあったアイテム、後ろにあったアイテムのリストを残す必要がある。
    以下が、アイテム名を引数に取って、現在のフォルダの中にあるファイルまたはフォルダに注目点を移す関数である。
-}

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in (item, FSCrumb folderName ls rs:bs)
fsTo _ (File _ _, _) = error "Error!"

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

{-
    fsTo は、Name と FSZipper を引数に取り、その名前を持つアイテムに注目した新しい FSZipper を返す。
    引数に与えられた名前を持つアイテムは、フォルダ内に存在しないといけない。
    この関数は、ファイルシステム全体を検索してくれるわけではない。カレントフォルダ内だけを探す。

    まず break を使って、いま探しているアイテムよりも前にあるものと後ろにあるものとにアイテムのリストを分ける。
    break は、述語とリストを引数に取り、リストのペアを返す関数である。
    ペアの第一要素はすべて、述語が False を返すような要素である。その後、述語が True を返すようなアイテムが 1 つでも見つかればそいつを含め、残りのアイテムは全部第二要素に入る。
    ここでは述語として、「名前とファイルシステムアイテムを引数に取り、名前が一致していれば True を返す補助関数 nameIs」を作って使っている。

    こうして、探していたアイテムより前にあるアイテム ls、探していたもの item、探していたものより後にいたもの rs が分類できた。
    この 3 つが break から取れたなら、あとは見つかったアイテムを注目点として提示し、パンくずに全部のデータを積めれば完成！

    探している名前を持つアイテムがフォルダにないときは、パターン item:rs が空リストに合致しようとしてエラーが出ることに注意。
    また、また、現在の注目点がフォルダでなくファイルである場合も、同じくエラーが出てプログラムがクラッシュする。

    これで、ファイルシステムの中を上下方向に移動できるようになった。
    ルートから "Oslo.png" というファイルまで辿ってみよう。
-}

nextFocus :: FSZipper
nextFocus = (myDisk, []) -: fsTo "pics" -: fsTo "Oslo.png"
 -- (File "Oslo.png" "love your coffee",[FSCrumb "pics" [File "Copenhagen.jpg" "the town with water",File "Stockholm.jpg" 
 -- "a old street named Gamura-Stan"] [],FSCrumb "root" [File "KOHH.mp3" "After all, my local!!",File "TheSmiths.mp3" "Queen is Dead"]
 --  [File "secret.doc" "xxxxxxxxxx",Folder "programs" 
 -- [File "model.py" "def main(): df_train, df_test = load.data() if __name__ == '__main__': main()",File "index.js" "console.log('hoge')",
 -- File "not_a_virus.exe" "really not a virus",Folder "haskell" [File "best_hs_prog.hs" "main = print (fix error)",File "random.hs" "main = print 4"]]]])

{-
    こうすれば nextFocus は File "Oslo.png" "love your coffee" に注目しているはず。
    では今度は上に行って、近所のファイル "Stockholm.jpg" を見てみよう。
-}

nextFocus2 :: FSZipper
nextFocus2 = nextFocus -: fsUp -: fsTo "Stockholm.jpg"
    -- (File "Stockholm.jpg" "a old street named Gamura-Stan",[FSCrumb "pics" [File "Copenhagen.jpg" "the town with water"] 
    -- [File "Oslo.png" "love your coffee"],FSCrumb "root" [File "KOHH.mp3" "After all, my local!!",File "TheSmiths.mp3" "Queen is Dead"]
    -- [File "secret.doc" "xxxxxxxxxx",Folder "programs" [File "model.py" "def main(): df_train, df_test = load.data() if __name__ == '__main__': main()",
    --    File "index.js" "console.log('hoge')",File "not_a_virus.exe" "really not a virus",
    -- Folder "haskell" [File "best_hs_prog.hs" "main = print (fix error)",File "random.hs" "main = print 4"]]]])

--- ¶　ファイルシステムの操作
{-
    ファイルシステムの中を移動できるようになった今となっては、ファイルシステムを操作するのも朝飯前。
    まずは、注目しているファイルもしくはフォルダの名前を変更する関数↓
-}
fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder _ items, bs) = (Folder newName items, bs) -- _ は変更前の名前
fsRename newName (File _ dat, bs) = (File newName dat, bs) -- _ は変更前の名前

{-
    さっそく "pics" フォルダの名前を "photos" にしてみよう。
-}

neoFocus :: FSZipper
neoFocus = (myDisk, []) -: fsTo "pics" -: fsRename "photos" -: fsUp
    -- (Folder "root" [File "KOHH.mp3" "After all, my local!!",File "TheSmiths.mp3" "Queen is Dead",Folder "photos" [File "Copenhagen.jpg" "the town with water",File "Stockholm.jpg" "a old street named Gamura-Stan",File "Oslo.png" "love your coffee"],File "secret.doc" "xxxxxxxxxx",Folder "programs" [File "model.py" "def main(): df_train, df_test = load.data() if __name__ == '__main__': main()",File "index.js" "console.log('hoge')",File "not_a_virus.exe" "really not a virus",Folder "haskell" [File "best_hs_prog.hs" "main = print (fix error)",File "random.hs" "main = print 4"]]],[])

{-
    "pics" フォルダまで降りて行って、名前を変え、再び上まで登った。では、現在のフォルダにアイテムを新規作成する関数は？以下のとおり。
-}

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) = (Folder folderName (item:items), bs)
fsNewFile _ (File _ _, _) = error "cannot use"

{-
    簡単！ただし、この関数はフォルダでなくファイルに注目しているジッパーに使うとクラッシュする（パターンマッチの 2 行目）。
    では "pics" フォルダにファイルを追加して、ルートまで戻ってみよう。
-}

novaFocus :: FSZipper
novaFocus = (myDisk, []) -: fsTo "pics" -: fsNewFile (File "Roma.jpg" "history and beauty") -: fsUp
    -- (Folder "root" [File "KOHH.mp3" "After all, my local!!",File "TheSmiths.mp3" "Queen is Dead",Folder "pics" [File "Roma.jpg" "history and beauty",File "Copenhagen.jpg" "the town with water",File "Stockholm.jpg" "a old street named Gamura-Stan",File "Oslo.png" "love your coffee"],File "secret.doc" "xxxxxxxxxx",Folder "programs" [File "model.py" "def main(): df_train, df_test = load.data() if __name__ == '__main__': main()",File "index.js" "console.log('hoge')",File "not_a_virus.exe" "really not a virus",Folder "haskell" [File "best_hs_prog.hs" "main = print (fix error)",File "random.hs" "main = print 4"]]],[])

{-
    これの何がすごいかというと、ファイルシステムを更新したときに、データ構造自体に修正が上書きされるのでなく、関数から新しいファイルシステム全体が帰ってくることである。
    この方式なら、古いファイルシステム（この場合は myDisk）にも新しい版（この場合は novaFocus）にも同時にアクセスできる。

    このようにジッパーを使えば、何もしなくてもバージョン管理がついてくる。
    データ構造を書き換えた後でも、旧バージョンのデータに何の問題もなくアクセスできる。
    これは何もジッパーに限った話ではなく、Haskell の性質である。
    Haskell のデータ構造は immutable（一度定義すると変更できないもの）だからである。
    ところがジッパーがあれば、そんな immutable なデータ構造の中を楽に効率よく移動できるようになり、Haskell のデータ構造の永続性がいよいよ輝きはじめる。
    （注）それでも自作のデータ構造にいちいちジッパーなんて作ってられないよ、と思っただろうか？→ Data.Generics.Zipepr モジュール！！
-}

-------------------------------
--　足下にご注意！
-------------------------------

{-
    これまで二分木、リスト、ファイルシステムといったデータ構造のジッパーを作ってきたが、いずれも、足下を確かめずに歩き崖から落ちても気にしないという実装だった。
    例えば、goLeft 関数は二分木のジッパーを取って左部分木に注目点を移す。

        goLeft :: Zipper a -> Zipper a
        goLeft (Node x l r, bs) = (l, (LeftCrumb x r):bs)

    でも、今の足場が空の木だったら？　つまり Node ではなく Empty だったら？
    すると、Node へのパターンマッチは失敗し、部分木を持たない空の木と合致するパターンはないので（テキストでは。↑ではそのパターンマッチも書いてるが結局右辺で error 関数により error を出すようにしてる）、
    実行時エラーをくらってしまう。

    今までのところは、空の木の左部分木に注目しようとする奴なんていないだろう、そんな部分木なんてないんだし、と想定している。
    空の木の左部分木に行く処理など意味がわからないし、便宜のために単に無視してきた。

    また、何らかの木のルートにいてパンくずリストが空のときに、さらに上に戻ろうとしたら？　同じエラーが発生するだろう。
    どうやらジッパーを使っているときは、一歩踏み出すたびにそれが人生最後の一歩になる覚悟がいるようだ。。
    いかなる移動も、運良く成功するかもしれないが、いつ失敗するかもわからないのである。

    と、これはどこかで聞いたセリフだ、、そうモナドである！　具体的には Maybe モナド、普通の値に失敗の可能性という文脈を追加するモナドである。

    では、Maybe モナドを使って、ジッパーの移動に失敗可能性という文脈を追加しよう。
    二分木を処理するジッパーをモナディック関数に変えてみよう。

    まず、goLeft と goRight が引き起こす可能性のある失敗をケアしよう。
    これまで、失敗するかもしれないという性質は、常に関数の返り値に反映されてきた。今回も例外ではない。
    以下が、失敗の可能性が追加された goLeft と goRight である。
-}

maybeGoLeft :: Zipper a -> Maybe (Zipper a)
maybeGoLeft (Node x l r, bs) = Just (l, (LeftCrumb x r): bs)
maybeGoLeft (Empty, _) = Nothing

maybeGoRight :: Zipper a -> Maybe (Zipper a)
maybeGoRight (Node x l r, bs) = Just (r, (RightCrumb x l): bs)
maybeGoRight (Empty, _) = Nothing

{-
    これで、空の木の部分木を取ろうとしたら Nothing が返るようになった。
-}

arrived :: Maybe (Zipper a)
arrived = maybeGoLeft (Empty, []) -- Nothing

arrived' :: Maybe (Zipper Char)
arrived' = maybeGoLeft (Node 'A' Empty Empty, []) -- Just (Empty,[LeftCrumb 'A' Empty])

{-
    よさそう！　では、上に行く場合は？　問題が起こるのは、上に行こうとしたときにパンくずが 1 つもない場合、つまりすでにルートにいた場合である。
    これが、木構造の境界に気をつけていないとエラーを投げる goUp 関数である。

        goUp :: Zipper a -> Zipper a
        goUp (t, (LeftCrumb x r):bs) = (Node x t r, bs)
        goUp (t, (RightCrumb x l):bs) = (Node x l t, bs)

    これを、もっと行儀良く失敗（fail gracefully）するようにしよう。
-}

maybeGoUp :: Zipper a -> Maybe (Zipper a)
maybeGoUp (t, (LeftCrumb x r):bs) = Just (Node x t r, bs)
maybeGoUp (t, (RightCrumb x l):bs) = Just (Node x l t, bs)
maybeGoUp (_, []) = Nothing

{-
    パンくずがあればそれでよし、新しい注目点を「成功した計算」の文脈に入れて返す。パンくずがなければ失敗を返す。
    以前は、この関数はジッパーを取ってジッパーを返していたので、こんな感じで連鎖させて木の中を歩き回ることができた。

        (freeTree, []) -: goLeft -: goRight

    ところが今は、Zipper a を返す代わりに Maybe (Zipper a) を返すようになったので、上記のような関数適用では連鎖できなくなった。
    第 13 章でピエールの綱渡りを扱ったときにも同じ問題にぶつかった。ピエールも、一歩ごとに失敗すsるかもしれない綱渡りを強いられていた。
    バランス棒の片側に鳥が止まりすぎると落ちてしまうからである。

    その経験を活用できる。関数適用の代わりに >>= を使えば良いのである。
    >>= は、文脈付きの値（今の場合、Maybe (Zipper a) という失敗する可能性の文脈）を取って関数に食わせつつ、文脈が適切に処理されることを保証してくれる。
    だから、ピエールがしたように、すべての -: 演算子を >>= で置き換えよう。それだけで再び関数を連鎖できるようになる。
-}

coolTree :: Tree Int
coolTree = Node 1 Empty (Node 3 Empty Empty)

afterWalk :: Maybe (Zipper Int)
afterWalk = return (coolTree, []) >>= maybeGoRight -- Just (Node 3 Empty Empty,[RightCrumb 1 Empty])

afterWalk' :: Maybe (Zipper Int)
afterWalk' = return (coolTree, []) >>= maybeGoRight >>= maybeGoRight -- Just (Empty,[RightCrumb 3 Empty,RightCrumb 1 Empty])

afterWalk'' :: Maybe (Zipper Int)
afterWalk'' = return (coolTree, []) >>= maybeGoRight >>= maybeGoRight >>= maybeGoRight -- Nothing

--- >>= を -: との対比で意識的に考えたことがそこまでなかったかも。わかりやすい！

{-
    まず、左の枝に空の木を持ち右の枝には「2 つの空の木を持つノード」を持つ木 coolTree を作る。
    ジッパーを Just に包むのには return を使い、それから >>= を使って maybeGoRight 関数に食わせていく。
    まず右へ 1 歩踏み出すというのは意味がある操作なので、結果は成功。2 歩でも大丈夫。
    だがこのとき、注目点は空の木になっている。そして右に 3 歩というのは意味の通らない操作である。
    空の部分木を右に辿ることはできない。これが結果が Nothing になった理由である。
    こうして、ピエールの綱のしたにあるような安全ネットが我々の木にも付いた！

    > 先ほどのファイルシステムも、存在しないファイルやフォルダに注目しようとするとか、さまざまな要員で失敗する可能性がある。
    > ファイルシステム操作関数も Maybe モナドを使って fail gracefully するように改造できる。
-}