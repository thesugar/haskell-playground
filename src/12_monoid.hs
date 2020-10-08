{-# OPTIONS -Wall -Werror #-}
import Data.Monoid

---_/_/_/_/_/_/_/_/_/_/_/---
-- 　 　　 モノイド　　　　    --
---_/_/_/_/_/_/_/_/_/_/_/---

{-
    この章では、また 1 つ便利で楽しい型クラスを紹介する。それは Monoid である。
    モノイドは、値を二項演算子で結合できるような型を表す。
    この章では、モノイドとは正確には何なのか、モノイドの満たすべき法則とは何であるかを紹介する。
    それから、Haskell のモノイドの具体例や用途を見ていく。

    まずは newtype キーワードを見ていこう。
    モノイドの素晴らしい世界を探検していると、newtype キーワードをしょっちゅう使うことになるからである。
-}

-------------------------------
--　既存の型を新しい型にくるむ
-------------------------------

{-
    これまでに、data キーワードを使って独自の代数データ型を作る方法は学んだ。
    また、type キーワードを使って既存の型に型シノニムを与える方法も見た。

    この節では、newtype キーワードを使って、既存の型から新たな型を作る方法を見ていく。
    そもそもなぜ newtype キーワードが必要なのかという疑問にも答える。

    第 11 章では、リストをアプリカティブファンクターにする方法は複数あると言った。
    1 つ目は、<*> が左辺のリストから関数を 1 つずつ取り出して、それぞれを右辺のリストの中の値すべてに適用し、
    左辺のリストの関数と右辺のリストの値のあらゆる組み合わせを作る、というものだった。

        > [(+1), (*100), (*5)] <*> [1,2,3]
        [2,3,4,100,200,300,5,10,15]

    2 つ目の方法は、<*> の左辺のリストから先頭の関数を取り出して右辺の先頭の値に適用し、左辺の次の関数を取り出して右辺の次の値に適用し、…… と繰り返すもの。
    最終的に <*>  は 2 つのリストを綴じ合わせるような働きをする。
    そのために、ZipList a という型を導入したんだった。ZipList 型の値コンストラクタは ZipList で、そのフィールドは 1 つだけである。
    そのフィールドにはリストを入れる。そして、綴じ合わせるタイプのアプリカティブとしてリストを使いたいときには、リストを ZipList コンストラクタでくるむだけで済むように、
    ZipList を Applicative のインスタンスにしておく。
    事が済んでくるまれた中身を取り出すには、getZipList を使う。

        > getZipList $ ZipList [(+1), (*100), (*5)] <*> ZipList [1,2,3]
        [2, 200, 15]

    それで、これが newtype キーワードと何か関係があるのだろうか？　ZipList a 型のデータ宣言を自分ならどう書くか考えてみよう。
    1 つの方法は次のとおりだ:

        data ZipList a = ZipList [a]

    この型には値コンストラクタが 1 つしかなく、その値コンストラクタにはフィールドがまた 1 つだけあって、そこにリストが入っている。
    ZipList からリストを取り出す関数が自動的に手に入るように、レコード構文を使ってもよいだろう。

        data ZipList a = ZipList { getZipList :: [a] }

    この方法はどこも悪くないように見えるし、実際うまく動く。
    既存の型をある型クラスのインスタンスにする方法は 2 つあった（deriving による自動導出と、instance による手作業）。
    data キーワードは既存の型（= [a]）を別の型（=ZipList）でくるむためだけに使っていて、
    その別の型を第 11 章では instance によって Applicative のインスタンスにした（`instance Applicative ZipList where ~` というふうに）。

    Haskell の newtype キーワードは、まさにこのような「1 つの型を取り、それを何かにくるんで別の型に見せかけたい」という場合のために作られたものである。
    ZipList a は実際のライブラリでは以下のように定義されている。

        newtype ZipList a = ZipList { getZipList :: [a]}

    data キーワードの代わりに newtype キーワードが使われている。なぜだろう？
    まず、newtype は高速である。型をくるむのに data キーワードを使うと、コンストラクタに包んだりほどいたりするたびにオーバーヘッドがかかる。
    しかし newtype を使えば、「これは単に既存の型をくるんで作った新しい型だ（だから newtype と呼ぶ）、内部処理は同じまま違う型を持たせたいんだ」ということが
    Haskell に伝わる。Haskell は型推論を済ませた後、この点を考慮して、包んだりほどいたりする処理を省略してくれる。

    では、逆に、なぜ常に data の代わりに newtype を使わないのだろうか？　それは、newtype キーワードを使って既存の型から新しい型を作るときには、
    値コンストラクタは 1 種類しか作れず、その値コンストラクタが持てるフィールドも 1 つだけ、という制限があるからである。
    一方、data キーワードを使えば、複数の値コンストラクタを持つデータ型を作れるし、各コンストラクタには 0 以上の任意個数のフィールドを持たせることができる。
-}

data Profession = Fighter | Archer | Accountant

data Race = Human | Elf | Orc | Goblin

data PlayerCharacter = PlayerCharacter Race Profession

-- これはエラー
-- newtype Enemy = Daemon | Devil | Ghost

{-
    newtype で作った型に対する deriving キーワードも、data の場合と同様にまったく問題なく使える。
    Eq, Ord, Enum, Bounded, Show, Read のインスタンスを導出できる（GHC 言語拡張の GeneralizedNewtypeDeriving を使えば、元の型が所属していた任意の型クラスを導出できるようになる）。
    ある型クラスのインスタンスを導出したかったら、newtype で包もうとしている中身の型がすでにその型クラスのインスタンスである必要がある。
    例えば、次のようにすれば新しい型は等号が使え、文字列に変換できるようになる。
-}

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

--　使ってみよう。
charlist1 :: CharList
charlist1 = CharList "This will be shown!" -- charlist1 は CharList {getCharList = "This will be shown!"}

charFromCharlist1 :: [Char]
charFromCharlist1 = getCharList charlist1 -- "This will be shown!"

trueOrFalse1 :: Bool
trueOrFalse1 = CharList "benny" == CharList "benny" -- True

trueOrFalse2 :: Bool
trueOrFalse2 = CharList "benny" == CharList "oisters" -- False

{-
    この newtype の例では、値コンストラクタの型は次のようになる。
        CharList :: [Char] -> CharList
    この値コンストラクタは [Char] 型の値、例えば "my sharona" を取り、CharList 型の値を返す。
    一方、newtype 宣言でレコード構文を使ったことにより自動生成された getCharList 関数は、以下の型を持つ。
        getCharList :: CharList -> [Char]
    これは包んだりほどいたりの処理だと考えてもいいが、2 種類の型の間の変換だと考えることもできる。
-}

--- ¶　newtype を使って型クラスのインスタンスを作る
{-
    ある型を型クラスのインスタンスにしたいのだが、型引数が一致しなくてできない、ということがよくある。
    例えば、Maybe を Functor のインスタンスにするのは簡単である。Functor 型クラスの定義は ↓ だから。
        instance Functor f where
            fmap :: (a -> b) -> f a -> f b

    だから、まずこう（↓）書いて、
        instance Functor Maybe where

    あとは fmap を実装するだけ。
    Maybe 型コンストラクタが Functor 型クラスの定義のうち f の位置を占め、すべての型引数が埋まる。
    fmap は、もし Maybe に限定して動作するなら、次のような型を持つだろう。

        fmap :: (a -> b) -> Maybe a -> Maybe b

    さて今度は、タプルを Functor のインスタンスにしたいと思ったとしよう。
    fmap をタプルに作用させて、第一の要素（fst）を変更するようにしたい！
    例えば fmap (+3) (1, 1) と書くと (4,1) になる、というような感じ。
    （ちなみに、実はタプルはデフォルトで Functor として実装されている。ただし、fmap (+3) (1, 1) とすると (1, 4) になる。）
    ところが、このようなインスタンスは、いざ書こうとすると書きにくいことがわかる。
    Maybe をインスタンス化するとき、instance Functor Maybe where と簡単に書けたのは、型変数が 1 つの型コンストラクタだけが Functor のインスタンスになれるからである。

    型 (a, b) について同じようなことを書いて、fmap が変更するのは型 a の部分だよ、と指定することはできそうにない。

        > 頭のこんがらがり防止：ふつうにタプルの最初の要素に数を足す素朴な関数を雑実装するには？（ファンクターは関係ない）
        addTup :: Num a => a -> (a, b) -> (a, b)
        addTup p (x, y) = (x+p, y)

    この制限を回避するために、タプルを newtype して 2 つの型引数の順番を入れ替えることができる。
-}

newtype Pair b a = Pair {getPair :: (a, b)}

{- これで、fmap が第一要素に作用するような形で、タプルを Functor のインスタンスにできる。 -}

instance Functor (Pair c) where
    fmap f (Pair (x, y)) = (Pair (f x, y))

{-
    ご覧のとおり、newtype で作った型にはパターンマッチも使える。
    fmap の実装では、まずパターンマッチを使って Pair 型から中身のタプルを取り出し、f をタプルの第一要素に適用し、
    Pair 値コンストラクタを使ってタプルを Pair b a 型に再変換している。
    Pair b a 型に限定した fmap の型は次のようになる。
        fmap :: (a -> b) -> Pair c a -> Pair c b

    ここでは、instance Functor (Pair c) where と定義されており、Functor の型クラス定義
    class Functor f where
        fmap :: (a -> b) -> f a -> f b

    の f に Pair c が収まっているのである。
    これで作りたかったものが作れた。タプルを Pair b a 型に変換すると、fmap が使えるようになり、
    関数をタプルの第一の要素に適用させることができる。
-}

pair1 :: (Int, Int)
pair1 = getPair $ fmap (*100) (Pair (2, 3)) -- (200, 3)

pair2 :: (String, Int)
pair2 = getPair $ fmap reverse (Pair ("london calling", 3)) -- ("gnillac nodnol",3)

--- ¶　newtype と遅延評価
{-
    newtype でできるのは、既存の型を新しい型に変えることだけである。
    だから Haskell は、newtype で作った型と元の型を型としては区別しつつ、同一の内部表現で扱っている。
    これは、newtype は data より高速で処理されるだけでなくパターンマッチがより怠惰になることを意味する。

    Haskell はデフォルトで遅延評価な言語である。つまり、関数の結果を実際に表示しろと言われるまでは、Haskell は計算を始めない。
    さらに Haskell は、関数の結果を表示するのにどうしても必要な部分の計算しか行わない。
    さて、undefined はぶっ壊れた計算を表す Haskell の値である。
    もし、端末に表示させるなどして、Haskell に undefined を評価させようとすると（つまり、どうしても undefined を計算させようとすると）、Haskell は例外を投げる。

    *Main> undefined 
    *** Exception: Prelude.undefined
    CallStack (from HasCallStack):
    error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err
    undefined, called at <interactive>:254:1 in interactive:Ghci3

    ところが、例えば undefined を要素に含むリストを作っても、その先頭要素を要求するだけなら、その先頭要素さえ undefined でなければ、
    すべてがうまくいってしまう。というのも、Haskell は先頭要素以外のリストの要素を評価する必要がないので、評価しないからである。

    たとえば次のとおり。

    *Main> head [1, undefined]
    1

    さて、ここで以下のような型を作ったとする。
-}

data CoolBool = CoolBool { getCoolBool :: Bool}

{-
    これは data キーワードで作った、いかにもありふれた代数データ型である。
    CoolBool 型には 1 つの値コンストラクタ CoolBool があり、それには 1 つのフィールドがあって、中身は Bool 型である。
    さて、CoolBool 型をパターンマッチして、中身の Bool が True であるか False であるかによらず "hello" を返す関数を書いてみよう。
-}

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

{-
    ではこの関数を、正常な CoolBool 値でなく undefined に適用してみよう。
-}

udfd :: String
udfd = helloMe undefined
    -- > udfd
    -- "*** Exception: Prelude.undefined

{-
    例外発生！！　どうしてこの例外が出たのだろうか？
    それは、data キーワードで定義された型には複数の値コンストラクタがあるかもしれず（CoolBool にはたまたま 1 つしかないが）、
    helloMe 関数に与えられた引数が (CoolBool _) に合致するかどうかを確認するためには、どのコンストラクタが使われたのかわかるところまで引数の評価を進める必要があるからである。
    （helloMe 関数の定義に出てくる helloMe (CoolBool _) = "hello" の CoolBool は型名でなく値コンストラクタ（つまり、CoolBool = Hoge { getHoge :: Bool } なら helloMe (Hoge _) = と書くことになる）。
    だから、data で型を定義した場合には data CoolBool = CoolBool | Hoge | Fuga のように値コンストラクタが複数ある場合もあって、そうした場合を想定すると、
    helloMe (CoolBool _) というパターンと一致しているかどうか判定するにはどの値コンストラクタを使っているか評価を進める必要があるということ。
    （CoolBool でなく Hoge や Fuga という値コンストラクタかもしれないから。）

    そして、undefined を少しでも評価しようものなら、例外が発生するのである。
    では、CoolBool を作るのに、data ではなく newtype を使ったらどうだろう？
-}

newtype CoolBool' = CoolBool' { getCoolBool' :: Bool }

-- helloMe 関数の実装は（変数名の ' を除いて）不変
helloMe' :: CoolBool' -> String
helloMe' (CoolBool' _) = "hello"

udfd' :: String
udfd' = helloMe' undefined -- udfd' は "hello" になる！

{-
    今度は動いた！
    これまでに学んだとおり、newtype を使ったときは、Haskell は新しい型の値も元の型の値も内部的には同じ表現を使う。
    新しい型に対して箱を追加するような処理はしていないし、ただ異なる型だと認識しているだけである。

    そして Haskell は、newtype キーワードはコンストラクタを 1 つしか作れないと知っているので、
    helloMe' 関数の引数を評価することなく、引数が (CoolBool' _) パターンに合致すると判定できる。
    なぜなら newtype には値コンストラクタもフィールドも 1 つずつしかないからである。

    いま紹介した事例は些細な違いに思えるかもしれないが、これは実に重要な違いである。
    この例が示しているのは、data キーワードで定義した型と newtype キーワードで定義した型はプログラマの視点からはそっくりに見えるかもしれない
    （どちらも値コンストラクタとフィールドがある）けれども、実際には 2 つの異なったメカニズムだということである。
    data はオリジナルな型を無から作り出すものである。これに対し newtype は、既存の型をもとに、はっきり区別される新しい型を作るものである。
    data に対するパターンマッチが箱から中身を取り出す操作なのに対し、newtype に対するパターンマッチは、ある型を別の型へ直接変換する操作なのである。
-}

--- ¶ type vs. newtype vs. data
{- あらためて、type と newtype と data の違いを整理しておこう。-}

-- 1️⃣ type キーワード
-- 型シノニムを作るためのもの。既存の型に別名をつけて、呼びやすくするのである。
type IntList = [Int]

-- こうすると、[Int] 型を IntList 型とも呼べるようになる。2 つの呼び名は自由に交換できるようになる。それだけ。
-- IntList 型の値コンストラクタとか、そういう類のものは一切生じない。

ints :: IntList
ints = ([1,2,3] :: IntList) ++ ([10,20,30] :: [Int]) -- [1,2,3,10,20,30]

{-
    型シノニムは、型シグネチャを整理してわかりやすくしたいときに使う。
    特定のものを表すために複雑な型を作ることがあるが、その型に名前をつければ、その型をどういう目的で使っているのかをコードを読む人に伝えられる。
    例えば第 7 章では、電話帳を表現するために [(String, String)] 型の連想リストを使った。
    そして、それに PhoneBook という型シノニムをつけることで、電話帳を扱う関数の型シグネチャを読みやすくした。
-}


-- 2️⃣ newtype キーワード
-- newtype キーワードは、既存の型をくるんで新しい型を作るためのもの。newtype は、もっぱら型クラスのインスタンスを作りやすくするために使われる。
-- newtype を使って既存の型を包むことにより出来上がる型は、元の型とは別物になる。次の例を見てみよう。
newtype MojiList = MojiList { getMojiList :: [Char] }

{-
    このとき、CharList と [Char] を ++ で連結することはできない。また、2 つの CharList を ++ で連結することすらできない。
    なぜなら、++ はリスト限定の演算子であり、CharList はリストを中身に持っているとはいえ、リストそのものではないからである。
    しかし、CharList をいったんリストに変換したうえで ++ して、また CharList に戻すことならできる（それはそう）。

    newtype 宣言でレコード構文を使うと、新しい型と元の型を相互変換する関数が作られる。
    具体的には、newtype の値コンストラクタと、フィールド内の値を取り出す関数である。
    新しい型は、元の型の所属していた型クラスを引き継がないので、deriving で導出するか、インスタンス宣言を手書き（instance SomeClass TypeName where）する必要がある。
    （あるいは GeneralizedNewtypeDeriving を使う）

    newtype 宣言は、値コンストラクタが 1 つだけ、フィールドも 1 つだけという制限のついた data 宣言だとみなしても実用上は問題ない。
    もし自分が書いたコードにそんな data 宣言を見つけたら、newtype で代用できないか考えてみるとよいだろう。
-}

-- 3️⃣　data キーワード
-- 自作の新しいデータ型を作るためのもの。
-- data を使えば、フィールドとコンストラクタを山ほど備えたどんな途方もないデータ型だろうと作り出せる。
-- リストや Maybe のような型も、木も、何でも。

-- 🎉 まとめ 🍷
---- 型シグネチャを整理したいとか、「（型）名は体を表す」ようにしたいだけなら、型シノニムを使うとよかろう。
---- 既存の型をある型クラスのインスタンスにしたくて、新しい型にくるむ方法を探しているなら、newtype がぴったり！
---- 何かまったく新しいものを作りたい場合には、data が向いている！

-------------------------------
--　Monoid 大集合
-------------------------------
{-
    Haskell の型クラスは、同じ振る舞いをする型たちに共通のインターフェイスを提供するために使われている。
    最初に紹介したのは、等号が使える型のクラスである Eq や、順序が付けられる Ord といった単純なものだった。
    それから、Functor や Applicative のような、もっと面白い型クラスを見てきた。

    新しい型を作る人は、「この型には何ができるだろう？　どんな操作をサポートするだろう？」と考えて、
    その型に欲しい機能をもとに、どの型クラスのインスタンスを実装するか決める。
    もしその型の値どうしの等号を定義することに意味があるなら、Eq のインスタンスにする。もしその型が何らかのファンクターになっっているなら Functor のインスタンスにする、などなど。

    さて、こんなことを考えてみよう。* は 2 つの数を取って掛け算をする関数である。
    そして、何かと 1 を掛け算すると、結果は常に元の数である。
    1 * x とやっても、x * 1 とやっても、結果は x である。
    次に、関数 ++ を考える。数の掛け算と 2 つのリストの連結は全然別種の操作に思えるが、2 つのものを取って 1 つを返すという点では同じ。
    しかも、++ には * と同じように、演算しても相手を変えない値（単位元！）がある。それは空リスト [] である。

    どうやら、* に 1 という組み合わせと、++ に [] という組み合わせは、共通の性質を持っているようだ。
        - 関数（ここでは `*` や `++`）は引数を 2 つ取る
        - 2 つの引数および返り値の型はすべて等しい
        - 2 引数関数を施して相手を変えないような特殊な値（ここでは `1` と `[]`）が存在する

    よく観察すると、ほかにも共通の性質が見つかる。この関数を使って 3 つ以上の値を 1 つの値にまとめる計算をするとき、
    値の間に関数を挟む順序を変えても結果は変わらない、という性質である。
        (3 * 2) * (8 * 5) == 3 * (2 * 8) * 5
        "la" ++ ("di" ++ "ga") == ("la" ++ "di") ++ "ga"

    この性質を結合的（associativity）と呼ぶ。演算 * と ++ は結合的であると言う。
    結合的でない演算の例は - である。例えば (5 - 3) - 4 と 5 - (3 - 4) は異なる結果んある。

    以上の性質に気づいたなら、……モノイドに出会ったのである！
-}

--- ¶　Monoid 型クラス

{-
    モノイドは、結合的な二項演算子（2 引数関数）と、その演算に関する単位元からなる構造である。
    ある値がある演算の単位元であるとは、その値と何か他の値を引数にしてその演算を呼び出したとき、返り値が常に他の値のほうに等しくなる、ということである。
    1 は * の単位元であり、[] は ++ の単位元である。
    Haskell の世界では、ほかにも無数のモノイドがあるので、Monoid 型クラスが用意されている。
    Monoid の定義を見てみよう。

    class Monoid m where
        mempty :: m
        mappend :: a -> a -> a
        mconcat :: [a] -> a

    まず、Monoid のインスタンスになれるのは具体型だけだとわかる。型クラス定義に現れる m が型引数を取っていないからである。
    この点で Monoid は、Functor や Applicative のような、1 つの型引数を取る型コンストラクタがインスタンスになる型クラスとは違っている。

    最初の関数は mempty である。いや、引数を取らないので関数ではない。
    mempty は多相定数である。Bounded の minBound みたいなもの。
    mempty は、そのモノイドの単位元を表す。

    次は mappend である。これは、モノイド固有の二項演算である。mappend は同じ型の引数を 2 つ取り、その型の別の値を返す。
    この関数の名前を mappend にしたのはちょっと残念。。何かを付け足す（append）という意味になってしまうから。
    ++ は確かに 2 つのリストを継ぎ足す操作だが、* には付け足すというような雰囲気は皆無だし、Monoid の他のインスタンスを見ても「付け足し」のような雰囲気はあまりない。
    それゆえ、mappend を「付け足す」と考えるのはやめて、単に 2 つのモノイド値を取って第三の値を返す関数とみなすようにしよう。

    最後の関数は mconcat である。これはモノイドのリストを取って mappend 間に挟んだ式を作り、単一の値を計算してくれる関数である。
    mconcat には、mempty を初期値に取り、リストを mappend で右畳み込みしていくというデフォルト実装がついている。
    ほとんどのモノイドに関してはこのデフォルト実装で十分なので、mconcat に関してはこれ以上深入りはしない。
    自分で Monoid 型クラスのインスタンスを作る時も mempty と mappend だけを実装すれば動く。
-}

--- ¶　モノイド則
{-
    📚モノイドが満たすべき法則

        - mempty `mappend` x = x
        - x `mappend` mempty = x
        - (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

    はじめの 2 つの法則は、mempty が mappend に関して単位元として振る舞う必要があることを述べている。
    第三の法則は、mappend が結合的であること、つまり複数の mappend で連結された式から 1 つの値を計算するとき、mappend を評価する順序は最終結果に影響しないことを述べている。
    Haskell はこれらの法則を強制しないので、インスタンスが実際にこれらの法則を満たすよう気を付けるのはプログラマの責任になる。
-}

-------------------------------
--　モノイドとの遭遇
-------------------------------

-- ¶　リストはモノイド
{-
    関数 ++ と空リスト [] がモノイドを成す。インスタンス宣言はとてもシンプル。

    instance Monoid [a] where
        mempty = []
        mappend = (++)

    リストは、中身の型が何であっても常に Monoid のインスタンスにできる。
    インスタンス宣言が instance Monoid [] ではなく instance Monoid [a] となっているのは、Monoid のインスタンスは具体型と決まっているからである。

        *Main Control.Applicative> [1,2,3] `mappend` [4,5,6]
        [1,2,3,4,5,6]

        *Main Control.Applicative> ("one" `mappend` "two") `mappend` "three"
        "onetwothree"

        *Main Control.Applicative> "one" `mappend` "two" `mappend` "three"
        "onetwothree"

        *Main Control.Applicative> "pang" `mappend` mempty 
        "pang"

        *Main Control.Applicative> mconcat [[1,2],[3,6],[9]]
        [1,2,3,6,9]

        *Main Control.Applicative> mempty :: [a]
        []

    最後の行には型注釈を明記してある。単に mempty とだけ書いても、どのインスタンスを使ってよいか GHCi にはわからないので、
    「ここではリストのインスタンスを使いたい」と伝える必要がある。
    ただし、[Int] や [String] ではなく、一般的な [a] という型でかまわない（[Int] や [String] で型注釈しても同じように [] が返ってくるけど）。
    空リストは任意の型を格納しているかのように振る舞えるからである。

    mconcat にはデフォルト実装が指定されているので、Monoid のインスタンスを作れば mconcat は勝手についてくる。
    リストの場合、mconcat は実はただの concat である。
    mconcat は二重リストを取って、その要素を連結した平らなリストを返す。というのも、二重リストの中の隣接するリストをみな ++ で結ぶとそうなるからである。

    リストは確かにモノイド則を満たす。
    複数のリストがあって、それを mappend、つまり ++ で結合していった結果は、どこから結合し始めても一緒だよね。
    なぜなら最後にはどのみちすべてが結合するわけだから。
    空リストが単位元であることもわかる。以上である。

    ところで、モノイド即は a `mappend` b と b `mappend` a が等しいこと（交換性）は要請していないことに気をつけて（※要請しているのは結合性）。
    リストの場合、これは明らかに成り立たない。（"hoge" ++ "fuga" と "fuga" ++ "hoge" は違う）
    でもこれで問題ない。 3 * 5 と 5 * 3 は確かに同じだが、それは掛け算の性質であって、すべてのモノイドがこの性質を満たすわけではない。
    実際、満たさないモノイドがほとんど。
-}

--- ¶　Product と Sum
{-
    数をモノイドにする方法の 1 つはすでに紹介した。* を二項演算にして 1 を単位元にする、という方法である。
    ほかにも、+ を演算にして 0 を単位元にするという方法もある。
    0 は加法（足し算）に関する単位元であるし、加法は結合法則も満たすから、モノイド則が成り立っている。

    さて、数をモノイドにする 2 つの方法からどちらを選べばよいだろう？　実は、1 つだけ選ぶ必要はない。
    ある型に対して同じ型クラスのインスタンスを複数定義したかったら、newtype に包んで新しい型をインスタンスにするという方法があった。
    二兎を追って両方を捕まえることができるのである。

    Data.Monoid モジュールは、この用途のために、Product と Sum という 2 つの型をエクスポートしている。
    （当たり前だけど、リストの和や積を計算する sum 関数や product 関数とは別物）

    Product の定義は以下。

        newtype Product a = Product {getProduct :: a}
            deriving (Eq, Ord, Read, Show, Bounded)

    newtype ラッパーと導出したインスタンスがいくつかあるだけである。
    Product の Monoid インスタンスは次のような感じ:
    （Product はすでにあるので、ここでの実装例のネーミングは Product' にする）
-}

newtype Product' a = Product' { getProduct' :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product' a) where
    mempty = Product' 1
    -- mappend = (<>) ...Semigroup の演算子(<>)と規定で同じ定義になるため、省略可能

instance Num a => Semigroup (Product' a) where
    (Product' x) <> (Product' y) = Product' (x * y)

-- 💡すごいH本の書き方とはちょっと違う（いろいろ仕様が変わったっぽい）
-- 参考
-- https://qiita.com/kurunin52/items/c555f30b4ea7362d3cf1#monoid%E3%82%A4%E3%83%B3%E3%82%B9%E3%82%BF%E3%83%B3%E3%82%B9%E3%81%AE%E5%AE%9A%E7%BE%A9%E6%96%B9%E6%B3%95
-- https://kazu-yamamoto.hatenablog.jp/entry/20180306/1520314185
-- https://blog.miz-ar.info/2019/02/monoid-for-haskellers/#Semigroup-Monoid_ProposalGHC_82

{-
    Product モノイドの mempty は、ただの 1 を Product コンストラクタにくるんだものである。
    mappend は、Semigroup のインスタンス宣言における <> の定義に準ずる。
    <> は、Product をパターンマッチしており、中身の数を掛け算してそれをまたコンストラクタにくるんでいる。
    そして、見ての通り、Num a という型クラス制約がついている。
    すでに Num のインスタンスであるようなすべての型 a について、Product a は Semigroup のインスタンスになり、ひいては Monoid のインスタンスになるということ。
    Product a をモノイドとして使うには、newtype に包んだりほどいたりする必要がある。

    ⭐️  すごい H 本には Semigroup に関する記述が出てこない。
        すごい H 本では、newtype で作成した新しい型 Product を Monoid 型クラスのインスタンスにしようと思ったら
        そのまま mempty と mappend を実装するだけでよかったが、現在は少し異なる。
        mempty はそのまま実装できるが、mappend を実装するにあたってはまず Product を Semigroup 型クラスのインスタンスにして、
        連結演算子 <> の実装を書く。
        そうしたら、Monoid 型クラスにおける mappend は自動的に <> と等しい処理を行うようになる。

    *Main Data.Monoid> getProduct $ Product 3 `mappend` Product 9
    27
    *Main Data.Monoid> getProduct $ Product 3 `mappend` mempty 
    3
    *Main Data.Monoid> getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2
    24
    *Main Data.Monoid> getProduct . mconcat . map Product $ [3, 4, 2]
    24
    *Main Data.Monoid> getProduct $ Product 3 <> Product 2
    6
-}

{-
    Sum は　Product の近くでまったく同様に定義されていて、インスタンスもよく似ている。使い方も同じ。

    *Main Data.Monoid> getProduct $ Product 3 <> Product 2
    6
    *Main Data.Monoid> getSum $ Sum 2 `mappend` Sum 9
    11
    *Main Data.Monoid> getSum $ mempty `mappend` Sum 3
    3
    *Main Data.Monoid> getSum . mconcat . map Sum $ [1,2,3]
    6
-}

--- ¶　Any と All
{-
    モノイドにする方法が 2 通りあって、どちらも捨てがたいような型は、Num a 以外にもある。Bool である。
    1 つ目の方法は || をモノイド演算とし、False を単位元とする方法である。|| は論理和を表し、2 つの引数のいずれかが True ならば True を返し、そうでなければ False を返す関数である。
    False を単位元として使えば、確かに || は False を取れば False を返し、True を取れば True を返す。
    これを踏まえて newtype で Any が定義され、Monoid のインスタンスにされている。
    Any の定義とインスタンス定義はそれぞれ以下のとおり（Any はすでにあるので、ここでは Any' で実装する）。
-}

newtype Any' = Any' {getAny' :: Bool}
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any' where
    mempty = Any' False

instance Semigroup Any' where
    Any' x <> Any' y = Any' (x || y)

{-
    これが Any と呼ばれるのは、x `mappend` y は x か y のいずれか（any）が True であった場合に True になるからである。
    Any で包んだ Bool を 3 つ以上 mappend（または <>） した場合も同様で、リストの中居がいずれか 1 つでも True であった場合に全体が True になる。

    *Main Data.Monoid> Any' True
    Any' {getAny' = True}

    *Main Data.Monoid> Any' True <> Any' False <> Any' False
    Any' {getAny' = True}

    *Main Data.Monoid> getAny . mconcat . map Any $ [False, False, False, True]
    True
-}

{-
    Bool を Monoid のインスタンスにするもう 1 つの方法は、Any のいわば真逆である。
    && をモノイド演算とし、True を単位元とする方法である。論理積は、2 つの引数がともに True である場合に限り True を返す。
    以下がその newtype 宣言とインスタンス定義である。
-}

newtype All' = All' { getAll' :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All' where
    mempty = All' True

instance Semigroup All' where
    All' x <> All' y = All' (x && y) 

{-
*Main Data.Monoid> All' True
All' {getAll' = True}

*Main Data.Monoid> All' True <> All' False
All' {getAll' = False}

*Main Data.Monoid> getAll' . mconcat . map All' $ [False, True, True, False]
False

*Main Data.Monoid> getAll' . mconcat . map All' $ [True, True, True, True]
True
-}

--- ¶　Ordering モノイド
{- 
    Ordering 型は、ものを比較した結果を表すのに使い、LT, EQ, GT という 3 つの値のいずれかを取る型だった。
    Ordering のモノイドを見抜くのは少し難しいが、しかし Ordering の Monoid インスタンスは、わかってみれば今までのモノイドと同じく
    ごく自然な定義で、しかも便利なのである。

        instance Monoid Ordering where
            mempty = EQ

        instance Semigroup Ordering where
            LT <> _ = LT
            EQ <> y = y
            GT <> _ = GT

    このインスタンスは次のような仕組みになっている。
    2 つの Ordering 値を mappend すると、左辺の値が優先されるが、左辺が EQ である場合は別。
    左辺が EQ である場合、右辺が返り値になる。単位元は EQ である。
    最初は場当たり的なルールに見えるかもしれないが、実はこれは文字列を辞書順で比較するときのルールに合わせた定義になっている。
    2 つの文字列を辞書順比較するときは、まず先頭の文字を比較し、異なっていた場合は直ちに順番が決まる。
    ところが、先頭の文字が同じだった場合は、次の文字を比較し、……と繰り返す必要がある。

    例えば、ox と on という単語を辞書順で比較するときは、まず先頭の文字を比較し、それらは同じなので 2 文字目の比較に進む。
    すると、x は n より辞書順が大きいので、単語の順序も ox は on より大きかったことがわかる。
    EQ が単位元とされているのは、「2 つの単語の同じ位置に同じ文字を挿入しても辞書順は変化しない」ことに対応すると考えれば直感的に理解できよう。
    （例えば、oix と oin の順番は ox と on に等しいということ）

    Ordering の Monoid インスタンスでは x `mappend` y は y `mappend` x と一致しない、という点も注意が必要。
    左辺が EQ (=mempty) でない限り左辺が優先されるので、LT `mappend` GT は LT を返すが、GT `mappend` LT は GT を返す。

    *Main Data.Monoid> LT <> GT
    LT
    *Main Data.Monoid> EQ <> GT
    GT
    *Main Data.Monoid> GT <> LT
    GT
    *Main Data.Monoid> mempty <> LT
    LT
    *Main Data.Monoid> mempty <> GT
    GT

    では、このモノイドはどういうときに便利なのだろう？
    例えば、2 つの文字列を引数に取り、その長さを比較して Ordering を返す関数を書きたいとする。
    ただし、2 つの文字列の長さが等しいときは、直ちに EQ を返すのではなく、2 つの文字列を辞書順比較することとする。
    次のように書くのも一つの手だろう。
-}

lengthCompare_ :: String -> String -> Ordering
lengthCompare_ x y =
    let a = length x `compare` length y
        b = x `compare` y
    in  if a == EQ then b else a

-- しかし、Ordering はモノイドであるという知識を使えば、この関数はずっとシンプルに書ける。
-----    => Ordering モノイドのルール：基本的には左辺優先、左辺が EQ の場合は右辺を評価

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) <> (x `compare` y)

{-
    mappend（すなわち <>）は、左辺が EQ でなければ左辺、EQ であれば右辺を返すのだった（つまり、「基本的には左辺を使う。左辺で順序が決まらなかったら右辺で評価」）。
    だから、優劣をつける場合に重視したい比較条件を左辺に置けばよいのである。
    さて、今度は単語の中の母音の数も比較して、それを 2 番目に重要な条件にしたくなったとしよう。
    ならば、こう修正すればよい。
-}

lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = 
    (length x `compare` length y) <>
    (vowels x `compare` vowels y) <>
    (x `compare` y)
    where vowels = length . filter (`elem` "aeiou") -- この母音の数を数える補助関数自体もいいね👍

{-
    *Main Data.Monoid> lengthCompare' "zen" "anna"
    LT
    *Main Data.Monoid> lengthCompare' "zen" "ana"
    LT
    *Main Data.Monoid> lengthCompare' "zen" "ann"
    GT

    1 つ目の例では、"zen" のほうが "anna" より短いため、LT が返ってくる。2 つ目の例では、長さは等しいが、2 つ目の文字列のほうが母音が多いため、これも LT が返ってくる。
    3 つ目の例では、どちらの文字列も同じ長さで母音の数も等しいため辞書順比較が行われ、"zen" のほうが大きい（GT）という結果んある。
    このように Ordering モノイドは、さまざまな条件でものの大小を比較し、条件そのものに「最も重視すべき条件」から「どうでもいい条件」まで優先順位をつけて最終判定を出すのに使える！
-}

--- ¶ Maybe モノイド
{-
    Maybe a も複数の方法でモノイドになれる。Maybe a がどのような Monoid インスタンスになるのか、どんなことに使えるのか、見ていこう。
    Maybe a をモノイドにする 1 つ目の方法は、型引数 a がモノイドであるときに限り Maybe a もモノイドであるとし、
    Maybe a の mappend を、Just の中身の mappend を使って定義することである。
    Nothing を単位元とし、mappend される 2 つの値のうち片方が Nothing であれば他方の値を使う、とする。
    以下がインスタンス宣言である。

        instance Monoid a => Monoid (Maybe a) where
            mempty = Nothing
        
        instance Semigroup a => Semigroup (Maybe a) where
            Nothing <> m = m
            m <> Nothing = m
            Just m1 <> Just m2 = Just (m1 <> m2)

    型クラス制約を見てほしい。a が Monoid のインスタンスである場合に限り、Maybe a を Monoid のインスタンスにする、と書いてある。
    「何か」と Nothing を mappend (<>) した結果は、その「何か」になる。
    2 つの Just 値を mappend した場合は、2 つの Just の中身を mappend して、また Just の中に入れる。
    これができるのも、型クラス制約によって Just の中身の型は Monoid のインスタンスであることが保証されているからこそである。
-}

result1 :: Maybe String
result1 = Nothing <> Just "andy" -- Just "andy"

result2 :: Maybe Ordering
result2 = Just LT <> Nothing -- Just LT

result3 :: Maybe (Sum Int)
result3 = Just (Sum 3) <> Just (Sum 4) -- Just (Sum {getSum = 7})

----------------------------
--- （※）以下は、Semigroup を使ったスタイルに書き直すために、自分で Maybe っぽい newtype（Maybe っぽいと言っても、
--- Maybee (Just 10) のように、Maybe 型を引数に取ってコンストラクトされる）Maybee 型を作って実験してみたもの。
newtype Maybee a = Maybee { getMaybee :: Maybe a}
    deriving (Show, Read, Eq, Ord)

instance Monoid a => Monoid (Maybee a) where
    mempty = Maybee Nothing

instance Semigroup a => Semigroup (Maybee a) where
    Maybee Nothing <> m = m
    m <> Maybee Nothing = m
    Maybee (Just m1) <> Maybee (Just m2) = Maybee (Just (m1 <> m2))

{-
*Main Data.Monoid> Maybee (Just 10)
Maybee {getMaybee = Just 10}
*Main Data.Monoid> getMaybee $ Maybee (Just (Sum 3)) <> Maybee (Just (Sum 4))
Just (Sum {getSum = 7})
-}
----------------------------

{-
    これ（Maybe モノイド）は、失敗するかもしれない計算の返り値をモノイドとして扱いたい場合に便利である。
    このインスタンスがあるおかげで、ここの計算が失敗したかどうかをいちいち覗き込まずに済む。
    Nothing か Just かの判定などせずに、そのまま普通のモノイドとして扱ってやればよいのである。
    でも、Maybe の中身が Monoid のインスタンスではなかったら？
    そういえば、中身がモノイドであることを利用したのは mappend の中身が Just である場合だけだった。
    中身がモノイドかどうかわからない状態では mappend は使えない。
    どうすればいいだろう？　1 つの選択は、第一引数を返して第二引数は捨てる（第一引数・第二引数というのは mappend 関数にとっての、ということで、<> の左辺右辺のこと）、と決めておくことである。
    この用途のために First a というものが存在する。
    以下が定義である。

        newtype First a = First { getFirst :: Maybe a }
            deriving (Eq, Ord, Read, Show)

    Maybe a が newtype で包まれている。Monoid インスタンスは以下である。

        instance Monoid (First a) where
            mempty = First Nothing

        instance Semigroup (First a) where
            First (Just x) <> _ = First (Just x)
            First Nothing <> x = x

    mempty は、ただ単に Nothing を First でっつんだものである。さて、mappend (<>) は、第一引数が Just 値なら第二引数を無視する。
    （mappend 関数が主語だから第一引数第二引数って言い方だけど、<> の左辺右辺のこと）
    もし第一引数が Nothing なら、第二引数が Just であろうと Nothing であろうと、それが返り値になる。

    *Main Data.Monoid> getFirst $ First (Just 'a') <> First (Just 'b')
    Just 'a'

    *Main Data.Monoid> getFirst $ First Nothing <> First (Just 'b')
    Just 'b'

    *Main Data.Monoid> getFirst $ First (Just 'a') <> First Nothing
    Just 'a'

    First は、いくつもある Maybe 値にどれか 1 つでも Just があるか調べたいときに役立つ。
    mconcat 関数が便利である。
-}
result4 :: Maybe Int
result4 = getFirst . mconcat . map First $ [Nothing, Just 9, Just 10] -- Just 9

{-
    逆に、2 つの Just を mappend したときに後の方の引数を優先するような Maybe a が欲しい、と言う人のために Last a 型も用意されている。
    これは First a とそっくりな働きをするが、mappend や mconcat を使ったときには Nothing でない最後の値が採用されるというものである。
-}

result5 :: Maybe Int
result5 = getLast . mconcat . map Last $ [Nothing, Just 9, Just 10] -- Just 10

result6 :: Maybe String
result6 = getLast $ Last (Just "one") <> Last (Just "two") -- Just "two"

