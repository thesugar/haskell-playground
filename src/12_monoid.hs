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

