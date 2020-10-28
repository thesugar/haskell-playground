{-# OPTIONS -Wall -Werror #-}
import Data.Monoid
import Control.Monad.Writer
import System.Random
import Control.Monad.State
import Data.Ratio

---_/_/_/_/_/_/_/_/_/_/_/---
--  もうちょっとだけモナド　　--
---_/_/_/_/_/_/_/_/_/_/_/---

{-
    これまで、モナドが値を文脈に入れること、文脈の中で関数適用が行えること、
    >>= や do 記法を使えば Haskell が文脈の面倒を見てくれるので、プログラマは値を扱うのに集中できることを見てきた。

    最後に出会った Maybe モナドは、失敗する可能性という文脈を値に付加するものだった。
    次にリストモナドを学び、実に簡単に非決定性計算を導入できることを見た。
    をういえば IO モナドも使っていた、しかも IO がモナドだったと知る前から！

    この章では、さらにいくつかのモナドを紹介する。それらを使うことで、普通の値をモナド値として扱えばプログラムがどんなにきれいに書けるかを体感できる。
    モナドの世界を探検することで、モナドを認識し、使いこなすための感覚が磨かれていくことだろう。

    この章に登場するモナドは、すべて mtl パッケージの一部である（Haskell のパッケージはモジュールの集まりである）。
    mtl パッケージは Haskell Platform に入っているので、たぶんすでに環境にインストール済みだと思われるが、mtl がインストールされているか調べるには、
    コマンドラインで
        $ stack exec ghc-pkg -- list
    と打つ。そうするとインストールされている Haskell パッケージの一部が出てくる。
    その中に mtl とバージョン番号が記された行があるはずである。
    mtl がインストールされていなければ、`$ stack exec ghc-pkg -- list` を実行してインストールしよう。
-}

-------------------------------
--　Writer？　中の人なんていません！
-------------------------------

{-
    これまで、Maybe モナド、リストモナド、そして IO モナドという武器をゲットしてきた。次は Writer である。
    Maybe モナドが失敗の可能性という文脈付きの値を表し、リストモナドが非決定性がついた値を表しているのに対して、
    Writer モナドはもう 1 つの値がくっついた値を表し、付加された値はログのように振る舞う。
    Writer モナドを使えば、一連の計算を行っている間、すべてのログが単一のログ値にまとめて記録されることを保証できる。
    最終的なログは、モナドの返り値にくっついて出てくる。

    例えば、デバッグ目的などで、何が起こっているのか説明する文字列を値にくっつけたいとしよう。
    盗賊団の人数を引数に取り、それが大きな盗賊団であるかどうか判定する関数を考えてみよう。とてもシンプルな関数である:
-}

isBigGang_ :: Int -> Bool
isBigGang_ x = x > 9

{-
    さて、ただ True か False を返すだけでなく、この関数が何をしたかを示す文字列も一緒に返してほしかったら、どうすればいいだろう？
    返したい文字列を作って、Bool と一緒に返せばよいだろう:
-}

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

{-
    これで、単に Bool を返すのではなく、第一要素が元来の返り値で、第二要素がそれに添えた文字列であるタプルを返すようになった。
    値に文脈が付いたわけである。動かしてみよう。

    *Main> isBigGang 3
    (False,"Compared gang size to 9.")

    *Main> isBigGang 30
    (True,"Compared gang size to 9.")

    これで問題なさそう。isBigGang は、普通の値を取って、文脈の付いた値を返す。
    ついさっきみたように、普通の値を渡すのには何の問題もない。
    では、すでに文字列が付いている値、例えば (3, "Smallish gang.") を isBigGang に食わせたかったら、どうしよう？
    なんか前にも聞いた話だ。
    普通の値を取って文脈の付いた値を返す関数があるとき、それに文脈の付いた値を食わせるにはどうしたらいいのだろう？

    前の章で Maybe モナドを探検しているとき、applyMaybe (>>= に相当) という関数を作ったことを思い出そう。
    この関数は Maybe a 型の値と a -> Maybe b 型の関数を引数に取った。
    関数のほうは Maybe a 型でなく a 型しか引数に取れないにもかかわらず、applyMaybe はそれに Maybe a 型の値を食わせることができた。
    それは、Maybe a 型の値についてくる文脈、つまり計算が失敗しているかもしれないという文脈を考慮することで可能になったのである。
    applyMaybe（のちの >>=）が、Nothing か Just かの文脈を処理してくれるので、関数 a -> Maybe b の中では、その値を普通の値として扱うことができたのである。

    同じ調子で、ログの付いた値、つまり (a, String) 型の値と、a -> (b, String) 型の関数の 2 つを取り、その値を関数のほうに食わせる関数を作ろう。
    名前は applyLog にしよう。
    (a, String) 値は、失敗の可能性という文脈ではなく、ログを表す値が付いているという文脈を持つ。
    したがって applyLog は、元の値に付いてきたログを失うことなく、関数が新たに生み出したログと結合するように注意を払ってくれる。
    以下が applyLog の実装である:
-}

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, oldLog) f = let (y, newLog) = f x in (y, oldLog ++ newLog) -- f は例えば isBigGang 関数

{-
    これまで文脈付きの値を関数に食わせたいと思ったら、まず実際の値を文脈から分離して、それに関数を適用し、
    それでもって文脈がどうなったかを見る、という手順を取ってきた。
    例えば Maybe モナドを扱うときは、まずそれが Just x であるか調べ、もしそうだったら x を取り出して関数を適用した。
    今回も、実際の値はとても簡単に見つかる。なぜなら本来の値とログのペアを扱っているのだから。
    というわけで、まず値のほうを取り出し、x と名付け、関数 f を適用する。
    すると (y, newLog) というペアが手に入る。ここで y は新しい値で、newLog は新しいログである。
    ただ、ここで newLog を直ちに返してしまうと古いログは結果から消えてしまうため、代わりに (y, oldLog ++ NewLog) を返す。

    applyLog を使ってみよう:
-}

gangLog :: (Bool, String)
gangLog = (3, "Smallish gang.") `applyLog` isBigGang -- (False,"Smallish gang.Compared gang size to 9.")

gangLog' :: (Bool, String)
gangLog' = (30, "A freaking platoon.") `applyLog` isBigGang -- (True,"A freaking platoon.Compared gang size to 9.")

{-
    実行結果は以前と似ているが、今度は盗賊団の人数にはじめからログが付いていて、それが結果のログにも含まれている。

    applyLog は isBigGang 関数以外とも使える。試してみよう。
-}

moreGang :: (Int, String)
moreGang = ("Tobin", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length.")) -- (5,"Got outlaw name.Applied length.")

moreGang' :: (Int, String)
moreGang' = ("Bathcat", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length.")) -- (7,"Got outlaw name.Applied length.")

{-
    ここではラムダ式の中で x は文字列になっており（※）、また、ログの追記はやはり applyLog が面倒を見てくれている。
    ※ isBigGang 関数（はラムダ式ではないが）では、x は Int で、x > 9 により True/False 判定をしていた。
-}

--- ¶　モノイドが助けにきたよ
{-
    現状の applyLog は (a, String) 型の値を取るようになっているが、ログは別に String である必要はないはずだ。
    ログへ追記するのには ++ を使っているのだから、文字のリストではなく、任意の型のリストが使えるのではなかろうか？
    もちろんそのとおりである。だから applyLog の型は以下に変更できる:
-}

-- applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c]) // 🤔でも❓↓

{-
    これでログはリストになった。ただし、最初のリストと関数が返すリストの中身の型は同じでないといけない。
    そうでなければ ++ を使ってくっつけることができないから。

    では、ByteString には使えるだろうか？　いかにもできそう。ところが、現状の型ではリストに対してしかつかえない。
    ByteString の applyLog は別に作らないといけないのだろうか。
    いや、リストも ByteString もモノイドだ！　ということは、どちらも mappend を実装しているということである。
    そして mappend はまさにものをくっつける操作である。

        > [1,2,3] `mappend` [4,5,6]
        [1,2,3,4,5,6]

        > [1,2,3] <> [4,5,6]
        [1,2,3,4,5,6]

        > B.pack [99,104,105] <> B.pack [104, 117, 97, 104, 117, 97]
        "chihuahua"
        # B.pack を使うときは　`import qualified Data.ByteString.Lazy as B` すること。

    これで applyLog が任意のモノイドを受け付けるようにできる。
    これを反映するよう、型と実装を変えないといけない。++ と mappend (<>) で取り換える。
-}

applyLog' :: Monoid m => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog' (x, oldLog) f = let (y, newLog) = f x in (y, oldLog <> newLog)

{-
    これで、applyLog が付加する値は任意のモノイド値になったので、別にタプルを「値とログのペア」と解釈する必要はなくなった。
    今や、「値と、モノイド値のおまけ」とみなすことができる。
    たとえば、商品の名前と価格（モノイド値）というのはどうだろう。newtype の Sum を使うだけで、商品を扱うとき価格が加算されることを保証できる。
    例として、これはぶっきらぼうに食事の注文を取って、飲み物を出してくれる関数である:
-}

-- import Data.Monoid
type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

{-
    ここでは、食べ物を文字列で表し、Sum で包んだ Int で値段を追跡している（Sum については 12 章）。
    復習だが、Sum を mappend (<>) すると、包まれた値は足し算されるのだった。
        > Sum 10 <> Sum 5
        Sum {getSum = 15}

    addDrink 関数はかなりシンプルである。まず、豆を食べているときは "milk" と一緒に Sum 25 を返す。
    つまり 25 セントが Sum に包まれているわけである。
    次に、ジャーキーを食べているときは、ウイスキーを飲むことにする。それ以外のものを食べるときはビールである。
    この関数を、普通に「食べ物」だけに適用してもさほど面白いことは起こらない。
    でも、「食べ物と値札の値」に applyLog を使って適用するのは一見の価値あり。
-}

order :: (Food, Price)
order = ("beans", Sum 10) `applyLog'` addDrink -- ("milk",Sum {getSum = 35})

order' :: (Food, Price)
order' = ("jerky", Sum 25) `applyLog'` addDrink -- ("whiskey",Sum {getSum = 124})

order'' :: (Food, Price)
order'' = ("dogmeat", Sum 5) `applyLog'` addDrink -- ("beer",Sum {getSum = 35})

{-
    ミルクは 25 セントだが、10 セントの豆と一緒に頼むと 35 セント払うことになる。
    これで、おまけの値の用途はログに限らないことがはっきりした。
    モノイド値だったらなんでもよく、その値のくっつけ方はモノイド次第である。
    実際、ログを扱っているときは文字列結合だったが、今は足し算になっている。

    addDrink が返す値は (Food, Price) のタプルなので、それにもう一度 addDrink を適用して、
    飲み物を追加注文し、値段の合計を知ることができる。 やってみよう。
-}

twoDrinks :: (Food, Price)
twoDrinks = ("dogmeat", Sum 5) `applyLog'` addDrink `applyLog'` addDrink -- ("beer",Sum {getSum = 65})

{-
    犬の肉に飲み物を注文すると、ビールが来て 30 セント追加されるので、結果は ("beer", Sum 35) になる。
    それにもう一度 applyLog' を使って addDrink すると、もう一杯ビールが来て ("beer", Sum 65) になる。
-}

--- ¶ Writer 型
{-
    これで、「モノイドのおまけのついた値」がいかにもモナド値のように振る舞うことがわかった。
    では、そのような値の Monad インスタンスを見ていこう。
    Control.Monad.Writer モジュールが、writer w a 型とその Monad インスタンス、それに Writer w a 型を扱うための便利な関数をエクスポートしている。

    値にモノイドのおまけをつけるには、タプルに入れるだけである。
    Writer w a 型の実態は、そんなタプルの newtype ラッパーに過ぎず、定義はとてもシンプル。

        newtype Writer w a = Writer ( runWriter :: (a, w)) 💡型変数の順番が左辺と右辺で逆順になってることに注意。12 章の「¶ newtype を使って型クラスのインスタンスを作る」参照。

    newtype に包むことで、Monad のインスタンスにするときに既存のタプルには影響を与えないようになっている。
    型引数 a が主となる値の型を表し、型引数 w がおまけのモノイド値の型を表している。

    Writer コンストラクタと同じことをする writer 関数も公開されており、これを使えばタプルを writer 値に変えられる。

    Writer 値コンストラクタ自体はエクスポートされていないので、それとパターンマッチして中身を取り出すこともできない。
    その代わり runWriter 関数を使う。
    これが newtype ラッパーである WRiter 値を取って中身のタプルを返してくれる。

    Monad インスタンスは、次のように定義されている。

    instance (Monoid w) => Monad (writer w) where
        return x = Writer (x, mempty) 💡ここの mempty は、実際に使われる時は型指定によって決まる。String の mempty なら "" だし Sum Int なら 0 だし、とか。
        (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v <> v')

    まずは >>= から見ていこう。
    この実装は applyLog と基本は同じ、ただしタプルが Writer による newtype ラッパーの中にいる点だけが違っており、
    パターンマッチを使って中身を取り出している（`Writer (x, v)` と書くことで x と v を取り出す）。
    出てきた値 x に関数 f を適用している。これで Writer w a 型の値が得られるから、さらに let 式でパターンマッチする（`let (Writer (y, v')) = f x` の部分で y と v' を取り出す）。
    そして y を新しい計算結果とする一方、mappend（<>） を使って 2 つのモノイド値を結合する。
    結合して得られた計算結果とモノイドをタプルに入れ、続いて Writer コンストラクタで包むことで、返り値を Writer 型にしている。
    生のタプルを返すわけにはいかないから。

    では return のほうはどうだろう？　return は、値をt追って、それを再現できるような最小限のデフォルト文脈に入れる必要がある。
    Writer 値にとってそのような最小限の文脈、つまりおまけのモノイド値とはなんだろう？
    他のモノイド値になるべく影響を与えないモノイド値を選ぶとしたら、単位元であるところの mempty を選ぶのがよいだろう。

    mempty はモノイドの単位元を表現するのに使われるんだった。例えば "" とか Sum 0 とか、空の ByteString とか。
    mempty と、何か他のモノイド値を mappend した結果は、常にその「他のモノイド値」である。
    そこで、return を使って Writer 値を作り、それを >>= を使って他の関数に食わせたら、結果のモノイド値は、関数が返したものがそのまま入っているはず。

    3 という数に対して、組み合わせるモノイド値を変えながら return を使ってみよう。
-}

three :: (Int, String)
three = runWriter (return 3) -- (3, "")

-- これはつまり、以下と同じ。
writerFoo :: Writer String Int -- ここの型引数の順番は three と逆になることに注意（Int と String の順番）
writerFoo = return 3 -- WriterT (Identity (3,""))

threeFoo :: (Int, String)
threeFoo = runWriter writerFoo -- (3,"")
-----

three' :: (Int, (Sum Int))
three' = runWriter (return 3) -- (3,Sum {getSum = 0})

three'' :: (Int, (Product Int))
three'' = runWriter (return 3) -- (3,Product {getProduct = 1})

{-
    Writer には Show インスタンスがないので、runWriter を使って、Writer 値を show が使えるタプルに変換している。
    String の単位元は空文字列になっている。
    Sum に対する単位元は 0 である。0 は何と足しても相手を返す。Product を使ったら、単位元は 1 になる。
    Writer インスタンスは fail の実装を与えていないので、do 記法の中でパターンマッチに失敗すると error が呼ばれる。
-}

--- ¶　Writer を do 記法で使う
{-
    こうして Monad インスタンスができたので、Writer を do 記法で自由に扱える。
    do 記法は複数の Writer をまとめて何かしたいときに便利。
    他のモナドの場合と同じく、プログラマにとっては普通の値のように扱える裏で、モナドが文脈の面倒を見てくれる。
    今回の場合は、すべてのモノイド値が mappend (<>) され、最終結果に反映される。

    Writer を do 記法で使い、2 つの数を掛け算する例:
-}

-- import Control.Monad.Writer
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got Number: " ++ show x]) -- ここでは writer 関数を使っている

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a * b)

    -- > runWriter multWithLog
    -- (15,["Got Number: 3","Got Number: 5"])

{-
    logNumber は、数を取って Writer 値を作り出す。Writer 値コンストラクタを使わずに、**writer 関数** 使って Writer 値を作っているところがポイント。
    モノイド値としては文字列のリストを使うことにし、入ってきた数に対して「その数が通ったよ」という記録を単一要素リストとして残す。
    multWithLog は、全体は 1 つの Writer 値で、中では 3 と 5 を掛け算しつつ、ログをもれなく残す。
    return を使って、a * b を最終結果として返している。
    return は、引数を最小の文脈に入れるものだったから、きっとログには何も追加しないだろうと、自信を持って言える。

    時には、ある時点でモノイド値（ここでは文字列のリスト）だけを追記したいことがあるかもしれない。
    そんなとき便利なのが tell である。tell は MonadWriter 型クラスの一部。
    Writer の場合は、モノイド値、例えば ["This is going on"] を取り、ダミー値 () を返しつつ、そのモノイド値を追記するという Writer を返す。
    返り値を変数に束縛する必要はない。
    multWithLog の特別な報告が追加されたバージョンは以下のようになる:
-}

multWithLog' :: Writer [String] Int
multWithLog' = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a * b)

        -- > runWriter multWithLog'
        -- (15,["Got Number: 3","Got Number: 5","Gonna multiply these two"])

{-
    return (a * b) が最後の行になっているのは重要。
    do 式の最後のモナドの結果が do 式全体の結果になるというルールだからである。
    仮に tell を最後の行においたら、do 式の結果は () になり、掛け算の結果は失われていただろう。
    ただし、ログは変更を受けない。
-}

--- 🎓独自検証----------------------------------
  --- Writer 値を作る時は、Writer 値コンストラクタではなく writer 関数を使用する。
moge :: Writer [String] Int
moge = writer (10, ["hoge"])
-- moge = Writer 10 ["hoge"]
-----------------------------------------------

--- ¶　プログラムにログを追加しよう！
{-
ユークリッドの互除法は、2 つの数を取ってその最大公約数を求めるアルゴリズムだ。
Haskell には、そのものずばり gcd 関数がすでにあるが、せっかくなのでログを残す機能のついたバージョンを自前で作ってみよう。
まず、以下が普通のアルゴリズムである:
-}

gcd_ :: Int -> Int -> Int
gcd_ a b
    | b == 0 = a
    | otherwise = gcd_ b (a `mod` b)

{-
    このアルゴリズムはとてもシンプルである。
    まず、第二引数がゼロかどうかを判定する。もしゼロなら、第一引数を返す。
    そうでないなら、第二引数と「第一引数を第二引数で割った余り」との最大公約数を返す。

    例えば、8 と 3 の最大公約数を、このアルゴリズムのとおりに求めてみよう。
    まず、3 は 0 でないので、3 と 2 (= 8 `mod` 3) の最大公約数を求めることになる。
    2 は、これも 0 ではないので、今度は 2 と 1 (= 3 `mod` 2) である。
    またしても 0 でないので、アルゴリズムは 1 と 0 (= 2 `mod` 1) に進み、ようやくのことで 0 になったので 1 を返す。
-}

gcdOf8and3 :: Int
gcdOf8and3 = gcd_ 8 3 -- 1

{-
    さて、この結果に、ログの役割をはたすモノイド値、という文脈を付けたい。
    さっきみたいに文字列のリストをログとして使おう。ということは、新しい gcd' 関数の型は次のようになるはず:
-}

gcd' :: Int -> Int -> Writer [String] Int

{- あとはこの関数にログをつけるだけである。以下が完成したコードだ: -}
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

{-
    この関数は、普通の Int 値を 2 つ取って、Writer [String] Int、すなわちログ取りという文脈のついた Int を返す。
    この関数は b が 0 であるとき、単純に a を返す代わりに、do 式を使って Writer 値を結果として返す。
    それには、まず tell を使って終了したことを伝え、次に return を使って a を do 式の結果としている。
    この do 式の代わりに、
        writer (a, ["Finished with " ++ show a])
    と書くこともできる。do 式を使うかどうかは好みだろう。

    次に、b が 0 でない場合がくる。
    この場合、mod を使って a を b で割った余りを求めたことを記録しておくことにする。
    それから do 式の 2 行目で gcd' を再帰的に呼び出す。
    ここで、gcd' は最後には Writer 値を返すことを思い出すと、gcd' b (a `mod` b) を do 式の結果行に置いておくのは正しいことだとわかる。

    この新しい gcd' を試してみよう。
    その返り値は Writer [String] Int であり、newtype から中身を取り出せばタプルとして読めるはずで、タプルの第一要素は計算結果のはず。
-}

gcdOf8and3' :: Int
gcdOf8and3' = fst $ runWriter $ gcd' 8 3 -- 1

-- OK! ではログのほうはどうだろう？　ログは文字列のリストだから、
-- mapM_ putStrLn でも使って画面に表示させてみることにしよう。

logging :: IO ()
logging = mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
    -- 8 mod 3 = 2
    -- 3 mod 2 = 1
    -- 2 mod 1 = 0
    -- Finished with 1

{-
    こんなふうに、普通のアルゴリズムを実行中に何をしているか報告するアルゴリズムに変えられるのって、すごい！
    しかも、普通の値をモナド値に変えるだけでそれができる。
    ログを集める作業は Writer の >>= の実装が勝手にやってくれる。

    このログ機能は、ほぼどんな関数にも追加できる。ただ、普通の値を Writer 値に、関数適用を >>= に変えればよいだけ。
    （あるいは do のほうが可読性が上がるかもしれない）
-}

--- ¶　非効率なリスト構築
{-
    Writer モナドを使うときは、使うモナドに気をつけて。
    リストを使うととても遅くなる場合があるからである。
    リストは mappend（<>） に ++ を使っているが、++ を使ってリストの最後にものを追加する操作は、そのリストがとても長いと遅くなってしまう。
    先ほど作った gcd' 関数のログ取りは速いほうだった。なぜなら、最終的に行われるリスト結合演算は以下のような感じになっていたからである:
        a ++ (b ++ (c ++ (d ++ (e ++ f))))
    リストは左から右へ構築されるデータ構造である。これが効率的なのは、まずリストの左辺を最後まで構築し、それから初めて右辺の長いリストを結合しているからである。

    でも、うっかりすると Writer モナドを使った結果、以下のようなコードができかねない:
        ((((a ++ b) ++ c) ++ d) ++ e) ++ f
    さっきのが右結合だったのに対し、これは左結合である。このコードは、右辺を左辺に結合しようとするたびに左辺をはじめから構築しないといけないので非常に非効率！

    今から示す関数は gcd' と似ているが、ログの出力が逆順になっている。
    再帰の各ステップは、まずプログラムの残りの部分のログを全部出力してから今のステップをログの最後に追加するようになっている。
-}

-- import Control.Monad.Writer
gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result

{-
    こいつは、まず再帰を呼び出してその結果を result という変数に束縛する。
    それから今のステップをログに追加するので、現在のステップは再帰が生成したログの最後にくる。
    最後に、再帰の結果を自身の計算結果として提示している。
    これを動かすと次のようになる:
-}

logging' :: IO ()
logging' = mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)
    -- Finished with 1
    -- 2 mod 1 = 0
    -- 3 mod 2 = 1
    -- 8 mod 3 = 2

{-
    この関数は、++ を右結合ではなく左結合で使ってしまうので、非効率的である。
    このようなやり方で結合していくとリストでは非効率になってしまう場合があるので、常に効率的な結合をサポートするデータ構造を使うのが一番だろう。
    そのようなデータ構造の 1 つが差分リストである。
-}

--- ¶　差分リストを使う
{-
    通常のリストに似ている **差分リスト** だが、その実態はリストを取って別のリストを先頭に付け加える関数である。
    例えば、[1,2,3] と等価な差分リストは \xs -> [1,2,3] ++ xs である。
    通常の空リストは [] だが、空の差分リストは関数 \xs -> [] ++ xs として表される。

    差分リストは効率の良いリスト結合をサポートする。普通のリストを 2 つ、++ で結合する時は、左辺のリストを最後まで延々と辿って行って、
    そこに右辺をくっつけないといけない。しかし、差分リストというアプローチをとってリストを関数として表現すると、何が起こるだろう？

    2 つの差分リストを結合する操作は、以下である。
-}

append :: (a -> b) -> (c -> a) -> c -> b
f `append` g = \xs -> f (g xs)

{-
    f と g は、リストを取ってその前に何かをつける関数だった。
    例えば、f が ("dog" ++)（別の書き方をすると \xs -> "dog" ++ xs）という関数で、
    g が ("meat" ++) という関数なら、f `append` g は次の関数と等価になる。
        \xs -> "dog" ++ ("meat" ++ xs)

    2 つの差分リストを結合した結果は、引数にまず 2 つ目の差分リスト、続いて 1 つ目の差分リストを適用する関数になるようだ。
    差分リストの newtype ラッパーを作ろう。そうすればモノイドインスタンスを作るのが楽になる。
-}

newtype DiffList a = DiffList { getDiffList :: [a] -> [a]}

{-
    包まれているものの型は [a] -> [a] （リストそのものではなく、リストを取ってリストを返す関数）である。
    差分リストは、リストを取って同じ型のリストを返す関数に過ぎないからだ。
    普通のリストを差分リストに変えたり、その逆をするのは簡単。
-}

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

{-
    普通のリストを差分リストにするには、さっきもやったような方法で、そのリストを引数リストの先頭に追加するような関数を作るだけである。
    そして差分リストはリストの前に何かを結合する関数なので、その「何か」を取り出したかったら、その関数を空リストに適用するまでである！
    以下が Monoid インスタンスである。
-}

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)

instance Semigroup (DiffList a) where
    (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))

{-
    差分リストをリストからリストへの関数と見た場合、mempty は id 関数であり、mappend (<>) は関数合成になっていることがわかるだろうか。
    これがうまく動くか試してみよう。
-}

lst :: [Int]
lst = fromDiffList (toDiffList [1,2,3,4] <> toDiffList [1,2,3]) -- [1,2,3,4,1,2,3]

{-
    うまく動いている！
    これで gcdReverse 関数の効率を上げられる。リストの代わりに差分リストを使うだけである。
-}

-- import Control.Monad.Writer

gcdReverse' :: Int -> Int -> Writer (DiffList String) Int
gcdReverse' a b
    | b == 0 = do
        tell (toDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcdReverse' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result

{-
    必要なことは、モノイド（つまり Writer のログ的な部分に入れるもの）の型を [String] から DiffList String に変えることと、tell を呼ぶには toDiffList で普通のリストを差分リストに変えることだけである。
    ログがきちんと組み立てられているか、確かめてみよう。
-}

logging'' :: IO ()
logging'' = mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse' 8 3
    -- Finished with 1
    -- 2 mod 1 = 0
    -- 3 mod 2 = 1
    -- 8 mod 3 = 2

{-
    まず gcdReverse' 8 3 をし、それから runWriter を使って newtype を剥がし、snd を使ってログだけを取り出し、
    fromDiffList を使って通常のリストに変換し、最後にその要素を画像に表示させている。
-}

--- ¶　性能の比較
{-
    差分リストがどのくらい速度を上げてくれるのか体幹sうるために、こんな関数を考えよう。
    その関数は、自然数の引数を取って、ひたすらゼロまでカウントダウンするが、gcdReverse のように逆向きのログを生成することで、
    ログの中では数がカウントアップされるようにする。
-}

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])

{-
    この関数に 0 を与えると、単にログを取る。
    他の数では、まずその数マイナス 1 のカウントダウンを呼び出してから、その数をログに加える。
    だから、finalCountDownを 100 に適用すると、文字列 "100" はログの最後に来るわけである。

    この関数を GHCi に読み込ませて、巨大な数（500000 とか）に適用すると、素早く 0 から数えだすのが見られる。
-}

count :: IO ()
count = mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000
        -- 0
        -- 1
        -- 2
        -- ...

-- しかし、差分リストの代わりに普通のリストを使ったらどうだろう？
finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
    tell ["0"]
finalCountDown' x = do
    finalCountDown' (x-1)
    tell [show x]

count' :: IO ()
count' = mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000
    -- 遅い！

-------------------------------
--　Reader？　それはあなたです！
-------------------------------

{-
    第 11 章では、関数を作る型、(->) r も、Functor のインスタンスであることを見た。
    関数 f で g を写すと、「g が取るのと同じ型の引数を取り、それに g を適用したものに f を適用して返す」関数ができるのだった。
    基本的にやっていることは「g のような関数」を作っているわけだが、ただし結果を返す前に f が適用されているわけである。
    （要は関数合成）
    以下が例。
-}

f_ :: Num a => a -> a
f_ = (*5)

g_ :: Num a => a -> a
g_ = (+3)

rslt :: Int
rslt = (fmap f_ g_) 8 -- 55

{-
    それから、関数はアプリカティブファンクターであることも見た。
    これにより、関数が将来返すであろう値を、すでに持っているかのように演算できるようになった。次が例である:
    （関数が返す値は、引数によって変わるので「将来返すであろう値」だが、引数に対する処理を (+) <$> (*2) <*> (+10) のような形で記述できるということ）
-}

fn :: Num a => a -> a
fn = (+) <$> (*2) <*> (+10)

rslt' :: Int
rslt' = fn 3 -- 19

{-
    (+) <$> (*2) <*> (+10) という式は、「ある数を引数に取って、それに (*2) と (+10) を適用し、
    その結果どうしを足し算する関数」になる。例えば、この関数を 3 に適用すると (*2) と (+10) の両方が 3 に適用され、
    6 と 13 ができる。それから 6 と 13 を引数に (+) が呼ばれ、結果は 19 になる。
-}

--- ¶　モナドとしての関数
{-
    関数の型 (->) r はファンクターであり、アプリカティブファンクターであるばかりでなく、モナドでもある。
    これまでに登場したモナド値と同様、関数もまた文脈を持った値だとみなすことができるのである。
    関数にとっての文脈とは、値がまだ手元になく、値が欲しければその関数を別の何かに適用しないといけない、というものである。

    これまでに、ファンクターやアプリカティブファンクターとして働く関数には精通してきたから、さっそく Monad インスタンスの設計を見に行こう。
    インスタンス宣言は GHC.Base にあり、次のようなソースになっている:

        instance Monad ((->) r) where
            return x = \_ -> x
            h >>= f = \w -> f (h w) w

    関数にとっての pure の実装は前に見た。return もほとんど pure と同じ。
    （pure は、値を取って「引数を無視して（関数の引数のことであって pure の引数ではない）つねにその値（pure の引数）を返す関数」だった）
    値を取って、その値を結果として返す最小限の文脈を常に返す。関数の場合、常に同じ値を返すようにする唯一の方法は、
    引数をガン無視させ（て、一定の値を返却させ）ることである。

    >>= の実装は暗号めいて見えるが、実際には見かけほど複雑ではない。
    >>= を使ってモナド値（ここでは h）を関数（ここでは f）に食わせるときは、結果は常にモナド値になる。
    だからこの場合、ある関数（=モナド値）を別の関数に食わせた結果は、また関数（ここでいう「モナド値」）になるはずである。
    それが、結果の式がラムダ（\）から始まっている理由である。
    これまで見てきた >>= の実装はすべて、何らかの形で値（=普通の値）をモナド（=文脈付きの値）から取り出して、それに関数 f （=普通の値を取って、文脈付きの値を返すもの）を適用するものだった。
    ここでも同じことが起こっている。
    関数から結果を取り出すには、それを何かに適用しないといけない。だから、(h w) を使って、それに f を適用しているわけである。
    f はモナド値（ここでは関数）を返すので、w にもそいつを適用する。
-}

--- ¶ Reader モナド
{-
    この時点で >>= がどう働くのか飲み込めなかったとしても心配無用。いくつか礼を見れば、これがシンプルなモナドであることがわかる。
    以下が関数モナドを使っている do 式である。
-}

addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a + b)
        -- addStuff 3 とすると結果は 19 になる

{-
    これは前に見たアプリカティブ式と同じものだが、関数がモナドであることを使っているバージョンである。
    _do 式は常にモナド値を生み出す_ もので、今回も例外ではない。
    この do 式が作るモナドは関数である。addStuff は整数を 1 つ関数に取る。
    まず、それに (*2) が適用され、結果は a になる。
    同じ整数に (+10) が適用され、その結果が b である。
    return は、他のモナドと同様、文脈には何も影響せず、（return の）引数に与えられた値をそのまま提示するモナド値を作る。
    これが a+b を関数全体の結果として提示する。

        addStuff 3
        > 19

    この例では (*2) と (+10) はどちらも 3 に適用される。実は、return (a+b) も同じく 3 に適用されるのだが、引数を無視して常に (a+b) を返している。
    そういうわけで、関数モナドは「Reader モナド」とも呼ばれたりする。すべての関数が共通の情報を「読む」からである。
    このことをもっと明確にするために、addStuff を次のように書き直すこともできる。
-}

addStuff' :: Int -> Int
addStuff' x = let
    a = (*2) x
    b = (+10) x
    in a+b
        -- addStuff' 3 とすると結果は 19

{-
    このように、Reader モナドは関数を文脈付きの値として扱うことを可能にする。
    関数が返すであろう値をすでに知っているつもりができるのである（a や b のことだと思われる）。
    それができるのは、複数ある関数を貼り付けて 1 つの関数を作り、それに渡った引数を構成要素の関数すべてに配っているからである。
    というわけで、もし最後の引数が届くのを待っている関数がたくさんあって、しかもそれらに渡したい値はみな同じ、という状況があれば、
    Reader モナドを使って未来の結果を取り出すことができる。
    うまく動くことは >>= が保証してくれる。
-}

-------------------------------
--　計算の状態の正体
-------------------------------

{-
    Haskell は純粋な言語だから、Haskell のプログラムはグローバルな状態や変数を書き換えたりできない関数だけで構成されている。
    関数は常に同じ計算をして値を返す運命なのである。
    この制限は、実のところプログラムについて考えるのを楽にしてくれる。
    なぜなら、特定の時刻でのすべての変数の値を考慮に入れる必要がなくなるわけだから。

    ところが、「状態」が本質的な問題、時間とともに変わっていく何らかの状態に依存している計算というのは確かにある。
    Haskell はそういった計算でも問題なく扱えるのだが、モデル化するのは少し骨が折れる。
    そこで Haskell には State モナドが用意されている。
    これさえあれば、状態付きの計算などいとも簡単。しかもすべてを純粋に保ったまま扱えるのである。

    第 9 章で乱数を見たときには、乱数ジェネレータを引数に鳥、乱数と新しい乱数ジェネレータを返す関数を扱った。
    複数の乱数が必要であれば、乱数と一緒に出てきた新しい乱数ジェネレータをつねに使うよう注意する必要があった。
    例えば、ジェネレータ StdGen を引数に取り、それを使ってコインを 3 回投げる関数を作るには、以下のようにする:
-}

-- import System.Random
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, _) = random newGen'
    in (firstCoin, secondCoin, thirdCoin)

-- この関数にジェネレータ gen を渡すと、まず random gen が Bool 値と新たなジェネレータを返す。
-- 2 枚目のコインを投げるには、この新しいジェネレータを使う。以下同様。

coinToss :: (Bool, Bool, Bool)
coinToss = threeCoins (mkStdGen 30) -- (True,False,False)

{-
    Haskell 以外のほとんどの言語では、乱数に添えて新しいジェネレータを返す必要などない。
    だってジェネレータの状態を上書きすればいいじゃん！
    でも Haskell は純粋な言語だから、そうはいかない。そこで、状態を受け取って、結果を作るとともに新しい状態も作り、
    その新しい状態を使って次の結果を作る必要がある。

    そんなふうに状態を手動で扱うのを避けるには Haskell の純粋性をあきらめる必要がある、と考えるかもしれない。
    だが、その必要はないのである。なぜなら、`State` モナドという、Haskell プログラミングをこんなに素敵にしている純粋性を少しも損なうことなく
    状態を扱うための面倒な作業を裏でやってくれるモナドがあるからである。
-}

--- ¶　状態付きの計算
{-
    状態付きの計算を実演するために、まずは型を与えてみよう。
    状態付きの計算とは、ある状態を取って、更新された状態と一緒に計算結果を返す関数として表現できるだろう。
    そんな関数の型は、次のようになるはずだ:
        s -> (a, s)
    s は状態の型で、a は状態付き計算の結果である。

        > 🍜 Note
        > Haskell 以外のほとんどの言語における「代入」操作は、状態付きの計算と捉えることができる。
        > 例えば、手続型言語で x = 5 と書くと、普通は変数 x に 5 が入り、ついでに式の値も 5 になる。
        > この挙動をよく観察すると、（これまでに代入されたすべての変数という）状態を取って、新しい状態と、(5) という結果を関数に見えてこないだろうか？
        > この場合の新しい状態というのは、代入前の変数のマッピングから新たに代入された部分だけが変わっている状態である。

    このような状態付きの計算（状態を取って、計算結果と新しい状態を返す関数）もまた、文脈付きの値だとみなすことができる。
    計算の結果が「生の値」であり、その計算結果をエルためには初期状態を与える必要があること、そして、
    計算の結果を得るのと同時に新しい状態が得られるというのが「文脈」にあたる。
-}

--- ¶　スタックと石
{-
    スタックをモデル化したいとしよう。
    stack とは、いくつかのデータを格納でき、次の 2 つの操作をサポートするデータ構造である。

    - Push: スタックのてっぺんに要素を積む
    - Pop: スタックのてっぺんの要素を取り除く

    スタックを表現するのにはリストを使い、リストの先頭がスタックのてっぺんに対応することにする。
    次のような 2 つのヘルパー関数を作ろう。

    - pop: スタックを引数に取って、要素を 1 つ取り出し、その取り出された要素を返す関数。ついでにその要素を除いたあとの新しいスタックも返す。
    - push: ある要素とスタックを引数に取り、その要素をスタックに積む関数。() を結果として返し、ついでに新しいスタックも返す。

    以下がその関数である:
-}

type Stack = [Int]

pop' :: Stack -> (Int, Stack)
pop' (x:xs) = (x, xs)
pop' [] = error "error" -- これがないとエラーになる。

push' :: Int -> Stack -> ((), Stack)
push' a xs = ((), a:xs)

{-
    push の返り値は () にした。push 操作の仕事はスタックを変更することで、特に重要な結果値というものはないからである。
    push の第一引数だけを部分適用すると、状態付き計算が生まれる（s -> (a, s) という型シグネチャになる。s は状態の型（ここでは Stack）で a は状態付き計算の結果（ここでは ()））。
    pop は、その型からして、すでに状態付き計算になっている（すでに s -> (a, s) という型シグネチャである）。

    これらの関数を使って、スタックをシミュレートするちょっとしたコードを書いてみよう。
    とりあえず、3 でも積んで、それから 2 つばかり値を取り出してみよう。
-}

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((), newStack1) = push' 3 stack
    (_, newStack2) = pop' newStack1 -- _ は、スタックから pop された値（Int）
    in pop' newStack2

{-
    この関数は、stack を取って、まず push 3 stack をする。その結果はタプルで、第一要素が ()、第二要素は新しいスタックである。
    この新しいスタックの名前は newStack1 としよう。次に newStack1 から新しい値を取り出す。
    その結果は _ という数（さっき積んだ 3 が入っているはず。後続の処理で使わないので _ に束縛）と、また新しいスタックである。
    今度は newStack2 という名前にする。
    さらに、newStack2 から数を取り出し、数 b とスタック newStack3 のタプルを手に入れる。
    stackManip は、このタプルをそのまま返している。使ってみよう:
-}

manipedStack :: (Int, Stack)
manipedStack = stackManip [5,8,2,1]
    -- (5,[8,2,1])

{-
    結果は 5 で、新しいスタックは [8,2,1] になった。
    stackManip 自身も状態付き計算になっていたことに気づいただろうか？（型を見ればわかる。 Stack -> (Int, Stack) で、s -> (a, s) というパターンになっている）
    今やったのは、状態付き計算をいくつか取って糊付けするという操作だったわけである。
    それってどこで聞いたことがあるような。。。

    さっきの stackManip のコードは、少々長ったらしい。すべての状態付き計算に、手で状態を与え、いちいち回収して名前をつけて、
    また次のやつに与えている。各関数にスタックを手動で与えるのではなくて、以下のようにかけたらすごいと思わないだろうか？
        stackManip = do
            push 3
            a <- pop
            pop
    なんと、State モナドを使うとまさにこんなふうにかけちゃうのである。
    State モナドがあれば、このような状態付き計算を、状態に手を触れる必要もなく扱えるのだ。
-}

--- ¶　State モナド
{-
    Control.Monad.State モジュールは、状態付き計算を包んだ newtype を提供している。
    以下がその定義である。
        newtype State s a = State { runState :: s -> (a, s) }

    State s a は、s 型の状態を操り、a 型の結果を返す状態付き計算である。
    （実態が s -> (a, s) だから、State モナドは確定的な値ではなく、（s -> (a, s) 型の）関数である）
    Control.Monad.Writer と同じく、Control.Monad.State も値コンストラクタをエクスポートしていない。
    状態付き計算を State の newtype に包みたい時は、state 関数を使おう。
    state 関数は State コンストラクタとまったく同じ動作をする。

    これで、状態付き計算とは何であって、それがどうして文脈付きの値とみなせるかがわかった。
    では状態付き計算の Monad インスタンスを見ていこう。

        instance Monad (State s) where
            return x = State $ \s -> (x, s)
            (State h) >>= f = State $ \s -> let (a, newState) = h s
                                                (State g) = f a
                                            in g newState

    return は、値を取って常にその値を結果として返すような状態付き計算にしたいわけである。
    それが、ラムダ式 \s -> (x, s) が出てくる理由である。
    こいつは常に x を状態付き計算の結果として提示し、状態には一切手をつけていない。
    return は値を最小限の文脈に入れるという約束だからである。
    というわけで、return はある値を提示し、状態を不変に保つような状態付き計算になるわけである。

    では、>= はどうだろう？　まず、状態付き計算を、>>= を使って関数に食わせた結果もまた状態付き計算にならないといけない、よね？（モナド計算の基本）
    そこでまず State の newtype ラッパーを書く（`State $...`）。それからラムダである（` \s -> let (a, newState) = h s...`）。
    このラムダ式が新しい状態付き計算になるのである。
    では、ラムダ式の中には何を書けばいいだろう？　とにかく 1 つ目の状態付き計算から結果の値を取り出さないといけない。
    我々は、まさに状態付き計算の中にいるから、現在の状態 s を状態付き計算 h に渡すことならできる。
        （h は（State h という書き方だと State s a の部分適用っぽく見えるけど、そうではなく、h は s -> (a, s) という状態付き計算を表している。
        　  実験：
            newtype Hoge s a = Hoge { getHoge :: s -> (a, s) }
            Hoge haha = Hoge \x -> (0, x)
            とすると
            > :t haha
            haha :: Num a => s -> (a, s)
            > haha "yeah"
            (0,"yeah")
        ） 
    すると計算結果と新しい状態のペア (a, newState) が出てくる。

    これまでのところ、>>= 演算子を実装するときは、必ずまず左辺のモナド値を使い、結果だけを取り出したあと、
    それに右辺の関数 f を適用して、新しいモナド値を得るという手順を踏んだ。
    Writer を作った時は、その手順にしたがって新しいモナド値を得た後、さらに 2 つのモノイド値を mappend する作業が必要だった。
    （(Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v <> v')）
    今回は、まず f a して、新しい状態付き計算 g を取り出している。
    こうして新しい「状態付き計算（g）」と新しい「状態」（newState）が揃ったら、あとは状態付き計算 g を newState に適用するだけ。
    その結果は、最終結果と最終状態のタプルになる！

    このように、>>= を使えば 2 つの状態付き計算を糊付けすることができる。
    2 つ目の計算は、1 つ目の計算の結果を受け取る関数の中に隠れている（g のこと？）。

    さて、pop と push はすでに状態付き計算だから、State ラッパーに包むのは簡単。
-}

-- import Control.Monad.State
pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

{-
    関数を State の newtype ラッパーで包むのに、State 値コンストラクタを直接使う代わりに state 関数を使っているのがポイント。
    pop はそのまま状態付き計算だし、push は Int を取って状態付き計算を返す関数である。
    これで、さっきの 3 を push してから 2 回 pop する例を、以下のように書ける。
-}

stackManip' :: State Stack Int
stackManip' = do
    push 3
    _ <- pop
    pop

{-
    みごと、1 つの push と 2 つの pop を糊付けして、1 つの状態付き計算が作れた。
    こいつの newtype ラッパーを剥がせば、初期状態を与えると動きだす関数が出てくる。
-}

manipedStack' :: (Int, Stack)
manipedStack' = runState stackManip' [5,8,2,1]
    -- (5,[8,2,1])

{-
    もうちょっとややこしいことはできるだろうか？
    例えば、スタックから 1 つの数を取り出し、それが 5 だったらそっと元に戻す、でも 5 でなかったら代わりに 3 と 8 を積む、とかどうだろう？
    以下がそのコードである:
-}

stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

{-
    これは直感的に書けた。では初期スタックを与えて走らせてみよう。
        > runState stackStuff [9,0,2,1,0]
        ((),[8,3,0,2,1,0])

    do 式はモナド値を作ること、そして State モナドに関しては do 式もまた状態付きの関数であることを思い出そう。
    stackManip' と stackStuff はどちらも普通の状態付き計算だから、この 2 つをさらに糊付けして、もっと大きな状態付き計算が作れる。
-}

moreStack :: State Stack ()
moreStack = do
    a <- stackManip'
    if a == 100
        then stackStuff
        else return ()

{-
    現在のスタックに stackManip' を使った結果が 100 なら、stackStuff を実行する。
    それ以外の場合は何もしない。return () は、状態に変更を加えず何もしないモナドである。

    > runState moreStack [1,3,4]
    ((),[3,4])

    > runState moreStack [100,3,4]
    ((),[8,3,4])
-}

--- ¶　状態の取得（get）と設定（put）
{-
    Control.Monad.State モジュールは、2 つの便利な関数 get と put を備えた、MonadState という型クラスを提供している。
    State モナドに対する get の実装は以下のとおり:

        get = state $ \s -> (s, s)

    現在の状態を取ってきて、それを結果として提示しているだけである。
    put 関数は、状態型の引数を取り、「その状態を、現在の状態に上書きする状態付き関数」を返す。

        put newState = state $ \s -> ((), newState)

    というわけで、この put と get があれば、現在のスタックを見たり、現在のスタックを丸ごとすり替えたりできる。
    以下のように:
-}

stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
        then put [8,3,1]
        else put [9,2,1]

{-
    > runState stackyStack [1,2,3]
    ((),[8,3,1])
    > runState stackyStack [1,2,4]
    ((),[9,2,1])
-}

{-
    また、get と put を使って pop や push を実装することもできる。
    まず、pop は以下。
-}

pop'' :: State Stack Int
pop'' = do
    xl <- get
    case xl of
        (x:xs) -> do
            put xs
            return x
        _ -> error "Pattern matching error."
{- ※ ここは、テキストどおりにやろうとするとエラーが出る。
```
Haskell WikiのMigration Guideで紹介されていますが、GHCの8.6.x以降ではdo記法内でエラーが発生しうるパターンマッチを使用する場合は、
case記法を使ってモナドからの値取得後にパターンマッチングを行うよう指示されています。
```
とのこと（https://qiita.com/kurunin52/items/c555f30b4ea7362d3cf1#do%E8%A8%98%E6%B3%95%E5%86%85%E3%81%A7%E3%81%AE%E3%83%91%E3%82%BF%E3%83%BC%E3%83%B3%E3%83%9E%E3%83%83%E3%83%81%E3%83%B3%E3%82%B0）。
-}

{-
    get を使ってスタック全体を取り出し、それから先頭要素を取り除いた残りを put を使って新しい状態にしている。
    それから return を使って x を結果として提示している。
    > runState pop'' [3,2,1]
    (3,[2,1])

    で、以下は get と put を使った push の実装である。
-}

push'' :: Int -> State Stack ()
push'' x = do
    xs <- get
    put (x:xs)

    -- > runState (push'' 10) [1,2,3]
    --  ((),[10,1,2,3])

{-
    get を使って現在のスタックの状態を取得し、それに x を積んだものを put を使って新しい状態として設定するだけ。
    ここで、もし >>= の型が State 値専用だったらどうなるか考えてみよう。

        (>>=) :: State s a -> (a -> State s b) -> State s b

    状態の型 s は常に同じで、計算結果の型は a から b に変えられる。
    結果の型が違う状態付き計算どうしは >>= で糊付けできるが、状態の型は同じでなければならない。
    なぜだかわかるだろうか？　例えば Maybe モナドの >>= はこんな型だった。

        (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    
    モナド自身の型、つまり Maybe が >>= の前後で変わらないのは当然である。
    型の違う 2 つのモナド（Maybe モナドとリストモナドなど）を >>= するのは無意味だ。さて、State モナドに戻ると、モナドのインスタンスになっている型は State s だった。
    だから s が違うということは、>>= を違う型のモナドの間で使おうとしていることになるのである。
-}

--- ¶　乱数と State モナド
{-
    この節の冒頭で、乱数を生成する処理ってきれいに書けないよねー、という話をした。
    乱数関数はジェネレータを引数に取り、乱数と一緒に新しいジェネレータを返すようにできていて、次の乱数を作るときは必ずこの新しいジェネレータを使い、
    古い方を使わないよう気を付ける必要があるのだった。
    State モナドがあれば、こういう処理はぐっと楽になる。

    System.Random モジュールの random 関数の型は次のとおり:
        random :: (RandomGen g, Random a) => g -> (a, g)
    random は、乱数ジェネレータを引数に取り、乱数と新しいジェネレータを返す関数である、と言っている。
    どう見ても状態付き計算である。
    state 関数を使って State の newtype に包めば、状態の扱いをモナドに任せられる。
-}

-- import System.Random
-- import Control.Monad.State
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

{-
    これでコインを 3 枚投げる操作は以下のように書けるようになった。
-}

threeCoins' :: State StdGen (Bool, Bool, Bool)
threeCoins' = do
    a <- randomSt -- randomSt は状態付き計算（単一の確定的な値ではなく、random 関数と同様、乱数ジェネレータを引数に取って乱数値と新しい状態（乱数ジェネレータ）を返すもの）。
    b <- randomSt
    c <- randomSt
    return (a, b, c)

    -- う〜〜ん。do 式の各行の挙動が理解不足。。
    -- do 式の各行は次の行に何か渡すんだっけ？
    {-
    ん、、でも
    stackManip' :: State Stack Int
    stackManip' = do
        push 3
        _ <- pop
        pop
    と書けたことから、do 式では状態は次の行に引き継がれるっていうことだよね。
    push 3 した後のスタックから pop して、その pop されたスタックからもう一度 pop するので。。
    今回もそれと同じで、randomSt で新しく生成された乱数ジェネレータを次の randomSt が受け取って、また新しく生成されたジェネレータをさらに次の randomSt が受け取って、という流れになるんだろう。。
    -}

{-
    threeCoins 関数は状態付き計算になった。threeCoins' は、まず受け取ったジェネレータを最初の randomSt に渡す。
    すると randomSt が乱数と新しいジェネレータを返す。
    この新しいジェネレータが次に渡って、...... と続く。
    最後に return (a, b, c) を使って、最も新しいジェネレータを変えることなく、(a, b, c) を結果として提示する。
    では使ってみよう。

    > runState threeCoins' (mkStdGen 30)
    ((True,False,False),935833018 2103410263)

    これで、状態が必要な計算をするときの手間がぐっと減った！
-}

{-
> 参考
State を使わなかったときの threeCoins の実装（再掲）

    -- import System.Random
    threeCoins :: StdGen -> (Bool, Bool, Bool)
    threeCoins gen =
        let (firstCoin, newGen) = random gen
            (secondCoin, newGen') = random newGen
            (thirdCoin, _) = random newGen'
        in (firstCoin, secondCoin, thirdCoin)
-}

---　🧐自己検証。。do 記法の挙動。ふつうの関数 vs Stateモナドを使ったとき
myFunc :: String -> (Int, String)
myFunc txt = (length txt, txt ++ "!")

doTest :: String -> ((Int, String),(Int, String),(Int, String))
doTest = do
    a <- myFunc
    b <- myFunc
    c <- myFunc
    return (a,b,c)

hogeResult :: ((Int, String),(Int, String),(Int, String))
hogeResult = doTest "hoge" -- ((4,"hoge!"),(4,"hoge!"),(4,"hoge!"))

---

myFuncSt :: State String Int
myFuncSt = state $ \txt -> (length txt, txt ++ "!")

doTestSt :: State String (Int, Int, Int)
doTestSt = do
    a <- myFuncSt
    b <- myFuncSt
    c <- myFuncSt
    return (a, b, c)

hogeResultSt :: ((Int,Int,Int), String)
hogeResultSt = runState doTestSt "hoge" -- ((4,5,6),"hoge!!!")

-------------------------------
--　Error を壁に
-------------------------------

{-
    Maybe モナドは「失敗するかもしれない計算」という文脈を値に与えるものだった。
    Maybe 値は Just something か Nothing のどちらかになれる。
    これは確かに便利なのだが、Nothing を受け取ったときにわかるのは、どこかで何かが失敗したというだけである。
    どんな失敗があったのか、という情報を詰め込む余地はない。

    Either e a 型も失敗の文脈を与えるモナドである。
    しかも、失敗に値を付加できるので、何が失敗したかを説明したり、そのほか説明にまつわる有用な情報を提供できる。
    Either e a は、Right 値であれば正解や計算の成功を、Left 値であれば失敗を表す。
    以下が例である。

    > :t Right 4
    Right 4 :: Num b => Either a b

    > :t Left "out of cheese error"
    Left "out of cheese error" :: Either [Char] b

    Either e はおおむね Maybe の強化版だから、モナドになっているのはごく自然なことである。
    Maybe と同じく、失敗する可能性という文脈が付加された値とみなせるが、今度はエラーがあった場合にも値をつけられるのである。

    Either の Monad インスタンスは Maybe のものによく似ており、Control.Monad.Error モジュール（🚨）で宣言されている。

    instance (Error e) => Monad (Either e) where　（🚨現在は、この Error e という制約はついていない）
        return x = Right x
        Right x >>= f = f x
        Left err >>= f = Left err
        fail msg = Left (strMsg msg)

    return は、いつもどおり、引数をデフォルトの最小限の文脈に入れる関数である。
    return は引数を Right コンストラクタに入れる。Right は、計算に成功して値があることを表すからである。
    これは Maybe モナドの return とよく似ている。

    >>= は 2 つの場合に分かれる。左辺が Left である場合と Right である場合だ。
    左辺が Right だった場合はその中の値に f を適用する（f は普通の値を取ってモナド値を返す関数というイメージなので、f x の結果もモナド値（Either）になる）。
    これは Maybe モナドの Just の処理とそっくり。
    一方、左辺がすでにエラーだった場合は、Left 値であることと、失敗を表す中身とがそのまま保たれる。

    Either e の Monad インスタンスには、もう 1 つ必要条件がある。
    Left に入るほうの値の型（型引数 e にあたる型）は、Error 型クラスのインスタンスでなければならない。
    Error 型クラスは、エラーメッセージのように振る舞える型のクラスである。
    Error 型クラスにはエラーを文字列として受け取って、その型に変換する strMsg 関数が定義されている。（🚨）
    Error 型クラスの自明なインスタンスは、むろん String である。　String の場合、strMsg 関数は受け取った文字列をそのまま返すだけである。
        strMsg :: Error a => String -> a

        > strMsg "boom!" :: String
        "boom!"

    もっとも、Either を使うにあたってエラーは普通 String で表すから、Error 型クラスについてそれほど心配はいらない。
    do 記法でのパターンマッチが失敗したときは、その失敗を表すのに Left 値を使ってくれる。
    Either を使ってみた例が以下:
-}

eith :: Either String Int
eith = Left "boom" >>= \x -> return (x+1) -- Left "boom"

eith' :: Either String a
eith' = Left "boom" >>= \_ -> Left "no way!" -- Left "boom"

eith'' :: Either String Int
eith'' = Right (10 :: Int) >>= \_ -> Left "no way!" -- Left "no way!"

{-
    >>= を使って Left 値を関数に食わせると、関数は無視されて Left 値がそのまま返る。
    Right 値を関数に食わせた場合は、関数が Right の中身に適用されるが、この最後の例の場合は中身がなんであれ関数が Left 値を返している。

    では、Right 値を成功する関数に渡したら？
-}

eith''' :: Either String Int
eith''' =  Right (3 :: Int) >>= \x -> return (x + 100)
    -- Right 103

{-
    🚨🚨🚨
    テキストと現時点では仕様が異なり、Control.Monad.Error モジュールは deprecated であるようだ。
    strMsg 関数も Control.Monad.Error に属するものであり、現在は使われていないようである。
    Error 型クラス自体使わない（のかな？）。
    Either のモナドインスタンスも、「Control.Monad.Instances モジュールで、Error 型クラスの文脈をつけない形で宣言する」ように変更されている。
-}
