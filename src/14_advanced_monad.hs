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

