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
