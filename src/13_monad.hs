{-# OPTIONS -Wall -Werror #-}
import Control.Monad

---_/_/_/_/_/_/_/_/_/_/_/---
-- 　  モナドがいっぱい　　　    --
---_/_/_/_/_/_/_/_/_/_/_/---

{-
    第 7 章で初めてファンクターの話をしたとき、ファンクターは関数で写せる値をあらわす　便利な概念であることを見た。
    第 11 章ではファンクターを一歩拡張してアプリカティブファンクターを導入し、ある種のデータ型は、文脈を持った値だと解釈できるようになった。
    アプリカティブファンクターを使えば、そのような文脈を保ったまま、通常の関数をそれらの値に適用できるのだった。

    この章では **モナド** を紹介する。
    モナドは強化されたアプリカティブファンクターである。
-}

-------------------------------
--　アプリカティブファンクターを強化する
-------------------------------

{-
    ファンクターを勉強し始めたとき、Functor 型クラスに属するさまざまな型は、すべて関数で写せることを見た。
    ファンクターを導入した動機は、「a -> b 型の関数と、f a というデータがあるとして、どうすれば等の関数を f a から f b への関数に変換できるだろう？」というものだった。
    Maybe a、リスト [a]、IO a などに対し、関数で写す方法を見てきた。
    a -> b 型の関数はなんと「r -> a 型の関数」を写すこともでき、その結果は「r -> b 型の関数」になることも見た。
    あるデータ型を関数で写したときの挙動を指定するには、fmap の型を見てから、
        fmap :: (Functor f) => (a -> b) -> f a -> f b
    Functor インスタンスを書き、fmap がそのデータ型を適切に処理できるようにすればよいのだった。

    それから、いくつかの疑問とともに、ファンクターを改良できる可能性が浮かび上がってきた。
    関数 a -> b が、はじめからファンクターに包まれていたらどうする？
    例えば Just (*3) があったとして、それを Just 5 に適用するには？
    その Just 5 が急に Nothing に変わったら？
    Maybe の代わりにリスト [(*2), (+4)] があったとして、それを [1,2,3] に適用sるうには？
    そもそも、そんなの動くの？　これらを解決するために、Applicative 型クラスを導入した。

        (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b

    Applicative 型クラスでは、通常の値をデータ型の中に入れる操作も可能になったのだった。
    例えば、1 を Just 1 に変えたり、[1] に変えたり、はたまた「何も副作用を起こさず 1 を返す I/O アクション」に変えたりできるのだった。
    この変換をしてくれる関数の名前は pure というんだった。
        *Main> pure 1 :: Maybe Int
        Just 1
        *Main> pure 1 :: [Int]
        [1]
        *Main> pure 1 :: IO Int
        1

    アプリカティブ値は、_変な_ 値、専門用語で言うと「文脈の付加された値」だとみなせる。
    例えば、文字 'a' はただの文字だが、Just 'a' は何らかの文脈がついている。
    Char 型の代わりに Maybe Char 型がきたとしたら、この値は文字かもしれないし文字がないことを表すのかもしれないということである。
    Applicative 型クラスは、これら文脈のついた値に、文脈を保ったまま普通の関数を適用させてくれる。例を見てみよう。

        *Main> (*) <$> Just 2 <*> Just 8
        Just 16
        *Main> (++) <$> Just "exdeath" <*> Nothing
        Nothing
        *Main> (-) <$> [3,4] <*> [1,2,3]
        [2,1,0,3,2,1]

    このようにアプリカティブ値として扱い始めると、Maybe a は失敗するかもしれない計算、[a] は複数の答えがありうる計算（非決定性計算）、IO a は副作用を伴う計算、などの意味を帯びてくるのだった。

    さて、モナドはある願いをかなえるための、アプリカティブ値の自然な拡張である。
    その願いとは「"普通の値 a (例: 'a') を取って文脈付きの値（例: Just 'a'）を返す関数" に、文脈付きの値 m a を渡したい」というもの。🧐❓
    言い換えると、a -> m b 型の関数を m a 型の値に適用したいということ。わかりやすく言うと、この関数が欲しいということである。

        (>>=) :: Monad m => m a -> (a -> m b) -> m b

    さて、「変な値 (例: Just 'a')」と、「普通の値を引数に取るけど変な値を返す関数 (例: 'a' -> Just 'a')」があったとき、どうやってその値を関数に食わせればよいだろう？
    これがモナドの一番の関心事である。
    これからは f a の代わりに m a と書くことにする。m は Monad の頭文字である。
    呼び方は変わるものの、モナドは >>= をサポートするアプリカティブファンクターにすぎない。関数 >>= はバインド（bind）と呼ばれる。
    普通の値 a と普通の関数 a -> b だったら、値を関数に食わせるのは造作もないことで、単に関数を値に適用すればよい。
    ところが、特定の文脈がつきまとう値を扱うとなると、その変な値を関数に食わせるとどうなるのか、文脈を保つにはどうするのか、ちゃんと考えないといけない。
    でも、やってみれば簡単だということがわかる！
-}

-------------------------------
--　Maybe から始めるモナド
-------------------------------

{-
    モナドが何者なのかぼんやりわかってきたところで、具体例を見ていこう。
    実は Maybe はモナドだったのである。

    Maybe a 型の値は a 型の値を表しているが、失敗する可能性という文脈付き。
    Just "dharma" という値は、文字列 "dharma" がそこに実在することを意味する。
    Nothing という値は、無を、あるいは文字列が何らかの計算の結果だとしたら、その計算が失敗したことを表している。

    ファンクターとして見た Maybe に対して関数を fmap すると、その Maybe が Just 値だった場合には、
    与えた関数が Just の中身に適用されるのだった。
    Nothing の場合は、Nothing のままだった。なぜなら関数を適用する相手がいないからである。

    *Main> fmap (++ "!") (Just "wisdom")
    Just "wisdom!"
    *Main> fmap (++ "!") Nothing
    Nothing

    アプリカティブファンクターとしての Maybe の機能も似たようなものである。
    ただし、アプリカティブファンクターになると、値だけでなく、値に適用する関数のほうにも文脈がつく。
    Maybe の Applicative インスタンスは、<*> を使って Maybe の中の関数を Maybe の中の値に適用しようとすると、
    関数と値が Just ならば結果が Just になる、そうでなければ結果は Nothing になる、というものだった。
    関数か値のどちらかが欠けていれば、無から適用結果をでっち上げるわけにはいかないから、失敗を伝播させる必要があるのだ。

        *Main> Just (+3) <*> Just 3
        Just 6

        *Main> Just reverse  <*> Nothing
        Nothing

    アプリカティブ・スタイルを使って複数の関数を Maybe 値に適用するときも、同じやり方でうまくいく。
    すべての引数が Just でなければ、結果は Nothing である！

        *Main> max <$> Just 3 <*> Just 6
        Just 6
        *Main> max <$> Just 3 <*> Nothing
        Nothing

    それではいよいよ、Maybe にとっての >>= をどう定義すればいいか考えていこう。
    >>= は、「モナド値（変な値、文脈付きの値）」と「普通の値を取る関数」を引数に取り、なんとかしてその関数をモナド値に適用してモナド値を得る。
    関数のほうは普通の値しか取れないのに、どうやってそんな芸当ができるのだろう？
    その答えを出すには、モナド値の文脈に立ち入る必要がある。

    この場合、>>= は Maybe a 型の値と a -> Maybe b 型の関数を取り、この関数をどうにかして Maybe a に適用するわけである。
    これをどうやって実現しているか探るには、Maybe がアプリカティブファンクターであるという知識が役立つ。
    さて、関数 \x -> Just (x+1) を考えよう。これは（普通の）数を取り、それに 1 を足して結果を Just に包む（ことで文脈を付ける）。
    例えば、この関数に 1 を食わせると Just 2 に評価され、100 を食わせると Just 101 に評価される。
    では、この関数に Maybe 値を食わせるにはどうしたらいいだろう？
    Maybe のアプリカティブファンクターとしての振る舞いから類推すると、この問いに答えるのは極めて簡単。
    Just 値がきたときは Just の中身を取り出し、それを関数に食わせればよい。
    Nothing 値がきたときは、関数はあるものの、それに適用すべき値がナッシングというわけだから、結果も Nothing とせざるを得ない。

    ひとまず >>= と呼ぶのはやめて applyMaybe という名前にする。
    これは「Maybe a 型の値」と「（普通の値を引数に取り、）Maybe b を返す関数」を引数に取り、どうにかしてその関数を Maybe a に適用してくれる関数である。
-}

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x -- f は `a -> Maybe b` 型であるため、ここは `Just f x` としないように。

-- 使ってみよう。
-- Maybe 値が左、関数が右に来るように、中置演算子表記で使ってみよう。

ans1 :: Maybe Int
ans1 = Just 3 `applyMaybe` \x -> Just (x+1) -- Just 4

ans2 :: Maybe String
ans2 = Just "smile" `applyMaybe` \x -> Just (x ++ " :) ") -- Just "smile :) "

ans3 :: Maybe Int
ans3 = Nothing `applyMaybe` \x -> Just (x+1) -- Nothing

{-
    この例では、Just 値と関数を引数に applyMaybe を呼び出したときは、単に Just の中の値に関数が適用されている。
    Nothing 値と関数を引数に呼び出すと、全体の結果が Nothing になっている。
    では、関数のほうが Nothing を返す場合はどうだろう？
-}

ans4 :: Maybe Int
ans4 = Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing -- Just 3

ans5 :: Maybe Int
ans5 = Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing -- Nothing

{-
    確かに期待通りの結果である。applyMaybe の左辺のモナド値が Nothing である場合には、全体の結果は Nothing になる。
    それから右辺の関数が Nothing を返した場合も、結果は Nothing である。
    これは Maybe をアプリカティブとして使ったときの挙動とよく似ている。
    式のどこかに Nothing があったら、答えも Nothing になるという挙動である。

    どうやら「変な値」に「普通の値を取って変な値を返す関数」を適用する方法が見えてきた。
    この例では「Maybe は失敗したかもしれない計算を表す」というイメージを心に描いて設計することで、これが実現できた。
    「で、これって何が便利なの？」という疑問が聞こえてきそう。
    実際、アプリカティブファンクターのほうがモナドよりも強力に思えるかもしれない。
    だってアプリカティブファンクターは、ごく普通の関数を文脈付きの値に適用できるようにしてくれるのだから。
    ところがどっこい、モナドも同じことができるのである。
    さらにモナドにはできてアプリカティブファンクターにはできないこともあるのである。

    Maybe モナドは少し置いておいて、まずは、モナドが属する型クラスを見ていこう。
-}

-------------------------------
--　Monad 型クラス
-------------------------------

{-
    ファンクターには Functor 型クラスがあり、アプリカティブファンクターには Applicative 型クラスがあるように、モナドにも型クラス Monad がある。

    class Applicative m => Monad (m :: * -> *) where
        return :: a -> m a

        (>>=) :: m a -> (a -> m b) -> m b
        (>>) :: m a -> m b -> m b
    
        fail :: String -> m a
        fail s = errorWithoutStackTrace s

    return 関数は Applicative 型クラスの　pure と同じもの。
    だから、名前は違っていてもすでにおなじみである。

    return の型は a -> m a である。
    return は、値を取って、その値を再現できるような最小のデフォルト文脈に入れる。
    return は、第 8 章で I/O を扱ったときすでに登場していた。そのときは、値を返すだけで何も I/O をしない、なんちゃって I/O アクションを作るために return を使ったのだった。
    Maybe の場合は、return は値を Just に入れて返す。

        *Main> return 20 >>= \x -> Just (x+1)
        Just 21

    次の関数は >>=、又の名をバインド。これは関数適用に似ているが、普通の値を取って通常の関数を適用するのではなく、
    モナド値（つまり、文脈付きの値）を取って、それに「通常の値を取るがモナド値を返す関数」を適用する（上の節では `applyMaybe` として実装した関数が >>= である）。

    それから、>> がある。これにはデフォルト実装があるのであまり詳しくは解説しない。
    Monad インスタンスを実装するときも、このデフォルト実装を上書きすることは滅多にない。

    Monad 型クラスの最後の関数は fail である。
    ユーザーがコードの中から fail を呼び出すことは決してなく、もっぱら Haskell システムが呼び出す。
    fail はモナド用の特別な構文において、パターンマッチに失敗してもプログラムを以上終了させず、失敗をモナドの文脈の中で扱えるようにするものである。
    これについては後で見るので、今はさほど気にしなくて良い。

    これで Monad 型クラスがどんなものなのかわかった。では、Maybe の Monad インスタンスの実装を見ていこう。

    instance Monad Maybe where
        return x = Just x
        Nothing >>= f = Nothing
        Just x >>= f = f x
        fail _ = Nothing

    return は pure と同じだから、目を瞑っていても書ける。
    Applicative 型クラスのときと同様、Just に包むだけ。
    関数 >>= は、さっきの applyMaybe と同じ。左辺の Maybe a を右辺の関数に食わせるとき、失敗するかもしれない計算という文脈の意味を考えれば、
    左辺に Nothing がきていれば Nothing を返す、Just がきていれば中身を取り出して f を適用する、という設計になる。

    モナド化した Maybe で遊んでみよう。
-}

foo :: Maybe String
foo = return "What" -- foo は Just "What" になる

foo' :: Maybe Int
foo' = Just 9 >>= \x -> return (x*10) -- Just 90

foo'' :: Maybe Int
foo'' = Nothing >>= \x -> return (x*10) -- Nothing

