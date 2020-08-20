{-# OPTIONS -Wall -Werror #-}

---_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/---
--   　　　　　　入出力　　　　　　　    --
---_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/---

{-
    この章では、キーボードからの入力を受け取ったり、画面に何かを表示したりする方法を学ぶ。
    また、その前に、入出力（I/O）の基本をカバーしておこう。
        - I/O アクションとは何か？
        - I/O アクションがどうやって入出力を可能にするのか？
        - 実際に I/O アクションが行われるのはいつか？
    I/O の扱いは、Haskell の関数にできることの制約にかかわる問題なので、その制約をどうやって回避するのかを最初に見ていく。
-}

-------------------------------
--　不純なものと純粋なものを分離する
-------------------------------

{-
    ここまで見てきたとおり、Haskell プログラミングでは、人間からコンピュータに与えるのは一連の実行ステップではなく、あるものが何であるかの定義だった。
    加えて、関数は**副作用**を持つことを許されない。
    関数が同じ引数で 2 回呼び出されたら、必ず同じ結果が返される。

    このことは、はじめのうちはとても大きな制限のように思えるかもしれないが、実際には素晴らしく優れた性質なのである。
    命令型言語では、一見すると数を処理するだけに見える簡単な関数が、
    処理の片手間にあなたの家に火をつけたり犬を誘拐したりしないことを保証できない。
    例えば、二分探索木を前の章で作ったが、木そのものを変更して要素を挿入したわけではない。
    その代わりに、木に新しい要素を挿入した新しい木を返したんだった。

    関数が状態を変更でいない、例えばグローバル変数を更新したりできないのは好ましいことである。
    なぜなら、ぷろぐらむについての推論が用意になるからだ。
    しかし 1 つ問題がある。
    もし関数がこの世界の状態を何も変えられないとしたら、計算したものをユーザーにどうやって伝えるのだろう？
    そのためには出力デバイス（モニタ）の状態を変更しなければならない。

    実は、Haskell は、副作用を持つ関数を扱うための素晴らしく賢いシステムを持っているのである。
    そのシステムが、プログラムの純粋（pure）な部分と、キーボードや画面とやりとりをするようなすべての汚い仕事をする不純（impure）な部分とをきっちり分離してくれる。
    この 2 つの部分が隔てられているので、外の世界とやりとりしつつも、依然としてプログラムの純粋な部分を推論したり、
    純粋だからこそ得られる遅延評価、堅牢性、関数合成などを利用したりできる。
-}


-------------------------------
--　Hello, World!
-------------------------------
{-
    これまでは、作った関数は GHCi にロードして試していた。
    標準ライブラリの関数たちも、同じく GHCi から探検してきた。
    今、ついに初めて本物の Haskell プログラムを書くときがやってきた。

    helloworld.hs というファイルを作成し、以下のコードを書こう。
        main = putStrLn "Hello, world"

    main を定義しただけである。その中で putStrLn という関数を、"Hello, world" という引数で呼び出している。

    そしてこれから今までやらなかったことをする。
    プログラムをコンパイルして、実行ファイルを生成するのである。
    そうしたら、（helloworld.hs のあるディレクトリに移動し、/* しなくてもよい */）次のコマンドを打ち込む。
        $ stack ghc src/helloworld（.hs）
        // $ ghc --make helloworld テキストではこうなっているが、stack の場合、↑ のように、 --make は不要

    これは、GHC コンパイラを起動してプログラムをコンパイルするコマンドである。

    $ stack ghc src/helloworld（.hs）                                                            
    [1 of 1] Compiling Main             ( src/helloworld.hs, src/helloworld.o )
    Linking src/helloworld ...

    上記のようにコンパイルが成功したら、次のように端末にタイプしてプログラムを実行できる。
        $ ./helloworld # ディレクトリは適宜補足すること。 `./src/helloworld` のように

    そうすると次のようなメッセージが表示される
        > Hello, world

    先ほどのコードを詳しく見ていこう。まず関数 putStrLn の型を見てみる。

        putStrLn :: String -> IO ()

        putStrLn "hello, world" :: IO ()

    putStrLn の型は次のように読める。「putStrLn は文字列を引数に取り、()（空のタプル。unit 型ともいう）を結果とする **I/O アクション**を返す」
    （putStrLn "hello, world" は評価されると "hello, world" と表示するのではなく、「"hello world" と表示しろ」という命令書（I/O アクション）を返す）
    I/O アクションとは、実行されると副作用（入力を読んだり画面やファイルに何かを書き出したり）を含む動作をして結果を返すような何かである。
    このことを、「I/O アクションが結果を**生成する**」という。
    文字列を端末に表示するアクションには実際には意味のある返り値がないので、ダミーの値として () を使う。
    （→ 繰り返しになるが、I/O アクションは命令書なので、例えば I/O アクションの返り値自体が "hello world"（String）だとかいうわけではないということだろう）
        > 🔭空のタプルの値は () であり、その型もまた () である。

    では I/O アクションはいつ実行されるのだろうか？
    ここで main が関係してくる。I/O アクションは、我々がそれに main という名前をつけてプログラムを起動すると実行されるのである。
-}

-------------------------------
--　I/O アクションどうしをまとめる
-------------------------------

{-
    プログラム全体を単一の I/O アクションにしないといけないのは、ちょっと制約に思える。
    そこで複数の I/O アクションを糊付けして 1 つにするのに do 構文が使える。
    次の例を見よ。
        main = do
            putStrLn "Hello, what's your name?"
            name <- getLine
            putStrLn ("Hey, " ++ name ++ ", you rock!")
    新しい構文である。しかも命令型のプログラムにかなり似ている。
    コンパイルして実行すれば、このコードから想像したとおりに動くだろう。

    do と書いてから、あたかも命令型のプログラムを書くように実行ステップを書き並べている。
    それら実行ステップのそれぞれが I/O アクションである。
    I/O アクションを do 構文を使ってまとめると、糊付けされた 1 つの I/O アクションにできる。
    こうして得られるアクションの型は IO () になる。
    do の中の最後の I/O アクションの型が IO () だからである。
    そのため、main の型シグネチャは「main :: IO 何か」になる。「何か」には具体型が入る。
    main の型宣言は明示しないこともある（※ドキュメントのため、トップレベルの定義では型宣言を書くのが一般的。main にも型宣言を書くことのほうが多い）。

    3 行目の name <- getLine はなんだろうか？「入力から 1 行読み込み、それを name という名前の変数に格納する」と読めそう。
    本当にそうだろうか？　では getLine の型を調べてみよう。

        getLine :: IO String

    getLine は　String を生成する I/O アクションだとわかる。
    ユーザーが端末に何か入力するのを待って、それからその何かを文字列として返すということなので、理にかなっている。
    では、name <- getLine とすると何が起こるのだろう？
    このコードは次のように読める。「I/O アクション getLine を実行して、それからその結果の値を name に束縛せよ」
    getLine は IO String という型を持つので、name の型は String になる。

    I/O アクションは、小さな足がついた箱だと考えることができる。
    実世界に出て行って、そこで何かを行い（壁に落書きをしたりとか）、何か値を持って返ってくる小さな箱である。
    箱がデータを取ってきたとき、それを開けて中のデータを手に入れる唯一の方法が <- である。
    そして、I/O アクションからデータを取り出そうとしても、それができるのは別の I/O アクションの中だけである。
    以上が純粋なものと不純なものをきちんと分類する Haskell の方法である。
    I/O アクション getLine は純粋ではない。2 回実行したときに同じ結果を返す保証はないからである。

    name <- getLine を実行すると、name は通常の文字列になる。
    なぜなら、name は箱の中にあるものを表しているからである。
    例えば、名前（普通の文字列）を引数として受け取り、その名前に基づいた占いの結果を返すという、実に複雑な関数が書ける。

    main = do
        putStrLn "Hello, what's your name?"
        name <- getLine
        putStrLn $ "Zis is your future: " ++ tellFortune name

    tellFortune 関数（というか、name を渡されるあらゆる関数）は、I/O について何も知る必要がない。
    これは単なる String -> String 型の関数である。
    I/O アクションのどのへんが普通の値と異なるのかみるために、次のコードを考えてみよう。
    これは正しいのだろうか？
        nameTag = "Hello, my name is " ++ getLine
    ++ は両辺に同じ型のリストを要求sるうので、これは動作しない。
    左の引数は String の型を持ち、右の引数 getLine は IO String の型を持つ。
    文字列と I/O アクションは連結できない。
    最初に I/O アクションから String 型の値を取り出す必要がある。
    それにはどこか別の I/O アクションの中で name <- getLine のようにするしかない。

    純粋でないデータを使いたいなら、純粋でない環境の中で行わなければならない。
    不純による汚染はゾンビのように拡散していくので、コード中の I/O の部分を可能な限り小さく抑えるようにするべきなのである。

    実行された I/O アクションはどれも結果を生成する。
    そのため、先ほどの例は次のように書くこともできる。

        main do
            foo <- putStrLn "Hello, what's your name?"
            name <- getLine
            putStrLn ("Hey " ++ name ++ ", you rock!")

    しかし、foo は () の値しか持たないので、このように書いても意味がない。
    最後の putStrLn は束縛を行っていないことに注目せよ。
    do ブロックの最後のアクションでは、最初の 2 つのアクションとは違って名前を束縛できないのである。
    正確な話は 13 章でモナドの世界に降り立ってから。

    最後の行を除いて、do ブロックのすべての行に束縛を書いてもかまわない。
    putStrLn "BLAH" を _ <- putStrLn "BLAH" と書いてもよい。でも意味がないので、putStrLn のような意味のある結果を生成しない I/O アクションに対しては束縛を省略する。

    以下のようなことをすると何が起こるだろう？
        myLine = getLine

    入力から行を読んで、それを myLine に束縛…しない！これは I/O アクション getLine に対して別の名前 myLine を与えているだけ。
    I/O アクションから値を取り出すには、他の I/O アクションの中で <- を使って名前に束縛するしかない。

    I/O アクションが実行されるのは、main という名前を与えられたとき、あるいは do ブロックで作った別の大きな I/O アクションの中にあるとき。
    いくつかの I/O アクションを糊付けするのにも do ブロックが使えて、その I/O アクションをまた別の do ブロックで使うことができて、
    その I/O アクションもまた別の do ブロックで使うことができる。
    それらは、最終的に main の中に含まれていれば実行されることになる。

    もう 1 つ I/O アクションが実行される場合がある。
    GHCi に I/O アクションを入力して Enter を押したときである。

        *Main> putStrLn "Heey"
        Heey

    単純に数や関数呼び出しを GHCi に入力して Enter を押した場合も、GHCi はその結果の値に show を適用して、
    その結果を putStrLn を使って端末に表示する。
-}

--- ¶ I/O アクションの中で let を使う
{-
    I/O アクションを do 構文を使って糊付けしているときに、let 構文を使って純粋な値を名前に束縛できる。
    <- が I/O を実行してその結果を名前に束縛するのに対し、let は I/O アクションの中で普通の値に名前を与えたいときに使う。
    これはリスト内包表記における let 構文に似ている。

    <- と let による束縛を両方とも使った I/O アクションを見てみることにしよう。

        import Data.Char

        main = do
            putStrLn "What's your first name?"
            firstName <- getLine
            putStrLn "What's your last name?"
            lastName <- getLine
            let bigFirstName = map toUpper firstName
                bigLastName = map toUpper lastName
            putStrLn $ "hey " ++ bigFirstName ++ " "
                            ++ bigLastName
                            ++ ", how are you?"

    do ブロックの中での I/O アクションの並べ方がわかっただろうか？
    let と I/O アクション、それから let の中の名前はどうだろう？
    Haskell ではインデントが重要なので、これはいい実例である。

    いつ <- を使って、いつ let 束縛を使えばいいか改めて説明しよう。
    <- は、I/O アクションを実行して、その結果に名前を束縛するのに使う。
    ところが map toUpper firstName は I/O アクションではない。純粋な Haskell の式だ。
    というわけで、<- は I/O アクションの結果に名前を束縛したいときに使い、let 束縛は純粋な式に名前を束縛するのに使う。
    let firstName = getLine のように実行しても、I/O アクション getLine を別の名前で呼んだことにしかならない。
    それを実行して結果を束縛するには <- を使う必要があるのだ。
-}

--- ¶　逆順に表示する
{-
    Haskell における入出力の感覚をもっとつかむために、1 行ずつ読み込んでは単語を逆さまにして表示するという動作を繰り返す単純なプログラムを作ってみよう。
    プログラムに空行を入力したら停止するようにする。

        main :: IO ()
        main = do
            line <- getLine
            if null line
                then return ()
                else do
                    putStrLn $ reverseWords line
                    main

        -- 単純に reverse を使わないのは、一行を単語ごとに区切って、単語ごとに逆順にするため
        reverseWords :: String -> String
        reverseWords = unwords . map reverse . words

    このプログラムが何をするのか、とりあえず動かしてみよう。
        clean up on aisle number nine
        naelc pu no elsia rebmun enin
        the goat of error shines a light upon your life
        eht taog fo rorre senihs a thgil nopu ruoy efil
        it was all a dream
        ti saw lla a maerd
        live
        evil

    reverseWords はごく普通の関数。"hey there man" のような文字列を受け取り、
    これに words を適用して ["hey", "there", "man"] のような単語のリストを生成する。
    それからそのリストに対して reverse をマップし、["yeh", "ereht", "nam"] を受け取り、unwords で 1 つの文字列に戻す。
    最終的な結果hは "yeh ereht nam" になる。

    main についてはどうだろうか？　まず getLine を実行して端末から行を読み、それに line という生をつける。
    それから条件式がある。
    Haskell ではすべての式がなんらかの値を持つので、すべての if には対応する else が必要なのだった。
    このコードの if では、条件が真（この場合は空行が入力された）なら 1 つの I/O アクションが実行され、
    真でなければ else 以下にある I/O アクションが実行されることになる。

    else 以下には正確に 1 つの I/O アクションがなければならないので、do ブロックを使って 2 つの I/O アクションを 1 つに糊付けしている。
    この部分を次のように書くこともできる。

        else (do
            putStrLn $ reverseWords line
            main)

    do ブロックを 1 つの I/O アクションとみなしていることがわかりやすくなるが、醜い。
    do ブロックの中では、getLine で入力した行に reverseWords を適用してから端末に表jいしている。
    それに続けて、main を実行する。
    これは再帰的に実行される正しいコードである。なぜなら、main もまた I/O アクションだからである。要するにプログラムの先頭に戻る。

    null line が True なら、then 以下にある return () というコードが実行される。
    他の言語でサブルーチンや関数から戻るのに return を使ったことがあるかもしれないが、Haskell の return は他の言語の return とはまったく異なるものである。

    Haskell（特に I/O アクションの中）での return は、純粋な値から I/O アクションを作り出す。
    I/O アクションを再び箱で例えると、return は値を受け取り、それを箱の中に入れるものだと考えることができる。
    作り出された I/O アクションは実際には何も行わない。単に結果を生成するだけである。
    ゆえに、I/O の文脈では return "haha" は IO String の型を持つ。

    純粋な値も何もしない I/O アクションに変換して、なんのありがたみがあるのだろうか？
    先ほどのプログラムでは、空の行を入力した場合に実行するための何らかの I/O アクションが必要なのだった。
    それが return () と書いて何もしないハリボテの I/O アクションを作る理由である。

    他の言語と異なり、Haskell の return には I/O の do ブロックの実行を終わらせる働きはない。
    例えば、以下のプログラムは最後の行まで実行されてしまう。

        main = do
            return ()
            return "HAHAHA"
            line <- getLine
            return "BLAH BLAH BLAH"
            return 4
            putStrLn line

    繰り返しになるが、どの return も結果を生成する　I/O アクションを作り出し、その結果は名前に束縛されていないので捨てられてしまう。
    （実行してもターミナルに "HAHAHA" や "BLAH BLAH BLAH" あるいは 4 が表示されるようなことはない。表示されるのは line に束縛された、入力行だけ）
    return を <- を使った名前の束縛と組み合わせて使うことができる。

        main = do
            a <- return "hell"
            b <- return "yeah!"
            putStrLn $ a ++ " " ++ b

    見ての通り、return が <- の反対側にある。return は箱の中に値を仕舞い込むものなので、
    <- はその箱を受け取り（そして実行し）、中の値を取り出して名前に束縛する。
    だが、これは冗長なコードである。なぜなら、do ブロックでは　let を使った束縛が使えるからである。

        main = do
            let a = "hell"
                b = "yeah!"
            putStrLn $ a ++ " " ++ b

    do ブロックで I/O を行うときは、たいてい return を使うことになる。
    というのも、何もしない I/O アクションを作る必要があったり、do ブロックの最後のアクションで作り出された結果を
    I/O アクションの結果として返したくない場合があったりするから。
    違う結果を I/O アクションの返り値にしたいときは、return を使って望みの結果を生成する I/O アクションを作り、それを do ブロックの最後に配置する。
-}

-------------------------------
--　いくつかの便利な I/O 関数
-------------------------------

import Control.Monad
import Data.Char

--- 🌸 putStr
{-
    putStr は putStrLn とよく似ている。文字列を引数としてうけとり、その文字列を端末に表示する I/O アクションを返す。
    しかし putStrLn とは異なり、putSTr は文字列を表示したあとに改行を出力しない。

        main = do
            putStr "Hey, "
            putStr "I'm "
            putStrLn "Andy!"

    これをコンパイルして実行すると以下のような結果になる。
        Hey, I'm Andy!
-}

--- 🌸 putChar
{-
    putChar は文字を受け取り、その文字を端末に表示する I/O アクションを返す関数。

        main = do
            putChar 't'
            putChar 'e'
            putChar 'h'

    putStr は putChar を使って再帰的に定義できる。
    putStr の基底部は空の文字列で、この場合は空の文字列を出力する。
    要するに、return () を使って何もしない I/O を返す。
    空でなければ、先頭の文字を putChar で表示して、それから残りを再帰的に表示する。
-}

myPutStr :: String -> IO ()
myPutStr [] = return ()
myPutStr (x:xs) = do
    putChar x
    myPutStr xs

{-
    I/O の中で、純粋なコードと同じように再帰が使えることがわかるだろうか。
    まずは再帰の基底部を定義して、それから残りのケースを考える。
    この場合だと、最初に先頭の文字を出力して、それから残りの文字列を出力する。
-}

--- ¶　print
{-
    print は Show のインスタンスの型（文字列としてどう表現すればよいか知っている型）の値を受け取り、それに show を適用して
    「文字列化」して、それからその文字列を端末に出力する。
    基本的にこれは putStrLn . show と同じものである。
    はじめに引数に対して show を呼び出し、その結果を putStrLn に与える。
    これは値を表示する I/O アクションを返す。
-}

--- GHCi で実行するために printTry とか変な名前にしてるけど、単独のファイルで書いてコンパイルして実行する場合は main = とすること。
printTry :: IO ()
printTry = do
    print True
    print (2 :: Int)
    print "haha"
    print (3.2 :: Double)
    print [3 :: Int, 4 ::Int, 3 :: Int] -- Werror オプション使ってると、こうやって型注釈いちいち書かないとエラーになる

{-
    見てのとおり、print はとても便利な関数。
    I/O アクションが実行されるのは main の中に入っているか GHCi のプロンプトで評価しようとしたときだけ、という話を思い出してほしい。
    我々が GHCi で値をタイプして Enter を押したとき、その値を端末に表示するのに GHCi が実際に使っているのは print なのである。

    文字列を表示したいとき、普通は putStrLn を使う。
    ダブルクオートで囲まれてほしくないからである。
    でも、他の型の値を端末に表示するときには print が一番よく使われる。

        〜参考〜
        *Main> print "haha"
        "haha"
        *Main> putStrLn "haha"
        haha
        *Main> print 2
        2
        *Main> 2
        2
-}

--- 🌸when
{-
    when 関数は Control.Monad モジュールにある関数（import Control.Monad するとアクセスできる）。
    この関数の面白いところは、do ブロックでは制御構文のように見えるのに実際には普通の関数だというところである。

    when は Bool と I/O アクションを受け取り、Bool の値が True の場合には渡された I/O と同じものを返す。
    False だった場合は何もしない return () を返す。
    次のコードは、入力を受け取り、それが SWORDFISH だったときに限ってそのままターミナルに出力するプログラムである。

    見てのとおり、when は条件が満たされたときだけなんらかの I/O アクションを行いたい場合に便利な関数である。
-}

sword :: IO ()
sword = do
    input <- getLine
    when (input == "SWORDFISH") $ do
        putStrLn input

{- when を使わなければ以下のように書くことになる。-}

sword' :: IO ()
sword' = do
    input <- getLine
    if (input == "SWORDFISH")
        then putStrLn input
        else return ()

--- 🌸sequence
{-
    sequence 関数は、I/O アクションのリストを受け取り、それらを順に実行する I/O アクション（シーケンス）を返す。
    この I/O アクションが生成する結果は、実行したすべての I/O アクションの結果からなるリストである。
    例えば、以下のようなコードがあったとする。
-}

seqTry :: IO ()
seqTry = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a, b, c]

{-　これを次のように書くことができる。-}

seqTry' :: IO ()
seqTry' = do
    rs <- sequence [getLine, getLine, getLine]
    print rs

{-
    これら 2 つの結果はまったく同じになる。sequence [getLine, getLine, getLine] は getLine を 3 回行う I/O アクションを作る。
    このアクションを名前に束縛したら、結果はすべての（getLine の）結果のリストになる。
    であるから、この場合の結果はユーザーがプロンプトから入力した 3 つのものからなるリストになる。
        *Main> seqTry'
        hai
        2
        9.8
        ["hai","2","9.8"]

    sequence を使ったよくあるパターンは、リストに対して print や putStrLn のような関数を map するときである。
    map print [1,2,3,4] は I/O アクションを作らない。
    代わりに I/O アクションのリストを作る。意味的には、これは次のように書いたのと同じ。
        [print 1, print 2, print 3, print 4]

    I/O のリストを I/O アクションに変換したいなら、それをシーケンスにしないといけない。
        *Main> sequence $ map print [1,2,3,4]
        1
        2
        3
        4
        [(),(),(),()]

    出力の最後にある [(), (), (), ()] はなんだろうか？
    GHCi で I/O アクションを評価すると、端末にはその結果が表示される。
    ただし結果が () のときだけは例外である。
    この例外があるので、putStrLn "hehe" を評価すると GHCi は hehe とだけ表示する。
    putStrLn "hehe" は () を生成するからである。
    しかし GHCi に getLine を入力した場合には I/O アクションの結果が表示される。
    getLine の型は IO String だからである。

    🤔❓

    （ん…？　だから、seqTry' のところの例では、sequence [getLine, getLine, getLine] は rs に束縛された。
    　一方、sequence $ map print [1,2,3,4] つまり sequence [print 1, print 2, print 3, print 4] では、
    　print 1 とか print 2 とかの型が print 1 :: IO () のように、() を結果として返す I/O アクションだから、
    　それをリストにしたときに返されるものとしては [(), (), (), ()] のようになるよねということ（？　いまいちよくわからないが）
    というか、結局、型が () だったら GHCi のターミナルには表示されないけど、() 以外だったら表示されるよってことで、
    今回は [(), (), (), ()] で () とは違うから表示されるんだよってことだろうね。
    return () すると端末には何も出てこないけど return "hoge" とやったり return [()] とするとそれぞれ "hoge"、[()] を結果が表示されるのでわかる）

    さらにもっというと、Jupyter Notebook みたいなもんってことでは？
    途中の 1 2 3 4 は副作用が表示されてて、最後の [(), (), (), ()] は関数の出力が出てるってことよね。
    んで、関数の出力が () の場合は () は表示されない仕様だよってことだよね。
    ちなみに、 sequence $ map print [1,2,3,4] の型を調べると IO [()] だよ。
    （なお、putStrLn "hehe" :: IO () である）
-}

--- 🌸 mapM
{-
    「リストに対して I/O アクションを返す関数をマップし、それからシーケンスにする」という操作は頻出するので、
    ユーティリティ関数 mapM と mapM_ が用意されている。
    mapM は関数とリストを受け取り、リストに対して関数をマップして、それからそれをシーケンスにする。
    mapM_ も同じことをするが、そのあとで結果を捨ててしまう。
    I/O アクションの結果が必要ないときは mapM_ を使う。
    mapM の使用例を次に示す。
-}

mapm :: IO [()]
mapm = mapM print [1 :: Int, 2 :: Int, 3 :: Int]
{-
# 結果
1
2
3
[(),(),()]
-}

-- 3 つのユニットからなるリストは不要なので、以下のようにしたほうがいいだろう。

mapm' :: IO ()
mapm' = mapM_ print [1 :: Int, 2 :: Int, 3 :: Int]
{-
# 結果
1
2
3
-}

--- 🌸forever
{-
    forever 関数は I/O アクションを受け取り、その I/O アクションを永遠に繰り返す I/O アクションを返す。
    Control.Monad で定義されている。
    次の小さなプログラムは、無限にユーザーからの入力を受け取り、それを大文字化して出力し続ける。
    Ctrl + C（Ctrl + D）で強制終了しないと終わらない。
-}

foreverTry :: IO ()
foreverTry = forever $ do -- forever がなければ、一回だけ入力を受け付けてそれを大文字にして返したらおわり。
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

--- 🌸 forM
{-
    forM（Control.Monad にある）は、mapM に似ているが引数の順序が逆になっている。
    最初の引数がリストで、2 番目がそのリストにマップする関数である。
    何の役に立つのだろうか？
    ラムダ式と do 記法をうまく組み合わせて以下のような書き方ができるのである。
-}

forMtry :: IO [()]
forMtry = do
    colors <- forM [1::Int, 2::Int, 3::Int, 4::Int] $ \a -> do
        putStrLn $ "Which color do you associate with the number "
                    ++ show a  ++ "?"
        color <- getLine
        return color
    putStrLn "The colors that you associate with 1, 2, 3, and 4 are: "
    mapM putStrLn colors

{-
*Main> forMtry 
Which color do you associate with the number 1?
black
Which color do you associate with the number 2?
red
Which color do you associate with the number 3?
blue
Which color do you associate with the number 4?
orange
The colors that you associate with 1, 2, 3, and 4 are: 
black
red
blue
orange
[(),(),(),()]
-}

{-
    \a -> do ... というラムダ式は、数を受け取り I/O アクションを返す関数である。
    do の最後で return color を呼んでいることに注目。
    この do ブロックはユーザーの選択した色を表す文字列を返すと定義しているのでこうしている。
    ただ、実際にこう書く必要はない。
    getLine はすでに選択した色を返していて、それが　do ブロックの最後に位置しているからである。
    color <- getLine を実行したあとで return color を行うのは、getLine の結果をほどいてから再度梱包しているだけなので、
    単に getLine を呼び出すのと同じである。

    forM 関数を 2 つの引数で呼び出すと、I/O アクションが生成され、その結果は colors に束縛される。
    colors は文字列を含む普通のリストである。
    最後にすべての色を mapM putStrLn colors を呼んで表示している。

    forM はこんなふうに考えればいいだろう。
    「このリストの各要素に対応する I/O アクションを作る。それぞれの I/O アクションの動作は、アクションを作るのに使った要素に応じたものにできる。
    最終的には、これらのアクションが実行された結果が何かに束縛される（結果が必要なければ丸ごと捨ててしまうこともできる）」

    forM を使わなくても同じことはできる。
    しかし forM を使うとコードが読みやすくなる。do 記法を使った何らかのアクションをマップしてシーケンスにしたい場合、普通は forM を使う。
    （💡mapM と forM はリストと関数、どちらの引数を長く書きたいかによって使い分けるのがよいだろう。
    　　関数を長く書きたい場合は今見たように forM がよいだろうし、関数が短い（print だけとか）なら mapM がよいだろう）
-}


-------------------------------
--　I/O アクションおさらい
-------------------------------

{-
    I/O の基本をざっとおさらいしておこう。
    I/O アクションというのは値であり、Haskell の他の値とよく似ている。
    関数の引数として渡すことができて、関数の結果として I/O アクションを返すことができる。

    I/O アクションが特別なのは、main 関数の中に入っていると（あるいは GHCi のターミナルで評価されると）、それが実行されるところである。
    画面に何かを表示したり、音楽をスピーカーから再生したりするのである。
    またどの I/O アクションも、実世界から取得してきたものを伝える結果を生成できる。
-}