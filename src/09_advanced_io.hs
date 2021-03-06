{-# OPTIONS -Wall -Werror #-}
import System.IO
--import System.Environment
import Control.Exception
import System.Random
import Control.Monad(when)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
import GHC.Word

---_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/---
--   　　もっと入力、もっと出力　　　    --
---_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/---

-------------------------------
--　ファイルとストリーム
-------------------------------

{-
    I/O アクションと仕組みの知識を手に、Haskell でファイルの読み書きをしていくことにする。
    その前に、どうすれば Haskell でストリームデータを簡単に扱えるのかを見ていこう。
    ストリームとは、時間をかけてプログラムに出たり入ったりする連続したデータ片のこと。
    例えば、キーボードからプログラムに文字を入力するとき、その文字たちをストリームと考えることができる。
-}

--- ¶　入力のリダイレクト
{-
    多くの対話型プログラムはキーボードからユーザの入力を受け取る。
    しかし、テキストファイルの内容をプログラムに入力として与えることができれば、さらに便利である。
    そのために**入力のリダイレクト**を使う。

    Haskell プログラムで入力のリダイレクトができると便利なので、それをどのように行うのかを見てみよう。
    はじめに、次のような俳句を含む小さなファイル haiku.txt を作る。

        ```txt
        I'm a lil' teapod
        What's with that airplane food, huh?
        It's so small, tasteless
        ```

    行を読み込んで大文字化して表示を繰り返す小さなプログラムを書こう。

        import Control.Monad
        import Data.Char

        main = forever $ do -- forever は Control.Monad で定義されている
            l <- getLine
            putStrLn $ map toUpper l

    このプログラムを capslocker.hs として保存し、コンパイルする。
    キーボードから行を入力する代わりに、haiku.txt をプログラムにリダイレクトして入力しよう。
    入力をリダイレクトするには、プログラム名のあとに < という文字と、それに続けて入力したいファイル名を指定する。

    つまり、
        ./capslocker < haiku.txt
    とすればよい。

        $ src/io/capslocker < src/io/haiku.txt
        I'M A LIL' TEAPOD
        WHAT'S WITH THAT AIRPLANE FOOD, HUH?
        IT'S SO SMALL, TASTELESS
        capslocker: <stdin>: hGetLine: end of file
-}

--- ¶　入力ストリームから文字列を得る
{-
    入力ストリームを扱いやすい普通の文字列にしてくれる I/O アクション getContents を見ていこう。
    getContents は、標準入力から EOF 文字に達するまですべての文字を読み込む。
    その型は getContents :: IO String である。
    getContents のいいところは遅延 I/O を行うところである。
    どういうことかというと、foo <- getContents としても getContents は一度に全部の入力をメモリに読み込んで foo に束縛しないのである。
    getContents は遅延するのである！「あとでちゃんと端末から入力を受け取るよ。それが本当に必要になったときに！」と言わんばかりに。

    capslocker.hs の例では、forever を使って 1 行ずつ入力を読み、それを大文字にして表示していた。
    getContents は I/O の細かい面倒を見てくれる。
    その一環として、必要なときに必要なぶんだけ入力を読み込んでくれるのである。
    何か入力を受け取り、それを変換して出力するプログラムなら、getContents を使って簡潔に書ける。

        import Data.Char

        main = do
            contents <- getContents
            putStr $ map toUpper contents

    I/O アクション getContents を実行して、その結果の文字列に contents という名前をつけている。
    それからその文字列を toUpper で写して、その結果を端末に表示する。
    文字列は基本的にはリストなので処理は遅延されるし、getContents は遅延 I/O なので大文字化した文字列を表示する前にすべての中身（コンテンツ）を一度にメモリに
    読み込んだりはしない。必要なときに入力から行を読み込むので、読んだらすぐに大文字化した文字列を表示する。
    試してみよう。

        $ src/io/capslocker2 < src/io/haiku.txt                                                                                                                           
        I'M A LIL' TEAPOD
        WHAT'S WITH THAT AIRPLANE FOOD, HUH?
        IT'S SO SMALL, TASTELESS

    では、capslocker2 をリダイレクトなしで実行し、端末から入力をタイプしたらどうなるだろうか。

        $ src/io/capslocker2                                                                                                                                               
        hey ho
        HEY HO
        Let's Go!        
        LET'S GO!
        ^D

    素晴らしい！　見てのとおり、大文字化された文字列が 1 行ごとに出力されている。

    getContents の結果が contents に束縛されるとき、それは本当の文字列ではなく、
    最終的には文字列に評価されるプロミス（promise）としてメモリ上に置かれる。
    contents に toUpper をマップするとき、それもまた入力の結果に関数をマップするというプロミスになる。
    最終的に putStr が呼ばれると、これがさっきのプロミスに対して「やあ、大文字化された行が必要なんだ！」と言う。
    そのプロミスはまだ入力の行を何も持っていないので、contents に対し「端末からの入力の状況はどうなってる？」と問い合わせる。
    それでようやく getContents は実際に端末から入力して、何か入力をくれと言ってきたコードに生成したものを渡すのである。
    受け取ったコードは渡されたものに toUpper をマップし、その結果を putStr に渡して、画面に行が出力される。
    さらに続けて putStr は「ヘイ、次の行をくれ！　カモン！」と言う。これが、入力がなくなるまで、つまり EOF 文字が入力されるまで繰り返される。

    では、入力を受け取り、10 文字より短い行だけを出力するプログラムを作ってみよう。

        main = do
            contents <- getContents
            putStr (shortLinesOnly contents)

        shortLinesOnly :: String -> String
        shortLinesOnly = unlines . filter (\line -> length line < 10) . lines

    shortLinesOnly 関数は、"short\nloooooooooong\nbort" のような文字列を受け取る。この例では文字列は 3 行で、
    そのうち 2 行は短く、真ん中の 1 行は長い。この文字列は lines 関数を適用すると、文字列のリスト ["short", "loooooooooong", "bort"] に変換される。
    これに 10 文字未満の文字列をフィルタする関数が適用され、["short", "bort"] になる。最後に、それに unlines を適用し、
    改行文字で区切られた 1 つの文字列 "short\nbort\n" になる。

    $ ./src/io/shortlinesonly < src/io/shortlines.txt                                                                                                                  
    i'm short
    so am i
    short
-}

--- ¶　入力を変換する
{-
    「入力を文字列として受け取り、それを関数で変換し、結果を出力する」というパターンは頻出なので、これを簡単に済ませるための interact という関数がある。
    interact は String -> String 型の関数を受け取り、入力にその関数を適用して、返ってきた結果を出力する、という I/O アクションを返す。
    先ほどのプログラムを interact を使って書き換えてみよう。

        main = interact shortLinesOnly
        
        shortLinesOnly :: String -> String
        shortLinesOnly = unlines . filter (\line -> length line < 10) . lines

    入力を行ごとに読み込み、それが回文かどうかを出力するプログラムを作ってみよう。
    行を読むのに getLine を使い、それが回文かどうかを出力し、それから main を呼び出して再帰してもいいが、
    interact を使うともっとシンプルに書ける。
    interact によって、入力を望みの出力に変換するのにはどうすればいいかだけを考えればよくなる。
    今回の場合だと、各行を "palindrome" か "not a palindrome" に置き換える。
-}

respondPalindromes :: String -> String
respondPalindromes =
    unlines .
    map (\xs -> if isPal xs then "palindrome" else "not a palindrome") .
    lines

isPal :: String -> Bool
isPal xs = xs == reverse xs

main_ :: IO ()
main_ = interact respondPalindromes

{-
    これを実行すると以下のようになる。
    $ ./src/io/palindrome
    hehe
    not a palindrome
    ABCBA
    palindrome
    cookie
    not a palindrome
    kayak
    palindrome
    akasaka
    palindrome

    大きい 1 つの文字列を別の文字列に変換するプログラムを作ったのに、このプログラムは 1 行ごとに処理するプログラムを書いたかのように動作する。
    これは Haskell が遅延評価だからである。結果の文字列の最初の行を表示したくても、入力の最初の行がまだないので表示できない。
    プログラムは、入力の最初の行を取得したらすぐに出力の最初の行を表示する。
    プログラムから抜け出すには EOF 文字を入力する。
    このプログラムにも入力をファイルからリダイレクトできる。

    遅延 I/O がどのように動作し、それをどう活用するかを見てきた。
    プログラムを書く時は、ある入力に対してどんな出力が考えられるかという視点から考えて、その変換をする関数を書くだけである。
    いま出力したいものは入力によって決まるので、遅延 I/O では本当に必要になるまで入力を一切消費しない。
-}

-------------------------------
--　ファイルの読み書き
-------------------------------

{-
    ここまでは、端末（ターミナル）への表示と端末（ターミナル）からの読み込みに I/O を使った。しかしファイルの読み書きはどうするのだろう？
    端末から読むということは（何か特別な）ファイルから呼んでいると考えることもできる。
    端末への書き出しも、同様に、ある特別なフィアルへの書き出しだと考えられる。
    これら 2 つの特別なファイルを stdin および stdout と呼ぶ。
    ファイルへの入出力は、標準入力からの読み込みと標準出力への書き出しにとてもよく似ている。

    まず、マザーグースの一節が書かれた baabaa.txt というファイルを開き、それを端末に表示するだけの簡単なプログラムを書くことから始めよう。

        ```baabaa.txt
        Baa, baa, black sheep,
        Have you any wool?
        Yes, sir, yes, sir,
        Three bags full;
        ```

    以下がプログラムである。

        import System.IO

        main = do
            handle <- openFile "baabaa.txt" ReadMode
            contents <- hGetContents handle
            putStr contents
            hClose handle

    コンパイルして実行すれば、期待した結果が得られる。
    $ ./src/io/baabaa
    Baa, baa, black sheep,
    Have you any wool?
    Yes, sir, yes, sir,
    Three bags full;%  

    プログラム全体は、複数の I/O アクションを do ブロックでまとめたものになっている。do ブロックの最初の行は初登場の関数 openFile である。
    これは次のような型シグネチャを持っている。
        openFile :: FilePath -> IOMode -> IO Handle
    openFile は、ファイルパスと IOMode を受け取り、そのファイルを開いて、そのファイルに関連づけられたハンドルを返す I/O アクション（それが IO Handle である）を返す。

    FilePath は単なる String の型シノニムである。現に、:i FilePath とすると以下の情報が得られる。
        type FilePath = String

    IOMode は次のように定義されている。
        data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
    この型は、曜日を表す 7 つの値を取りうる型のように、開いたファイルに対して何をしたいのか列挙した型である。
    この型は IOMode であって IO Mode ではないことに気を付けること。IO Mode だと、何か Mode 型の結果を生成する IO アクションを意味する（IO () とか IO String のように）。

    最終的に OpenFile は、指定されたファイルを指定されたモードで開く I/O アクションを返す。そのアクションの結果を何かに束縛すれば、そのファイルに対する Handle が得られる。
    そのハンドルが読み込むファイルを示している。

    次の行では hGetContents という関数を使っている。
    これは、コンテンツをどのファイルから読み出すべきか知っている Handle を受け取り、そのファイルに含まれる内容を結果として返す IO String を返す。
        hGetContents :: Handle -> IO String
    この関数は getContents にとてもよく似ている。
    唯一の違いは、getContents が自動的に標準入力（つまりターミナル）から入力するのに対し、hGetContents は渡されたハンドルから入力するという点である。
    それ以外の挙動は同じ。

    hGetContents は、getContents のようにファイルの内容を一度にメモリに読み込むことはせず、必要になったときに必要な分だけコンテンツを読む。
    これは、ファイル全体のコンテンツが contents として扱えるにもかかわらず実際にはメモリに読み込まれていないという、本当に素晴らしい機能である。

    だから、たとえとてつもなく大きなファイルを読み込んだとしても、hGetContents はメモリを食い潰すことはない。

    ファイルのハンドルと実際のコンテンツの違いにも注意せよ。ハンドルはファイルの現在の位置を指し示すポインタに過ぎない。
    コンテンツはファイルに実際に書かれているものである。
    ファイルシステム全体をとても大きな本だとすると、ハンドルは今呼んでいる（もしくは書いている）ところを指し示すしおりのようなものである。

    putStr contents でファイルのコンテンツを標準出力に表示し、それからハンドルを受け取って、そのハンドルを閉じる hClose を実行する。
    openFile で開いたファイルは自分で閉じる必要があるのである。
    ハンドルが閉じられていないファイルを開こうとすると、プログラムは強制終了する。
-}

--- ¶　withFile 関数を使う
{-
    ファイルの内容を扱うもう 1 つの方法は、次のようなシグネチャを持つ withFile 関数を用いるものである。

        withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a

    この関数は、ファイルのパスと IO Mode、それに「ハンドルを受け取って I/O アクションを返す関数」を受け取り、
    「そのファイルを開いてから何かして閉じる」という I/O アクションを返す。
    さらに withFle は、ファイルの操作中に何かおかしなことが起こった場合にもファイルのハンドルを確実に閉じてくれる。
    ラムダ式と一緒に使うと便利。

    先ほどの例を withFile を使って書き換えると以下になる。

        import System.IO

        main = do
            withFile "baabaa.txt" ReadMode $ \handle -> do
                contents <- hGetContents handle
                putStr contents

    \handle -> ...（からブロックの最後まで）はハンドルを受け取り I/O アクションを返す関数である。
    こんなふうに withFile にはよくラムダ式で関数を渡す。
    実行したい I/O アクションを渡してファイルを閉じるだけではなく、どのファイルを操作するのか、そのハンドルを I/O アクションに教えてやらなければならないので、
    このようにして（ハンドルを受け取って）I/O アクションを返す関数（＝ラムダ式）を渡す必要があるのである。
    withFile はファイルを開いてそのハンドルを受け取った関数に渡す。withFile は、返ってきた I/O アクションと同じ動作をし、なおかつ何か失敗した場合でも
    ファイルハンドルを確実に閉じてくれるような I/O アクションを作る。
-}

--- ¶　ブラケットの時間
{-
    error が呼ばれたり（空リストに対して head が呼ばれたときなど）、あるいは入出力の際にとてもまずいことが起こると、通常はプログラムが強制終了させられ、何らかのエラ〜メッセージが表示される。
    そのような状況を、**例外**が投げられたという。
    withFile 関数は、このような忌むべき例外が投げられたときでもファイルのハンドルを閉じてくれるのである。

    この例のような、「何らかのリソース（例えばファイルのハンドル）を獲得し、それに対して何かを行う、
    ただしリソースが確実に開放される（例えばファイルのハンドルを閉じる）ことを保証する」というシナリオは、わりとよく登場する。
    そのために、Control.Exception モジュールに bracket という関数が用意されている。この関数は次のような型シグネチャを持っている。
        bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
    最初の引数はリソース（ファイルハンドルのような）の獲得を行う I/O アクションである。
    2 番目の引数はリソースを開放する関数である。この関数は例外が投げられた場合でも呼ばれる。
    3 番目の引数はリソースを受け取り、それを使って何かを行う関数である。
    ファイルからの読み込みやファイルへの書き出しといったメインの操作は、この 3 番目の引数にあたる。

    リソースを獲得し、それを使って何かを行い、そして確実に開放することが bracket のすべてだから、withFile を実装するのもとても簡単。
-}

-- import Control.Exception
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' name mode f = bracket (openFile name mode)
    (\handle' -> hClose handle')
    (\handle' -> f handle')
    -- わかりやすさのためにラムダ式を使っているんだろうけど、 withFile' name mode f = bracket (openFile name mode) hClose f としても全く同じ。

{-
    bracket に渡す最初の引数によってファイルが開き、その結果はファイルのハンドルである。
    たとえ例外が発生しようとも、確実にハンドルを閉じる処理を実行する。
    最後の 3 つ目の引数は、ハンドルを受け取り、それに f を適用する。この f は、ファイルハンドルを受け取り、そのハンドルに対してファイルからの読み書きといった操作を行う関数。

    以下、参考に型シグネチャを示しておく。
    bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c（再掲）
    openFile :: FilePath -> IOMode -> IO Handle
    hClose :: Handle -> IO ()
-}

--- ¶　ハンドルを握れ！
{-
    hGetContents が指定したファイルに対して動作する getContents であるように、hGetLine、hPutStr、hPutStrLn、hGetChar のような関数にも h がつかないバージョンがあって、
    ハンドルを取らないかわりに標準入出力に対して動作する。
    例えば、putStrLn は文字列を受け取り、それをターミナルに表示してから改行文字を出力するという I/O アクションを返す。
    hPutStrLn はハンドルと文字列を受け取り、ハンドルに対応したファイルに文字列を書き込み、さらに改行文字を書き込むという I/O アクションを返す。
    同様に、hGetLine はハンドルを受け取り、そのファイルから 1 行入力する I/O アクションを返す。

    ファイルを読み込み、そのコンテンツを文字列として扱うというのは、とても一般的な操作である。
    そのため、これを手軽にするためのシャレた関数が 3 つ用意されている。
    readFile、writeFile、appendFile である。

    readFile 関数は、readFile :: FilePath -> IO String という型シグネチャを持つ関数である（FilePath は String の別名に過ぎない）。
    readFile はファイルのパスを受け取り、そのファイルを読み込み（もちろん遅延する）、その内容を表す文字列を返す I/O アクションを返す。
    普通なら openFile を呼んでから hGetContents をそのハンドルで呼び出さなければいけないので、それよりお手軽である。
    前の例を readFile 関数を使って書くと以下のようになる。

        import System.IO

        main = do
            contents <- readFile "baabaa.txt"
            putStr contents

    ファイルを示すハンドルを獲得しないので、それを手動で閉じることはできない。readFile を使う場合、ハンドルを閉じるのは Haskell が自動で行う。

    writeFile は writeFile :: FilePath -> String -> IO () 型の関数である。
    これはファイルのパスと、そのファイルに書き込みたい文字列を受け取り、その書き込みを行う I/O アクションを返す。
    指定されたファイルがすでに存在している場合、ファイルは上書きされる。次のコードは baabaa.txt から、それを大文字化したバージョンの baabaacaps.txt を生成するプログラムである。

        import System.IO
        import Data.Char

        main = do
            contents <- readFile "baabaa.txt"
            writeFile "baabaacaps.txt" (map toUpper contents)

    appendFile 関数は writeFile と同じ型シグネチャを持ち、同じような動作をするが、
    appendFile はすでにファイルが存在していた場合に上書きをするのではなくファイルの末尾に追記するという点が異なる。
-}

-------------------------------
--　ToDo リスト
-------------------------------
{-
    ToDo リストをテキストファイルに追加するプログラムを作るのに appendFile 関数を使ってみよう。
    todo.txt という名前のファイルにタスクが 1 行ごとに書かれているものとする。
    プログラムは、標準入力から 1 行ずつ読み込んで、それを ToDo リストに追加する。

        ```
        import System.IO

        main = do
            todoItem <- getLine
            appendFile "todo.txt" (todoItem ++ "\n")
        ```

    各行の最後に "\n" を追加していることに注意。getLine は改行文字を除いた文字列を返してくるからである。
    これを appendtodo.hs に保存して、コンパイルして、実行する。

        $ ./src/todo/appendtodo
        Iron the dishes
        $ ./src/todo/appendtodo
        Dust the dog
        $ ./src/todo/appendtodo
        Take salad out of the oven
-}

--- ¶　アイテムの削除
{-
    todo.txt の ToDo リストに新しいアイテムを追加するプログラムを作った。
    次はアイテムを削除するプログラムを作ろう。
    System.DIrectory の新しい関数をいくつかと、System.IO の新しい関数を 1 つ使う。

        ```
        import System.IO
        import System.Directory
        import Data.List

        main = do
            contents <- readFile "todo.txt"
            let todoTasks = lines contents
                numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
            putStrLn "These are your TO-DO items:"
            mapM_ putStrLn numberedTasks
            putSTrLn "Which one do you want to delete?"
            numberString <- getLine
            let number = read numberString
                newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
            (tempName, tempHandle) <- openTempFile "." "temp"
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile "todo.txt"
            renameFile tempName "todo.txt"
        ```

    最初に todo.txt を読み込み、その内容を contents に束縛する。それから、その文字列を行ごとに分割し、文字列のリスト todoTasks にする。
    ここで todoTasks は次のようになっているはずである。
        ["Iron the dishes", "Dust the dog", "Take salad out of the oven"]
    このリストと、0 から始まるリストを、数（例えば 3）と文字列（例えば "hey"）を受け取り、新しい文字列（"3 - hey"）を返す関数で zip する。
    さて、numberedTasks は次のようになっているはずである。
        ["0 - Iron the dishes"
        ,"1 - Dust the dog"
        ,"2 - Take salad out of the oven"
        ]
    それから mapM_ putStrLn numberedTasks で行ごとに表示して、どれを削除したいのかをユーザに問い合わせる。
    いま、1 (Dust the dog) を削除したいとしよう。ターミナルに 1 を打ち込む。そうすると、numberString は "1" になる。
    文字列ではなく数が欲しいので、この文字列に対して read を適用する。すると 1 が得られ、これが let で number に束縛される。

    `Data.List の delete` と !! 関数 は覚えているだろうか？（delete は初出じゃないかな〜）
    !! は、添字に対応するリストの要素を返す（インデックスアクセス的な）。
    delete は、リストから指定した要素のうち最初に出てくるものを削除した新しいリストを返す。
        > delete "hoge" ["hoge", "fuga", "moge", "hoge"]
        ["fuga","moge","hoge"]
    (todoTasks !! number) は "Dust the dog" になる。そして todoTasks の中から "Dust the dog" の最初の出現を削除し、
    unlines を使って改行文字で区切られた 1 つの文字列に連結する。
    それが newTodoItems になる。

    それから、System.IO にある初登場の関数 openTempFile を使う。
        openTempFile :: FilePath -> String -> IO (FilePath, Handle)
    この関数は一時ディレクトリ（temp directory）のパスとファイル名のテンプレートを受け取り、一時ファイルを開く。
    ここでは一時ディレクトリとして "." を使っている。"."　はカレントディレクトリを顕wす。
    一時ファイル名のテンプレートとしては "temp" を使っている。
    一時ファイルの名前は、"temp" の後ろにランダムな文字をいくつか付けたものになる。
    openTempFile が返す IO アクションは、一時ファイルを開き、そのファイル名とハンドルのペアを返す。
    todo2.txt のような名前の普通のファイルを開いても同じことはできるが、openTempFile を使うようにせよ。
    openTempFile を使えば、何かが入っているファイルにうっかり上書きしないことが保証されるからである。

    一時ファイルを開いたので、それに newTodoItems を書き込む。
    古いファイルは変更されず、削除すべきものを削除した新しいリストが一時ファイルに格納される。

    そのあとで、一時ファイルのハンドルを閉じ、removeFile で元のファイルを削除する。
    （なお、removeFile :: FilePath -> IO () である）
    古い todo.txt を削除したら、renameFile を使って一時ファイルの名前を todo.txt に変更する。
    removeFile と renameFile（どちらも System.Directory の関数）は、引数としてハンドルではなくファイルのパスを受け取る。
    このプログラムを deletetodo.hs に保存し、コンパイルして実行してみよう。

    $ src/todo/deletetodo                                                                                                                                              +[master]
    These are your TO-DO items:
    1 - Iron the dishes
    2 - Dust the dog
    3 - Take salad out of the oven
    Which one do you want to delete?
    1

    どのアイテムが残っているかを見てみると、
        Iron the dishes
        Take salad out of the oven
    という結果になる。

    いい感じ！　だが、一点気になるポイントがある。
    一時ファイルを開いた後でプログラムが異常終了したら、一時ファイルが残ってしまうということである。
    これを修正しよう。
-}

---　¶　クリーンアップ
{-
    問題が起こった場合でも一時ファイルが確実に削除されるようにするために、Control.Exception にある bracketOnError 関数を使うことにすsる。
    この関数は bracket によく似ているが、bracket では処理が終わると常に獲得したリソースを開放するのに対し、
    bracketOnError は何らかの例外が発生したときのみリソースを開放する。

        ```
        import System.IO
        import System.Directory
        import Data.List

        main = do
            contents <- readFile "todo.txt"
            let todoTasks = lines contents
                numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
            putStrLn "These are your TO-DO items:"
            mapM_ putStrLn numberedTasks
            putSTrLn "Which one do you want to delete?"
            numberString <- getLine
            let number = read numberString
                newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
            
            bracketOnError (openTempFile "." "temp")
                (\(tempName, tempHandle) -> do -- 例外が発生したときの処理
                    hClose tempHandle
                    removeFile tempName)

                (\(tempName, tempHandle) -> do
                    hPutStr tempHandle newTodoItems -- 正常系
                    hClose tempHandle
                    removeFile "todo.txt"
                    renameFile tempName "todo.txt")
        ```

    普通に openTempFile を使うのではなく、bracketOnError と一緒に使った。
    引数として、エラーが発生したときにすべきこと、つまり、一時ハンドルを閉じてから一時ファイルを削除するというラムダ式を渡している。
    最後に、一時ファイルを使って何をしたいか（正常系）を記述している。この部分は前と同じ。
    新しいアイテムリストを書き出し、一時ファイルのハンドルを閉じて、今のファイルを削除し、一時ファイルの名前を変更している。
-}

-------------------------------
--　コマンドライン引数
-------------------------------

{-
    ターミナルで動作するスクリプトやアプリケーションを作りたいなら、コマンドライン引数を扱うことは不可欠だ。
    ここまで作った ToDo アプリの問題は、ToDo ファイル（todo.txt）の名前がハードコードされていることである。

    1 つの解決方法は、ToDo リストのファイル名を毎回ユーザーに尋ねるというものである。
    このアプローチは、どのアイテムを削除するか決めるときに使った。
    それでも動作するが、これはユーザに「プログラムを実行してから、プログラムが何か聞いてくるのを待って、プログラムに何か入力する」という要求をするわけで、
    あまり理想的な解決策ではない。ちなみに、これは対話的（interactive）プログラムと呼ばれている。

    対話的なコマンドラインプログラムには難点がある。スクリプトから呼び出してプログラムの実行を自動化したいとき、どうすればいいだろう？
    プログラムと対話するスクリプトを書くのは、単純にプログラムを呼び出すスクリプトを書くより難しいものである。
    そのため、プログラム実行に必要な情報は、プログラムを実行している間ではなく、プログラムを起動するときにユーザーに問い合わせるようにしたい。
    プログラムに何をさせたいかをユーザからプログラムに伝える方法として、コマンドライン引数よりマシな方法はない。

    System.Environment モジュールは、コマンドライン引数を取得するのに便利な 2 つの I/O アクション、getArgs と getProgName を提供している。
    getArgs は getArgs :: IO [String] という型を持つ。これは、プログラムに与えられた引数を取得して、それを文字列のリストとして返す I/O アクションである。
    getProgName は getProgName :: IO String という型を持つ。
    これはプログラム名を返す I/O アクションである。これらがどのように動作するか、次の小さなプログラムで見てみよう。

        import System.Environment
        import Data.List

        main :: IO ()
        main = do
            args <- getArgs
            progName <- getProgName
            putStrLn "The arguments are:"
            mapM_ putStrLn args
            putStrLn "The program name is:"
            putStrLn progName

    最初にコマンドライン引数を args に束縛して、それからプログラム名を progName に束縛する。
    次に、putStrLn を使ってプログラムの引数をすべて表示し、それからプログラム地震の名前を法事する。
    このコードを arg-test としてコンパイルし、実行してみよう。

    $ ./src/io/arg-test first second w00t "multi word arg"
    The arguments are:
    first
    second
    w00t
    multi word arg
    The program name is:
    arg-test
-}

-------------------------------
--　ToDo リストをもっと楽しむ
-------------------------------

{-
    前の例では、タスクを追加するプログラムと削除するプログラムを完全に別々のプログラムとして作成した。
    ここでは、両方のプログラムを 1 つにまとめて、追加するか削除するかをプログラムに渡すコマンドライン引数で選択できるようにしよう。
    さらに、todo.txt ではなく別のファイルも操作できるようにしよう。
    このプログラムを todo という名前にすることにして、次の 3 つの異なる操作を行えるようにする。
        - タスクの閲覧
        - タスクの追加
        - タスクの削除
    タスクを todo.txt に追加するには、端末に次のように入力することにする。
        $ ./todo add todo.txt "Find the magic sword of power"
    タスクを閲覧するには view コマンドを入力する。
        $ ./todo view todo.txt
    タスクの削除には番号を使う
        $ ./todo remove todo.txt 2
-}

--- ¶　マルチタスクタスクリスト
{- 
    まず、コマンドを "add" や "view" のような文字列として受け取り、引数のリストを受け取って望みの動作を行う I/O アクションを返す関数を作る。

    ```
    import System.Environment
    import System.Directory
    import System.IO
    import Data.List
    import Control.Exception

    dispatch :: String -> [String] -> IO ()
    dispatch "add" = add
    dispatch "view" = view
    dispatch "remove" = remove

    main = do
        (command:argList) <- getArgs
        dispatch command argList
    ```

    最初に、コマンドライン引数を取得してそれらを (command:argList) に束縛する。
    これは、最初の引数を command に束縛して、残りの引数を argList に束縛するという意味のパターンマッチである。
    main ブロックの次の行で dispatch 関数にコマンド（command）を渡し、これは add、view、remove のいずれかを返す。
    それから、その関数に argList を渡す。

    次に、add 関数を実装しよう。

    ```
    add :: [String] -> IO ()
    add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
    ```

    今度は view 関数を実装する。
    
    ```
    view :: [String] -> IO ()
    view [fileName] = do
        contents <- readFile fileName
        let todoTasks = lines contents
            numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
        putStr $ unlines numberedTasks
    ```

    最後に remove 関数を実装する（上記の「¶　アイテムの削除」でやったことと同じ）。

    ```
    remove :: [String] -> IO ()
    remove [fileName, numberString] = do
        contents <- readFile fileName
        let todoTasks = lines contents
            numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
        putStrLn "These are your TO-DO items:"
        mapM_ putStrLn numberedTasks
        let number = read numberString
            newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
        bracketOnError (openTempFile "." "temp")
            (\(tempName, tempHandle) -> do
                hClose tempHandle
                removeFile templFile)

            (\(tempName, tempHandle) -> do
                hPutStrLn tempHandle newTodoItems
                hClose tempHandle
                removeFile fileName
                renameFile tempName fileName)
    ```

    改めてここでの解法をまとめると、コマンドから「リストの形でコマンドライン引数を受け取って I/O アクションを返す関数」への橋渡しをする dispatch を作った。
    関数 dispatch は、”command が何か” に基づいて適切な関数を返す。
    その関数をコマンドライン引数の残りと一緒に呼び出し、適切な操作を行う I/O アクションを取得して、それを実行する。
    このように、高階関数を使うことで、まず distpatch 関数から適切な関数を受け取り、次にその関数にコマンドライン引数を渡して I/O アクションを得る、という設計が可能になったのである。

    dispatch 関数を使っていることで、簡単に機能を追加できるという利点もある。
    dispatch にパターンを追加して対応する関数を実装するだけで万事 OK！

    ※　以下、使用例。パス（path）に注意。親ディレクトリから子（/src/todo）ディレクトリ内のファイルを実行するときは、その親ディレクトリから見た相対パスでファイルを指定する必要がある
    　（そのときに単に「todo.txt」と指定すると、その子ディレクトリの中ではなく、親ディレクトリと同じディレクトリに todo.txt が作成されたりする）が、/src/todo ディレクトリに移動して
    　　実行すると、単に「todo.txt」と指定しても、その子ディレクトリの中に todo.txt が作成されるなどといった動きになる）
    ~/haskell-playground/src/todo thesugar $ ./todo view todo.txt
    0 - Iron the dishes
    1 - Take salad out of the oven
    ~/haskell-playground/src/todo thesugar $ ./todo add todo.txt "Pick up children from dry cleaners"
    ~/haskell-playground/src/todo thesugar $ ./todo view todo.txt
    0 - Iron the dishes
    1 - Take salad out of the oven
    2 - Pick up children from dry cleaners
    ~/haskell-playground/src/todo thesugar $ ./todo remove todo.txt 2
    These are your TO-DO items:
    0 - Iron the dishes
    1 - Take salad out of the oven
    2 - Pick up children from dry cleaners
    ~/haskell-playground/src/todo thesugar $ ./todo view todo.txt
    0 - Iron the dishes
    1 - Take salad out of the oven
-}

--- ¶　不正な入力に対応する
{- 
    まずは、すべてを拾うパターンを dispatch 関数の最後に追加して、コマンドが存在しなかった旨を表示する関数を返すようにする。

        ```
        dispatch :: String -> [String] -> IO ()
        dispatch "add" = add
        ...
        dispatch command = doesntExist command

        doesntExist :: String -> [String] -> IO ()
        doesntExist command _ =
            putStrLn $ "The " ++ command ++ " command doesn't exist" 
        ```

    add, view、remove のそれぞれの関数に対してもすべて拾うパターンを追加して、与えられたコマンドに対して
    引数の数が違うということをユーザーに伝えることができる。
        
        ```
        add :: [String] -> IO ()
        add [fileName, todoItems] = appendFile fileName (todoItem ++ "\n")
        add _ = putStrLn "The add command takes exactly two arguments"
        ```

    まだすべての不正な入力をカバーしきれていないことに注意。
    たとえば、プログラムを `./todo` のように（引数なしで）実行すると、プログラムはクラッシュする。
    他にも、ファイルを開く前に存在チェックをするといったことも実際には必要だろう。
-}

-------------------------------
--　ランダム性
-------------------------------

{-
    たいていのプログラミング言語は乱数を返す関数を持っている。
    関数を呼び出すたびに違う乱数が返る。さて、Haskell ではどうだろう？
    Haskell が純粋関数型言語だということを思い出そう。
    これはすなわち参照透過性を持つということだった。
    そして参照透過性は、関数が同じ引数で 2 回呼ばれたなら必ず同じ結果を生成しなければならないことを意味する。

    これは本当に素晴らしい性質である。
    なぜなら、そのおかげでプログラムが本当に必要になるまで値の評価を遅らせることができるようになるからである。
    ところが、これが乱数を得るのを少々厄介にしている原因でもある。

    他の言語では、ランダムに見える数を生成するとき、まずは初期データ（例えば現在時刻のようなもの）を受け取り、
    それに基づいてランダムに見える数を生成する。Haskell でも、何か初期データ、あるいはランダム性を受け取り、それから乱数を生成する関数を作れる。
    ランダム性は I/O を使って外から持ってくる。

    System.Random モジュールを見てみよう。このモジュールは乱数生成に必要なすべての関数を持っている。
    それでは、ここからエクスポートされている関数 random を見ていこう。
    型シグネチャは次のようになっている。
        random :: (RandomGen g, Random a) => g -> (a, g)

    RandomGen 型クラスはランダム性の源として扱える型を表し、Random 型クラスはランダムな値になることのできる型を表す。
    例えば、真理値は True か False からランダムに選ぶことにより生成できる。同様に数も生成できる。
    random の型宣言を翻訳してみると、次のような感じになる。「乱数ジェネレータ（ランダム性の源）を受け取り、ランダムな値と新しい乱数ジェネレータを返す」。
    なぜランダムな値と一緒に新しいジェネレータも返す必要があるのかはすぐにわかる。

    random 関数を使うためには、何らかの乱数ジェネレータを手に入れる必要がある。System.Random モジュールには StdGen といういい感じの型がある。
    これは型クラス RandomGen のインスタンスになっている。
    StdGen を手動で作ったり、ある種の乱数値をもとにシステムに生成してもらったりできる。

    手動で乱数ジェネレータを作るには mkStdGen を使う。これは
        mkStdGen :: Int -> StdGen
    という型を持つ。整数を引数に取り、その値をもとに乱数ジェネレータを返す。
    それでは random と mkStdGen を連携させて、（ほぼ）ランダムな数を作ってみよう。

        💡まず `stack install random` として Random パッケージをインストールする必要あり。
-}

rand :: (Int, StdGen)
rand = random (mkStdGen 100)
    -- (-3633736515773289454,693699796 2103410263)

randBool :: (Bool, StdGen)
randBool = random (mkStdGen 100)
    -- (True,4041414 40692)

    -- 同じ乱数ジェネレータ（`mkStdGen 100` など）で random を再度呼び出しても結果は同じ。
    -- 異なる乱数ジェネレータを引数として random 関数に渡すと異なるランダム値が得られる。

{-
    👉　テキストだと、GHC のターミナルに打ち込むときであっても、random (mkStdGen 100) :: (Int, StdGen) というふうに
        型指定しないとエラーが出る、と記述されている（random 関数は Random 型クラスの任意の型を返す可能性があるため）が、
        （仕様が変わったのか、）実際には GHC のターミナルに打ち込むときは random (mkStdGen 100) とするだけで (Int, StdGen) 型の結果が出る。
        （なお、↑ のようにモジュール内に記載するときはどのみち型指定するけど）
-}

--- ¶　コイントス
{-
    3 回のコイントスをシミュレートする関数を書こう。もし random がランダムな値と一緒に新しい乱数ジェネレータを返さなければ、それぞれのコイントスの結果を生成するために、
    この関数に 3 つの乱数ジェネレータを渡す必要がある。
    しかし、1 つのジェネレータで Int 型（たくさんの異なる値を取りうる）のランダムな値を生成できるのだから、3 回のコイントスの結果（8 通りしかない）だって生成できてしかるべきである。
    ランダムな値と一緒に新しいジェネレータをあけす random 関数がここで役に立つ。
    コインを単純に Bool として表すことにしよう。True が裏で、False が表である。
-}

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, _) = random newGen'
    in (firstCoin, secondCoin, thirdCoin)

{-
    random にジェネレータを渡して、コイントスの結果と新しいジェネレータを生成する。
    それから、新しいジェネレータで再度 random を呼び出し、2 つ目のコイントスの結果を得る。
    3 つ目のコインも同様にする。

    *Main System.Random> threeCoins $ mkStdGen 100
    (True,False,False)
    *Main System.Random> threeCoins $ mkStdGen 10
    (True,True,True)
-}

--- ¶　ランダムな関数をもっと
{-
    もっとたくさんのコインを投げたいときはどうすればいいだろうか？　そのために randoms という関数がある。
    これは、ジェネレータを受け取って、そのジェネレータに基づく無限長のランダムな値のリストを返す関数である。

    *Main System.Random> take 5 $ randoms (mkStdGen 11)
    [5260538044923710387,4361398698747678847,-8221315287270277529,7278185606566790575,1652507602255180489]

    *Main System.Random> take 5 $ randoms (mkStdGen 11) :: [Bool]
    [True,True,True,True,False]

    *Main System.Random> take 10 $ randoms (mkStdGen 11) :: [Float]
    [0.26201087,0.1271351,0.31857032,0.1921351,0.31495118,0.866733,0.10540885,8.465308e-2,0.4563836,0.4733292]

    なぜ randoms はリストと一緒に新しいジェネレータを返さないのだろうか？
    randoms 関数は次のようにとても簡単に実装できる。
-}

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value: randoms' newGen

{-
    これは再帰的な定義である。ランダムな値と新しいジェネレータを受け取ってその値を head に、新しいジェネレーターで作ったリストを残りの要素としてリストを作る。
    この関数には無限の長さのリストを生成できて欲しいので新しい乱数ジェネレーターを返してもらうことはできない。
    有限のリストと新しいジェネレーターを生成する関数なら作れる。
-}

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in (value:restOfList, finalGen)

{-
    これもまた再帰的定義になっている。0 個の数を生成したいとしたら、単純に空のリストと与えられたジェネレータをそのまま返す。
    それ以外の個数のランダムな値を返す場合は、まず乱数を 1 つとジェネレータを生成する。
    この数が head の要素になる。それから新しいジェネレータを使って n-1 個の整数を生成し、リストの残りの部分とする。
    これらをくっつけたものと最終的なジェネレータをペアにして結果として返す。

    *Main System.Random> finiteRandoms 0 (mkStdGen 10)
    ([],11 1)
    *Main System.Random> finiteRandoms 1 (mkStdGen 10)
    ([-2774747785423059091],1925364037 2103410263)
    *Main System.Random> finiteRandoms 5 (mkStdGen 10)
    ([-2774747785423059091,-5364865979222864935,5005192715100199576,-2238708107678760508,-1609484772912886991],299355613 2118231989)

    ある範囲の乱数を生成したい場合はどうすればいいだろうか？
    ある数より小さい整数の乱数全部、というのでは、欲しい数に対して大き過ぎたり小さ過ぎたりする。
    サイコロを投げたい場合は？
    これには randomR を使う。型は次のようになっている。
        randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
    random に似ているが、1 つ目の引数として上限と下限のペアを受け取り、その範囲内の値を生成する。

    *Main System.Random> randomR (1,6) (mkStdGen 1)
    (6,80028 40692)

    *Main System.Random> randomR (1,6) (mkStdGen 333333333929434)
    (3,469120347 40692)

    randomRs 関数も同じく用意されていて、指定された範囲の乱数を無限に生成する。

    *Main System.Random> take 10 $ randomRs ('a', 'z') (mkStdGen 3)
    "xnuhlfwywq"
-}

--- ¶　ランダム性と I/O
{-
    ここまでは乱数ジェネレータに手作業で適当な整数を与えていた。これでは現実の問題を扱うには不十分である。
    プログラムが返すのは毎回同じ乱数になってしまう。この動作は望ましくない。
    これを解決するために、System.Random は getStdGen という IO StdGen 型の I/O アクションを提供している。
    このアクションは、なんらかの初期データを使ってシステムの **グローバル乱数ジェネレータ** を初期化する。
    getStdGen はそのグローバル乱数ジェネレータを返す。

    グローバル乱数ジェネレータを使ってランダムな文字列を生成するシンプルなプログラムの例を示す。
-}

-- import System.Random
main_rndm :: IO ()
main_rndm = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a', 'z') gen)

    {- 試してみると以下のようになる
    *Main System.Random> main_rndm
    axcrffbpargzngjrmhvr

    GHCi を再起動すると、、（再起動せずに main_rndm を再度実行しても同じ結果になる）
    *Main> main_rndm
    endjmjoynktidwhqydgt
    -}

{-
なお、getStdGen を 2 回実行しても（プログラム内で getStdGen を 2 箇所に記述しても）、システムは同じグローバル乱数ジェネレータを 2 回返すだけである。
2 つの異なる文字列を得るベストな方法は、現在の乱数ジェネレータを 2 つのジェネレータに分割する、newStdGen アクションである。
このアクションは、グローバル乱数ジェネレータを分割した片方で置き換え、もう片方を結果として返す。
-}

-- import System.Random

main_rndm' :: IO ()
main_rndm' = do
    gen <- getStdGen
    putStrLn $ take 20 $ randomRs ('a', 'z') gen
    gen2 <- newStdGen
    putStrLn $ take 20 $ randomRs ('a', 'z') gen2

{-
    newStdGen を束縛すると、新しい乱数ジェネレータが得られるだけでなく、グローバルジェネレータも更新される。
    これは、getStdGen を再度実行して何かに束縛すると gen に異なるジェネレータが得られることを意味する。
    （main_rndm の場合（gen <- getStdGen 1 つだけで、newStdGen は使用せず）、GHCi を再起動しない限り、何回 main_rndm を実行しても結果は同じ。
    main_rndm' の場合は、実行のたびに、（newStdGen を使って得たランダム値のみならず）getStdGen を使って得たランダム値も変わる）

    次のコードは、ユーザにプログラムが考えた数を当てさせるプログラムである。
-}

-- import System.Random
-- import Control.Monad(when)

main_a4n :: IO ()
main_a4n = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (1::Int, 10::Int) gen --として、1 と 10 に注釈入れるか、この式全体の右に👉のように注釈を入れる。 :: (Int, StdGen)
    putStrLn "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber
        -- newGen <- newStdGen -- ↓ の main_a4n' でやっているように、newStdGen を使ってもいいけど、あんまり意味がない（let (randomNumber, newGen) = ...の newGen を使うで十分だから、
        --                          わざわざ無意味に新しい乱数ジェネレータをここで取得する必要はない。main_a4n' は、main_a4n' 内のコメントで説明するように、新しい乱数ジェネレータをこの部分で得る必要がある）
        askForNumber newGen
{-
    read が parse できない文字列（"haha" のような）をユーザーが入力した場合、プログラムはクラッシュする。
    そういう入力に対してプログラムがクラッシュしないようにしたいなら、文字列の parse に失敗したときに空のリストを返す reads を使おう。
    成功すると望みの値と食べ残しの文字列のペアからなる単一要素のリストを返す。
    （reads の使い方よくわからないけどいったん省略）
-}

--- 上記のコードは以下のように、すべて main でやっちゃうようにも実装できる（でも、↑のほうが推奨）。
main_a4n' :: IO ()
main_a4n' = do
    gen <- getStdGen
    let (randNumber, _) = randomR (1::Int, 10::Int) gen -- 全部 main でやっちゃうわけで、main は引数を取らないので、
                                                        -- ここで newGen を作ったところでそれを main newGen みたいな形で与えられないから、ここで生成される新しい乱数ジェネレータは捨てる
    putStrLn "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber
        _ <- newStdGen -- ここでグローバル乱数ジェネレータを更新。newStdGen を呼び出せば getStdGen から得られるジェネレータも更新されるから、newStdGen 自体から得られる StdGen 自体は _ に束縛（つまり捨てる）でよい。
        main_a4n'

-------------------------------
--　bytestring
-------------------------------

{-
    リストは便利である。リストに対して動作する関数はたくさんあるし、Haskell の遅延評価のおかげで、
    リストに対するフィルタやマップとして他の言語における for や while ループが書ける。
    本当に必要とされたときにのみ評価されるので、無限リスト（無限リストの無限リストさえも！）のようなものを扱うのも造作はない。
    このため、リストをストリームとして扱うことができる。
    標準入力から読み込むときも、ファイルから読み込むときも、ファイルを開き、それを文字列として読むだけである。
    実際のファイルアクセスは必要になってから行われる。

    しかし、ファイルを文字列として処理することには 1 つ難点がある。実行速度が遅くなりがちなのである。
    リストは本当に怠け者である。[1,2,3,4] は 1:2:3:4:[] の構文糖衣だった。
    例えばリストを出力すると、最初の要素が無理やり評価されるが、このとき残りのリスト（2:3:4:[]）はまだプロミスのまま。この未評価の部分をサンク（thunk）と呼ぶ。
    サンクというのは、要するに遅延された計算のことである。
    Haskell の遅延評価は、前もってすべてを計算するのではなく、サンクを使ってそれを必要なときのみ計算することで実現されている。
    したがって、リストはプロミスだと考えられる。それも、必要になると初めて次要素と後続のプロミスを渡してくれるようなプロミスである。
    単なる数のリストをサンクの列として処理するのは、どう考えても効率の面でベストではなかろう。

    大きなファイルを読んだり、操作しようとしたときに、多くの場合このオーバーヘッドが問題になるのである。これが Haskell に bytestring が存在する理由である。
    bytestring はリストに似たデータ構造で、要素は 1 バイト（8 ビット）のサイズ固定である。bytestring では遅延評価を扱う方法も異なる。
-}

-- ¶　正格 bytestring と 遅延 bytestring
{-
    bytestring には「正格」なものと「遅延」なものがある。正格 bytestring は Data.ByteString で提供されていて、遅延性が完全に排除されている。サンクは一切ない。
    正格 bytestring は配列上のバイト列として表現される。無限の正格 bytestring のようなものは作れない。
    正格 bytestring の最初のバイトを評価するなら、全体を評価しなくてはならない。

    一方、遅延 bytestring は Data.ByteString.Lazy で提供される。これは遅延評価されるが、リストほどは遅延されない。
    リストには、その要素数とちょうど同じくらいの数のサンクがあり、これが場合によってリストが遅くなる原因である。
    遅延 bytestring では別の方法が採用されている。遅延 bytestring は、64 KB（キロバイト）のチャンク（chunk）という塊に格納されるのである。
    そして、遅延 bytestring を評価したら（出力するなど）、最初の 64 KB が評価される。
    残りのチャンクはプロミスである。遅延 bytestring は、64 KB の正格 bytestring からなるリストだとも考えられる。
    ファイルを遅延 bytestring で処理するときにはチャンク単位で読み込まれる。
    これが巧妙なのは、メモリ使用量の上昇を抑えつつ、しかも 64 KB は CPU の L2 キャッシュ（L1 キャッシュより下位のキャッシュメモリ）にきちんとフィットするサイズになっているところである。

    Data.ByteString.Lazy のドキュメントをみると、Data.List の関数と同じ名前の関数がたくさん見つかるだろう。
    これらは [a] の代わりに ByteString を、a の代わりに word8 を受け取る。
    それらの関数はリストに対して動作する関数に似ている。
    名前が同じなので、スクリプトでは修飾付きインポートし、しかるのちに GHCi にロードして bytestring を楽しむこととしよう。
        import qualified Data.ByteString.Lazy as B
        import qualified Data.ByteString as S
    こうすれば、B が遅延 bytestring の型と関数、S が正格 bytestring の型と関数になる。ここではもっぱら遅延版を扱う。

        B.pack :: [GHC.Word.Word8] -> B.ByteString

    という型シグネチャを持つ、 (B.)pack という関数がある。これは (GHC.Word.)Word8 のリストを受け取り、ByteString を返す。
    遅延するリストを受け取り、それよりは怠け者でない 64 KB 間隔で遅延する bytestring を作る関数だと考えればよいだろう。

    Word8 型は Int に似ているが、これは 8 ビット符号無し整数を表す。
    つまり、0 から 255 という、より小さい範囲の数である。Int と同様に Num 型クラスのインスタンスである。
    例えば、5（といった整数）は多相的であらゆる数値的な型として振る舞えるが、Word8 としても振る舞えるということである。

    数から bytestring にパックする方法を示す。
-}

packed :: B.ByteString
packed = B.pack [99, 97, 110] -- Output: "can"

packed' :: B.ByteString
packed' = B.pack [98..120] -- "bcdefghijklmnopqrstuvwx"

--- === 🧐 === ---
{-
    いろいろわかりづらいが、ここまでの話の流れをまとめると、、
    「（とりわけファイル操作などにおいて）ファイルの中身を文字列として扱うと、（文字列は Char の "リスト" だから遅延評価されまくって）速度出ないよね」っていう問題意識からスタートした。
    「文字列 String の代わりに B.ByteString（や S.ByteString）っていう、パフォーマンスを出しやすい型があるから紹介するよー」
    という流れ。
    だから ByteString 型は、お気持ち的な理解としては「String 型っぽいもの」。
    で、数字（Word8）から ByteString を構成することもできるよということ。でも、この下で出てくる「bytestring を使ったファイルのコピー」の例などから見て取れるように、
    数字（word8）から ByteString を構成するってあまりないんじゃないのかな？（わからないけど）
    B.readFile とかを使えば、ファイルの内容をそのまま B.ByteString 型として読めてしまうようだし。
-}

{-
    数が Word8 であることを明示する必要はない。数がその型になることは、型システムによって強いられるからである。
    もし 336 のような大きな数を扱おうとすると、オーバーフローして 80 になる。

    bytestring を 1 バイトずつイラベル必要がある場合は unpack する。unpack 関数は pack 関数の逆関数である。
    bytestring を受け取り、バイトのリストを返す。
-}

by :: B.ByteString
by = B.pack [98, 111, 114, 116] -- `by` : "bort"

unpacked :: [GHC.Word.Word8]
unpacked = B.unpack by -- `unpacked` : [98,111,114,116]

{-
    正格 bytestring と遅延 bytestring を相互に変換することもできる。
    toChunks 関数は、遅延 bytestring を受け取り、それを正格 bytestring のリストに変換する。
    fromChunks 関数は、正格 bytestring のリストを受け取り、それを遅延 bytestring に変換する。
-}

chunks :: B.ByteString
chunks = B.fromChunks [S.pack [40, 41, 42], S.pack [43, 44, 45], S.pack [46, 47, 48]] -- "()*+,-./0"

{-
    小さな正格 bytestring がたくさんあり、それらを連結せずにメモリ上で 1 つの大きな正格 bytestring として効率的に処理したい場合には、
    このような変換を行うとよいだろう。
    bytestring 版の : は cons である。これはバイト値と bytestring を受け取り、そのバイト値を先頭にくっつける。
-}

consed :: B.ByteString
consed = B.cons 85 $ B.pack [80, 81, 82, 84] -- "UPQRT"

{-
    bytestring モジュールには、Data.List などで提供されている関数によく似た関数がかなりある。
    head, tail, init, null, length, map, reverse, foldl, foldr, concat, takeWhile, filter などなど。
    bytestring パッケージのドキュメント http://hackage.haskell.org/package/bytestring/ にすべての関数が載っている。
    ※ ちなみに concat の使い方は次のとおり。
    concat [[1,2],[3,4]]
    > [1,2,3,4]

    bytestring モジュールには、System.IO モジュールが提供する関数と同様の動作をする関数もある。
    これらは String の代わりに ByteString を受け取る。
    例えば、System.IO の readFile 関数は次の型を持っている。

        readFile :: FilePath -> IO String

    bytestring モジュールの readFile 関数は次の型を持つ。

        readFile :: FilePath -> IO ByteString

    > 正格 bytestring を使ってファイルを読もうとすると、そのファイルの内容すべてがメモリ上に一度に読まれる。
    > 遅延 bytestring なら、こぢんまりとしたチャンクごとに読まれる。
-}

--- ¶　bytestring を使ったファイルのコピー
{-
    コマンドライン引数から 2 つのファイル名を受け取って、1 つ目のファイルを 2 つ目のファイルにコピーするプログラムを作ろう。
    System.Diretory には copyFile という関数がすでにあるが、とにかく独自のファイルコピー関数を実装してみよう。

    ```/io/bytestring.hs
    import System.Environment
    import System.Directory
    import System.IO
    import Control.Exception
    import qualified Data.ByteString.Lazy as B

    main = do
        (fileName1:fileName2:_) <- getArgs
        copy fileName1 fileName2

    copy source dest = do
        contents <- B.readFile source
        bracketOnError
            (openTempFile "." "temp")
            (\(tempName, tempHandle) -> do
                hClose tempHandle
                removeFile tempName)
            (\(tempName, tempHandle) -> do
                B.hPutStr tempHandle contents
                hClose tempHandle
                renameFile tempName dest)
    ```

    はじめに main でコマンドライン引数を取得したら、copy 関数（組み込みの関数ではなく、自分で定義するもの）を呼び出すだけ。
    単にファイルを読みおk味、もう片方のファイルに書き込めばコピーはできるが、何かまずいことが起こると（コピーするのにディスクの空きが足りないなど）、おかしなファイルが残ってしまう。
    そこで、いったん一時ファイルに書き込むことにする。何かおかしなことが怒っても、そのファイルを消すだけで済む。

    最初に、B.readFile を使ってコピー元ファイルの内容を読む。それから bracketOnError を使ってエラーハンドラをセットアップする。
    リソースの獲得は　openTempFile "." "temp" である。一時ファイルの名前とそのハンドルのタプルが返される。
    次に、エラーが怒ったときに何をすべきかを書く。
    まずいことが起こったら、ハンドルを閉じ、一時ファイルを削除する。
    最後がコピー処理の本体。B.hPutStr を使って内容を一時ファイルに書き出す。
    一時ファイルを閉じて、それをコピー先ファイル名に変更したら、望みの処理が完了である。

    readFile と hPutStr の代わりに B.readFile と B.hPutStr を使っただけ、というのがポイントである。
    ファイルを開いたり閉じたり、ファイル名を変更したりするのに、bytestring 用の特別な関数を使っていない。
    bytestring の関数を必要とするのは読み書きのときだけである。

    bytestringを使わないプログラムも、だいたい同じような感じになる。
    唯一の違いはやはり readFile と writeFile の代わりに B.readFile と B.writeFile を使っていることだけである（原文ママ。writeFile なんて出てきていないけど）。
    普通の文字列を使ったプログラムを bytestring を使った物に置き換えるには、多くの場合、単に修飾付きインポートして対応する関数の前にモジュール名を付け足すだけ。
    文字列に対して動作する関数を bytestring で動作するように書き換える必要が出てくることはあるが、難しくはない。

    たくさんのデータを文字列として読み込むプログラムで、より良いパフォーマンスを必要とするなら、bytestrin を試してみよう。
    とても少ない労力でパフォーマンスを向上できるチャンスである。
-}
