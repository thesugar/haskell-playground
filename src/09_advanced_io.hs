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


