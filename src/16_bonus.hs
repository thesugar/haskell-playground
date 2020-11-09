---_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/ ---
--   付録　マルチバイト文字列処理に関する補足　　    --
---_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/ ---

-- 文字列を扱う処理を bytestring で書き直すのは比較的簡単だと言われているが、特にマルチバイト文字を扱うにあたっては、いくつか気をつけなければならないところがある。

--- 🐶🔠文字コードと text
{-
    bytestring はその名が示すとおり、バイト列からなる文字列である。一方 String は、Char のリストである。
    Char はバイトよりも広い範囲の文字、具体的にはすべてのユニコード文字を表現できる（4 バイトを用いて表現される）。
    そのため、日本語のような 1 文字が 1 バイトに収まらない文字を含むテキストを扱う場合、そのまま bytestring に書き換えると問題が生じる。

    1 つは文字のエンコードの問題である。多倍長文字列をファイルに書き出す、あるいはハンドルから入出力する際には、バイト列としてエンコードされる必要があるが、
    そのためのエンコーディング（shift-jis, euc-jp, あるいは utf-8 など）を正しく扱う必要がある。
    エンコードされたバイト列をデコードせずにそのまま扱うこともできるが、その場合、多倍長文字列を正しく扱うのは非常に大変である。

    String とほぼ同じ扱い方ができる高速なライブラリとして、text というパッケージがある。
    このパッケージは、効率の良い文字列型 Text を提供している。Text の内部表現はバイト列ではなく、（現在の実装では）UTF-16 でエンコードされた 16 ビット列になっているので、簡単にユニコード文字列を扱える。
    UTF-16 でエンコードされているものの、API としては（オンザフライでデコードを行い）ユニコード文字を扱うものになっているので、それを意識する必要はない。
    文字列長、サロゲートペアなどは自動的に正しく扱われる。
    text も bytestring と同じく正格版と遅延版がある。遅延版だとチャンクサイズごとに処理される。
    -}

--- 🐱🍛OverloadedStrings 拡張
{-
    もう 1 つの問題は、（ダブルクオートで囲まれた）文字列リテラルが String の値だということである。
    bytestring/text とともに文字列リテラルを使おうとすると、型を合わせるために、String を bytestring/text に変換（Data.ByteString.pack、unpackという関数がある）、または
    逆の変kなを行うコードが至る所に入ることになる。これはとても面倒！

    実は GHC には、文字列リテラルを数値のように多相的な定数として扱う機能がある（5 が Int にも Integer にも Double にもなるように文字列もいろんな型として扱える機能）。
    既存のコードがコンパイルエラーにならぬように、コンパイラオプションで切り替えるようになっている。利用するには ghc コマンドに -XOverloadedStrings を渡すか、
        {-# LANGUAGE OverloadedStrings #-}
    という行をコードの先頭に追加する。
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

main = do
    txt <- T.getContents
    let out = T.unlines . map (T.append "☆") . T.lines $ txt
    T.putStr out

{-
    これは標準入力からテキストを入力して、各行の先頭に "☆" と追加して出力するプログラムである。
    " ☆ " がここでは String でなく、Text として扱われている。

    でも以下のように書いても、stack runhaskell src/16_bonus.hs を実行して、標準入力に文字を入力したときの結果は変わらなく見えるけどなー。
    仕様が変わったのか？

    ```
    main = do
        txt <- getContents
        let out = unlines . map (mappend "☆") . lines $ txt
        putStr out
    ```

    パターンマッチに多相的文字列を用いることもできる。
-}

wordToInt :: T.Text -> Int
wordToInt "ひとつ" = 1
wordToInt "ふたつ" = 2
wordToInt "みっつ" = 3
wordToInt _ = 0

testes :: Int
testes = wordToInt "ひとつ" -- 1

{-
    非 ASCII 文字を含むコードをファイルに保存する場合は、UTF-8 で保存するようにすること。
-}

--- 🔭🐟ViewPatterns 拡張
{-
    bytestring/text と一緒に用いると便利なものに、ViewPatterns という GHC 拡張がある。
    次のコードは文字列を画面に表示するプログラムである。
    text の uncons 関数は、与えられた文字列が空でなければ先頭の文字列と残りの文字列のペアを Just で包んだものを、空のときは Nothing を返す関数である。

    ```
    putText :: T.Text -> IO ()
    putText txt =
        case T.uncons txt of
            Just (x, xs) -> do
                putChar x
                putChar xs
            Nothing ->
                return ()
    ```

    String で同じようなことをするなら、(:) でのパターンマッチを使ってとてもシンプルに書けた。
    bytestring/text でも、ViewPatterns というものを使って似たようなことができるようになる。
-}

-- {-# LANGUAGE ViewPatterns #-} コードの先頭行に

putText :: T.Text -> IO ()
putText (T.uncons -> Just (x, xs)) = do
    putChar x
    putText xs -- テキストだと putStr' ってなってるけど putStr' ってなに？
putText _ = return ()

-- GHCi で動かすときは `stack ghci src/16_bonus.hs --ghci-options -XOverloadedStrings --ghci-options -XViewPatterns` としてロードして putText "hoge" とかすればよい。

{-
    ViewPatterns はネストして使うこともできる。
    以下は、あなたの悩みの種を入力すると明るく励ましてくれるプログラム。

        {-# LANGUAGE OverloadedStrings #-}
        {-# LANGUAGE ViewPatterns #-}
        import qualified Data.Text.Lazy as T
        import qualified Data.Text.Lazy.IO as T

        main' :: IO ()
        main' = do
            yourWorry <- T.getLine
            T.putStrLn $ encourage yourWorry
            main'

        isNegative :: Char -> Bool
        isNegative = (`elem` " 非不未無 ")

        encourage :: T.Text -> T.Text
        encourage (T.uncons -> Just ((isNegative -> True), xs))
            = T.append xs "!!"
        encourage xs = xs
-}