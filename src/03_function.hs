{-# OPTIONS -Wall -Werror #-}

---_/_/_/_/_/_/_/_/_/_/_/_/---
--        関数の構文       --
---_/_/_/_/_/_/_/_/_/_/_/_/---

{- この章では、Haskell の関数を書くための構文を見ていく。値を手軽に分解する方法、
　　大きな if/else の連鎖を避ける方法、計算の中間データを一時的に保存して複数回利用する方法を見ていく。 -}

-------------------------------
-- パターンマッチ
-------------------------------

{-関数を定義する際にパターンマッチを使って、関数の本体を場合分けできる。-}

-- 例. 渡された数が 7 かどうか調べる関数

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky _ = "Sorry, you're out of luck, pal!"
    -- lucky を呼ぶと、パターンが上から下の順で試される。
    -- 渡された値が指定されたパターンに合致すると、対応する関数の本体が使われ、残りの部分は無視される。
    -- 最初のパターンに合致するのは 7 が渡されたときだけ。
    -- 2 つ目のは（引数が Int であれば）何にでも合致するパターンで、引数は　**x に束縛** される。

{-
    パターンに（7のような）具体的な値でなく、小文字から始まる名前（x、y, myNumber など）を書くと、任意の値に合致するようになる。
    このパターンは与えられた値につねに合致し、その値をパターンに使った名前で参照できるようになる。
-}

{-    上記の例であれば if 式を使っても簡単に実装できる。では……     -}

-- 1 から 5 が入力されたときはそれを単語として出力し、それ以外なら "Not between 1 and 5" と出力するような関数
sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe _ = "Not between 1 and 5"

-- n の階乗（n!）を再帰的に定義すると？ // 再帰的でない計算は product [1..n] として定義できる
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName _ = "Other than a, b, or c"

{-　【タプルのパターンマッチ】　-}

---- 2 つの 2 次元ベクトル（ペアで表す）を受け取り、それらを足し合わせる関数を書きたいとする。

-- 🤔 パターンマッチを使わない書き方
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

-- 🙆‍♀️ パターンマッチを使う書き方
addVectors':: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, y1) (x2, y2) = (x1 + y1, x2 + y2)

-- 他にも、パターンマッチを使ってたとえば fst, snd（それぞれペアの最初の要素、2 番目の要素を取り出す）のトリプル版を実装できる。
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z


{- 【リストのパターンマッチとリスト内包表記】 -}

-- リスト内包表記でもパターンマッチが使える。
xs :: [(Int, Int)]
xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
sumXs :: [Int]
sumXs = [a+b | (a, b) <- xs] -- パターンマッチ使用 // 結果は [4,7,6,8,11,4]

-- リスト内包表記のパターンマッチでは、失敗したら単に次の要素に進み、失敗した要素は結果のリストには含まれない
hoho :: [Int]
hoho = [x*100+3 | (x, 3) <- xs] -- [103,403,503] // つまり、(x, 3) にマッチする (1, 3), (4, 3), (5, 3) のみが使われた

{-
    🔥普通のリストもパターンマッチで使える。空リスト []、または : を含むパターンと合致させることができる。（[1, 2, 3] は 1:2:3:[] の構文糖衣）。
    🔥x:xs というパターンは、リストの先頭要素を x に束縛し、リストの残りを xs に束縛する。リストの要素がちょうど 1 つだった場合、xs には空のリストが束縛される。
-}

-- 独自の head 関数を head' という名前で実装してみよう。

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!" -- エラーの定義のしかた。error 関数は文字列を引数に取り、その文字列を使ってランタイムエラーを生成する。
head' (x:_) = x -- (x:_) は括弧で括らないとエラー。　👍複数の変数に束縛したい場合は丸括弧で囲む！

-- もうひとつの例。リストの要素を回りくどくて不便な書式で出力する関数。
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x: []) = "The list has one element: " ++ show x
tell (x:y: []) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first two elements are: " ++ show x ++ " and " ++ show y -- このパターンは長さが 2 以上の任意のリストに合致する。

---- 💡リストに対するパターンマッチの注意点: ++ 演算子（2 つのリストをつなげる演算子）は使えない

{- 【as パターン】 -}
    -- as パターンは、値をパターンに分解しつつ、パターンマッチの対象になった値自体も参照したいときに使う
    -- as パターンを作るには、普通のパターンの前に名前と @ を追加する

-- 例えば、xs@(x:y:ys) のような as パターンを作れる。
-- このパターンは、x:y:ys に合致するものとまったく同じものに合致しつつ、xs で元のリスト全体にアクセスできる。

-- 例
firstLetter ::  String -> String
firstLetter "" = "Empty string, Whoops!"
firstLetter sentence@(x:_) = "The first letter of " ++ sentence ++ " is " ++ [x]


-------------------------------
-- 場合分けして、きっちりガード！
-------------------------------

{-
    関数を定義する際、**引数の構造で** 場合分けするときにはパターン（マッチ）を使う。
    **引数の値が満たす性質で場合分けするとき** には、ガードを使う。

    性質で場合分けするというと if 式っぽいし、実際に if とガードは似ているが、
    複数の条件があるときにはガードのほうが if より可読性が高く、パターンマッチとの相性も抜群である。
-}

-- 例. 肥満度指数（BMI）によって異なる叱り方をする関数を定義する。
--     BMI とは、体重（kg）を身長（m）の 2 乗で割った値。

bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. \
                    \ Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!" -- すべてをキャッチする otherwise

{-
    ⭐️ ガードには、パイプ文字（|）とそれに続く真理値式、さらにその式が True に評価されたときに使われる関数の本体が続く。
    　 式が False に評価されたら、その次のガードの評価に移る。この繰り返しである。
    ⭐️　ガードは最低でも 1 つのスペースでインデントする必要がある。
    ⭐️ すべてのガードが False に評価されて、最後に全部をキャッチする otherwise もなかったら、評価は失敗して **次のパターンに** 移る。
    　　👉　パターンとガードの相性がいい！
    　　適切なパターンが見つからなければエラーが投げられる。
-}

-- もちろん、ガードは複数の引数を取る関数にも使える。bmiTell 関数を身長と体重を受け取るように変更して、BMI の計算もするように変更しよう。

bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | weight / height ** 2 <= 18.5 = "You're underweight, you emo, you!" -- テキストだと ^ 2 としているが ** 2としないとエラーになる。。
    | weight / height ** 2 <= 25.0 =  "You're supposedly normal. \
                    \ Pffft, I bet you're ugly!"
    | weight / height ** 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"

-- もう一つシンプルな例として、独自の max 関数を定義してみよう。引数を 2 つ取って大きい方を返す。
max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b = b
    | otherwise = a

-- compare 関数もガードを使って実装できる。
compare' :: (Ord a) => a -> a -> Ordering
compare' a b -- ここは a `compare'` b と書いてもよい
    | a < b = LT
    | a == b = EQ
    | otherwise = GT


-- [参考] swift ではガードは `guard let someVariable = hoge else { return } ` というような形で使い、nil チェック（hoge が nil なら処理を進めない）に使われる。

-------------------------------
-- where ?!
-------------------------------

-- 上記のコードでは BMI 計算を 3 回繰り返した。この値を where キーワードを使って変数に束縛して、繰り返しを避けよう。

bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. \
                    \ Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ** 2
        -- where キーワードをガードの後に置いて、1 つもしくは複数の変数や関数を定義できる。
        -- それら変数や関数の名前はどのガードからも見える。
        
-- もっと外に出しても OK。
bmiTell''' :: Double -> Double -> String
bmiTell''' weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. \
                    \ Pffft, I bet you're ugly!"
    | bmi <= fat = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where   bmi = weight / height ** 2
            skinny = 18.5
            normal = 25.0
            fat = 30.0 -- where ブロックのインデントがずれているとエラーになる。

---- ¶ where のスコープ

{-
    where 節で定義した変数は、その関数からしか見えないので、他の関数の名前空間を汚染する心配はない。
    複数の異なる関数から見える必要のある変数を定義したい場合は、グローバルに定義する必要がある。

    また、where による束縛は関数の違うパターンの本体では共有されない。
-}

-- 例えば、名前を引数に取り、その名前を認識できた場合には上品な挨拶を、そうでなければ下品な挨拶をする関数を考えてみる。

{-
    greet :: String -> String
    greet "Juan" = niceGreeting ++ " Juan!"
    greet "Fernando" = niceGreeting ++ " Fernando!"
    greet name = badGreeting ++ " " ++ name
        where   niceGreeting = "Hello! So very nice to see you,"
                badGreeting = "Oh! Pfft. It's you."

    🛑　上の greet 関数は意図したとおりには動かない。
    🛑　where の束縛は違うパターンの関数本体で共有されず、where の束縛での名前は最後の本体からしか見えないからである（greet "Juan" や greet "Fernando" から niceGreeting は見れない）
    　👉この関数を正しく動くようにするには、badGreeting と niceGreeting は次のようにグローバルに定義しなくてはならない。
-}

badGreeting :: String
badGreeting = "Oh! Pfft. It's you."
niceGreeting :: String
niceGreeting = "Hello! So very nice to see you,"

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name

---- ¶　パターンマッチと where

{-
    where の束縛の中でもパターンマッチを使うことができる。
    BMI 関数の where 節を次のように書いてもよい。

    ```
    where   bmi = weight / height ** 2
            (skinny, normal, fat) = (18.5, 25.0, 30.0)
    ```
-}

-- 上記のテクニックの例として、ファーストネームとラストネームを受け取ってイニシャルを返す関数を書いてみよう。

initials :: String -> String -> String

-- ↓でもいいんだろうけど、まったく上記テクニックの例になってないので。。
-- initials firstName lastName = head firstName:[] ++ head lastName:[]

initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
    where   (f:_) = firstName
            (l:_) = lastName
    -- もちろん、引数のところで直接パターンマッチしてもよい（し、そのほうが可読性も高くはなりそう。今回は一例。）

---- ¶ where ブロックの中の関数

{-
    where ブロックの中では定数だけでなく関数も定義できる。
    体重と身長のペアのリストを受け取って BMI のリストを返す関数を作ってみよう。
-}

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xxs = [bmi weight height | (weight, height) <- xxs]
    where bmi w h = w / h ** 2
    
    -- この例で bmi を関数として導入したのは、calcBmis 関数の引数に対して 1 つの BMI を計算するのではなく、
    -- 関数に渡されたリストの要素それぞれに対して、異なる BMI を計算する必要があるから。

-------------------------------
-- let It Be
-------------------------------

{-
    let 式は where 節によく似ている。
    where は関数の終わりで変数を束縛し、その変数は ガードを含む関数 全体から見える（ガード間で共有される。なお、違うパターン間では共有されない）。
    それに対して let 式は、どこでも変数を束縛でき、そして let 自身も式になる。
    しかし、let 式が作る束縛は局所的で、ガード間で共有されない。
    束縛を行う Haskell の他の構文と同じく、let 式でもパターンマッチが使える。
-}

-- 円柱の表面積を高さと半径から求める関数
cylinder :: Double -> Double -> Double
cylinder r h = 2 * pi * r * h + 2 * pi * r ** 2

-- これを、練習のためにあえて let を使って書き換えてみる。
cylinder' :: Double -> Double -> Double
cylinder' r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ** 2
    in sideArea + 2 * topArea

{-
    ⭐️ let 式は `let <bindings> in <expression>` という形を取る。
    ⭐️ let で定義した変数は、その let 式全体から見える。
-}

{-
    💭let と where の違いは❓
        ✅ let は束縛を先に書いて式を後ろに書くけど、where はその逆（表面的な違い）。
        ✅ let 式はその名のとおり「式」で、where 節はそうではない。
        　　👉 何かが「式である」とは、それが値を持つということ。"Boo!"　は式、 3 + 5 も head [1, 2, 3] も式。
        　　👉 つまり、let 式は次のようにコード中のほとんどどんな場所でも使えるということ。
                4 * (let a = 9 in a + 1) + 2 // 結果は 42 になる
-}

-- ✅ let はローカルスコープに関数を作るのに使える。
aa :: [(Int, Int, Int)]
aa = [let square x = x * x in (square 5, square 3, square 2)] -- [(25,9,4)]

-- ✅ let ではセミコロン区切りを使える。これは複数の変数を 1 行で束縛したいときに便利。間延びした構文を使わずに済む。
bb :: (Int, String)
bb = (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar="there!" in foo ++ bar) -- (6000000,"Hey there!")

-- ✅ let 式とパターンマッチがあれば、あっという間にタプルを要素に分解してそれぞれ名前に束縛できる。
cc :: Int
cc = (let (a, b, c) = (1, 2, 3) in a+b+c) * 100

{-
　💭こんなに使い勝手がよいのであればいつも let 式を使えばよい？
    - let 式は「式」であり、そのスコープに局所的なので、ガードをまたいでは使えない。
    - 関数の前ではなく後ろで部品を定義するのが好きだから where を使うという人もいる。
        - そのほうが関数の本体が名前と型宣言に近くなるので、コードが読みやすくなる。
-}

---- リスト内包表記での let

-- 体重と身長のペアのリストを計算する先ほどの例を、where で関数を定義するのではなく、リスト内包表記中の let を使って書き換えてみよう。
calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' info = [bmi | (w, h) <- info, let bmi = w / h ** 2]

{-
    リスト内包表記が元のリストからタプルを受け取り、その要素を w と h に束縛するたびに let 式は w / h ** 2 を変数 bmi に束縛する。
    それから bmi をリスト内包表記の出力として書き出している。
    ここでは in は使っていない。

    リスト内包表記の中の let を述語のように使っているが、これはリストをフィルタするわけではなく、名前を束縛しているだけ。
    let で定義された名前は、出力（| より前の部分）とその let より後ろのリスト内包表記のすべてから見える。

    このテクニックを使うと、次のように、肥満な人の BMI のみを返すように関数を変えることができる。
-}

calcBmisForFat :: [(Double, Double)] -> [Double]
calcBmisForFat info = [bmi | (w, h) <- info, let bmi = w / h ** 2, bmi > 25.0 ]

-- リスト内包表記の (w, h) <- info の部分は **ジェネレータ** と呼ばれる。
-- let の束縛よりも前に定義されているので、変数 bmi はジェネレータからは参照できない。

-------------------------------
-- case 式
-------------------------------

{-
    case 式を使うと、変数の指定した値に対するコードブロックを評価できる。
    要するに case 式は、コード中のどこでもパターンマッチが使える構文。多くの言語にも似たような case "文" があるので馴染み深い。
    ただ、Haskell の case 式はその一歩先をいくものである。
    if/else　式や let 式と同じく　case も式である。さらに、 case 式では変数の値に基づいて評価を分岐させるだけでなく、パターンマッチも使える。

    値を取り、パターンマッチし、その結果に基づいてコード片を評価するという点において、case 式は関数の引数に対するパターンマッチとかなり似ている。
    実のところ、それ（関数の引数に対するパターンマッチ）はcase 式の構文糖衣になっている。
-}

-- 例えば、以下の 2 つのコードはまったく同じで、交換可能。

-- 1
myhead :: [a] -> a
myhead [] = error "No head for empty lists!"
myhead (x:_) = x

-- 2
myhead' :: [a] -> a
myhead' arg = case arg of [] -> error "No head for empty lists!"
                          (x:_) -> x

{-
📜 case 式の構文
    case expression of  pattern -> result
                        pattern -> result
                        pattern -> result
                        ...

    式に合致した最初のパターンが使われる。case 式に合致するパターンが見つからなかった場合、ランタイムエラーが発生する。
    引数によるパターンマッチが使えるのは関数を定義するときだけだが、case 式はどこでも使える。

    例えば、式の途中でパターンマッチをするのにも使える。
-}

describeList :: [a] -> String
describeList ls = "The list is " -- case 式は値を表現しているので、文字列 "The list is " と case 式に対して ++ が利用できる。
                ++ case ls of   [] -> "empty."
                                [_] -> "a singleton list."
                                _ -> "a longer list." 

-- 関数定義のパターンマッチは case 式と同じように使えるので、describeList は以下のようにも定義できる。
describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
    where   what [] = "empty."
            what [_] = "a singleton list."
            what _ = "a longer list."