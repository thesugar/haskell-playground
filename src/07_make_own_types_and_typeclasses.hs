{-# OPTIONS -Wall -Werror #-}

import qualified Data.Map as Map

---_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/---
--   　型や型クラスを自分で作ろう　    --
---_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/---

-------------------------------
-- 新しいデータ型を定義する
-------------------------------

{-
    自作のデータ型を作る方法の 1 つは data キーワードを使うことです。
    まずは、標準ライブラリで Bool 型の定義がどうなっているか見てみよう。

        data Bool = False | True

    このように data キーワードを使うと、新しいデータ型を定義する構文になる。
    等号の前の部分は型の名前、ここでは Bool を表す。
    等号の後の部分は値コンストラクタである。

    値コンストラクタは、この型が取りうる値の種類を指定している。記号 | は「または」の意味。
    というわけで、この型宣言は「Bool 型は True または　False の値を取りうる」と読める。

    型名と値コンストラクタはどちらも大文字で始まる必要がある。

    同様に、Int 型はこのように定義されているとみなせる。

        data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647

    最初と最後の値は Int が取りうる最小値と最大値である（実際の Int はこのように定義されているわけではない）。

    さて、Haskell で図形を表すにはどうする？
    タプルを使うという手がある。例えば、円は (43.1, 55.0, 10.4) と表せる。
    1 つ目と 2 つ目が縁の中心座標で、3 つ目が縁の半径。
    この表記の問題点は、三次元ベクトルみたいな、実数の 3 つ組で指定できる任意のものを表せてしまうということである。

    より良い解決策は、図形を表す型を自作することであろう。
-}

-------------------------------
-- 形づくる
-------------------------------

{-
    ここでは、長方形と円という 2 種類の図形を扱うことにしよう。以下のようなやり方が考えられる。
-}

{- 下部で再定義するため、ここではコメントアウトしておく
data Shape = Circle Float Float Float |
             Rectangle Float Float Float Float
-}

{-
    解釈：Circle 値コンストラクタには、浮動小数を受け取るフィールドが 3 つある。
    このように、値コンストラクタを書くときは後ろに型を付け足すことができ、それらは値コンストラクタに与える引数の型になる。

    ここでは、最初の 2 つのフィールドは円の中心の座標で、3 つ目のフィールドは円の半径である。
    一方、Rectangle 値コンストラクタには浮動小数を受け取るフィールドが 4 つある。
    最初の 2 つは左下の角、後の 2 つは右上の角の座標を表す。

    実のところ値コンストラクタは、最終的にそのデータ型の値を返す関数なのである。
    これら 2 つの値コンストラクタの型シグネチャを見てみよう。

    :t Circle
    > Circle :: Float -> Float -> Float -> Shape

    :t Rectangle
    > Rectangle :: Float -> Float -> Float -> Float -> Shape

    というわけで、値コンストラクタはなんの変哲もない関数である。
    データ型にあったフィールドは、値コンストラクタ関数にとっての引数に対応する。

    では、Shape を引数に取って、その面積を返す関数を作ってみよう。
-}

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ (2 :: Int)
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

{-
    型宣言に注目。
    area は、Shape を取って Float を返す関数だよ、と言っている。
    ここに例えば Circle -> Float という型宣言を書くことはできない。なぜなら Shape は型だけど Circle は型でなく値コンストラクタであるから。
    （True -> Int のような型宣言を持つ関数が書けないのと同じ）

    次に、コンストラクタはパターンマッチに使えることに注目。
    これまでにも、[]、False、5 といった値に対するパターンマッチは行ってきた。
    しかし、今までの値にはフィールドがなかった。フィールドがある値コンストラクタでパターンマッチを行いたい場合、
    まず値コンストラクタ名を書き、それからフィールドを名前に束縛する。
    円の場合、必要なのは半径の情報だけで、円の位置を表す最初の 2 つのフィールドはいらないので、_ で捨てている。
-}

circleArea :: Float
circleArea = area (Circle 10 20 10) -- もちろん area $ Circle 10 20 10 と書いてもよい

rectArea :: Float
rectArea = area (Rectangle 0 0 (-100) (-100))

{-
    ここで、Circle 10 20 5 をプロンプトから表示しようとするとエラーになる。
    これは、Haskell が今作ったデータ型を文字列にして表示する方法をまだ知らないからである。
    プロンプトからなんらかの値を表示しようとした場合、Haskell はまずその値に show 関数を適用して文字列表記を得て、それを端末に表示する。

    この Shape 型を Show 型クラスの一員にするには、型宣言を以下のように修正する。
-}


data Shape = Circle Float Float Float |
             Rectangle Float Float Float Float
    deriving (Show)


{-
    データ宣言の最後に deriving (Show) と書けば、Haskell が自動的にこの型を Show 型クラスのインスタンスにしてくれる。
    これで Shape 型の値を表示できる。

        Circle 10 20 5
        > Circle 10.0 20.0 5.0

        Rectangle 50 230 60 90
        > Rectangle 50.0 230.0 60.0 90.0

    値コンストラクタは関数であるから、普通に map したり、部分適用したりできる。
    たとえば、半径だけ違う同心円のリストを作りたかったら、以下のように書ける。
-}

circles :: [Shape]
circles = map (Circle 10 20) [4, 5, 6, 6] -- [Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]

--- ¶　Point データ型で形を整える
{-
    ここまで、いい感じにデータ型を自作できたが、もっとよくできる。
    二次元空間の点を表す中間データ構造を作ろう。
    そうすれば図形をもっとわかりやすくできる。
-}

data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)

{-
    点（Point）を定義するとき、データ型と値コンストラクタに同じ名前を使った。ここがポイントである。
    同じ名前にしておくことに特別な意味はないのだが、値コンストラクタが 1 つしかないデータ型はそうするのが慣例。
    というわけで、新しい Circle には 2 つのフィールドがある。
    1 つはPoint 型で、もう 1 つは Float 型。これで、どのフィールドが何なのかわかりやすくなった。
    Rectangle も同じことである。
    では、area 関数にもこの変化を反映させよう。
-}

area' :: Shape' -> Float
area' (Circle' _ r) = pi * r ^ (2 :: Int)
area' (Rectangle' (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

{-
    変更する必要があったのはパターンだけであった。Circle パターンでは、点全体を無視している。
    Rectangle パターンでは、パターンマッチをネストすることで、一気に点が属するフィールドへアクセスしている。
    改良したバージョンを試してみよう。
-}

rectArea' :: Float
rectArea' = area' (Rectangle' (Point 0 0) (Point 100 100)) -- 10000.0

circleArea' :: Float
circleArea' = area' (Circle' (Point 0 0) 24) -- 1809.5574

{-
図形を動かす関数も定義してみる。
図形と、x 軸方向への移動量、y 軸方向への移動量を取って、元の図形と同じ寸法だけど違う場所にある図形を返す。
-}

nudge :: Shape' -> Float -> Float -> Shape'
nudge (Circle' (Point x1 y1) r ) dx dy = Circle' (Point (x1+dx) (y1+dy)) r
nudge (Rectangle' (Point x1 y1) (Point x2 y2)) dx dy = Rectangle' (Point (x1+dx) (y1+dy)) (Point (x2+dx) (y2+dy))

-- 使用例
nudgedCircle :: Shape'
nudgedCircle = nudge (Circle' (Point 0 0) 12) 5 10 -- Circle' (Point 5.0 10.0) 12.0

{- もし点を直接触りたくないという場合には、指定したサイズの図形を原点に作る補助関数を作って、それから移動させることもできる。　-}

baseCircle :: Float -> Shape'
baseCircle r = Circle' (Point 0 0) r

baseRect :: Float -> Float -> Shape'
baseRect width height = Rectangle' (Point 0 0) (Point width height)

{- これらの関数を使えば、まず座標系の原点に図形を作って、それから望みの場所まで移動させることができる。これで図形を作るのが楽になる。-}

myRect :: Shape'
myRect = nudge (baseRect 40 100) 60 23 -- Rectangle' (Point 60.0 23.0) (Point 100.0 123.0)

--- ¶　Shape をモジュールとしてエクスポートする
{-
    自作のデータ型も自作のモジュールからエクスポートできる。型をエクスポートするには、関数のエクスポートと同じところに型名を書くだけである。
    値コンストラクタをエクスポートしたい場合は、型名の後に括弧を追加し、その中にカンマ区切りで値コンストラクタを書く。
    ある型の値コンストラクタをすべてエクスポートしたい場合は、ピリオド 2 つ（..）を書く。
    図形を扱う関数と型をモジュールからエクスポートしたいとしたら、こう書き始める。

        module Shapes
        ( Point (..)
        , Shape (..)
        , area
        , nudge
        , baseCircle
        , baseRect
        ) where

    Shape (..) と書くことで、Shape のすべての値コンストラクタがエクスポートされる。
    つまり、このモジュールをインポートした人は、Rectangle と Circle の値コンストラクタを使って図形を作れるようになる。

    この行は　Shape (Rectangle, Circle) と書いても同じことだが、(..) を使うほうが簡単。
    それに、エクスポートした型に後から値コンストラクタを追加することにした場合でも、エクスポート文を変える必要がなくなる。
    .. を使えば、その型のすべての値コンストラクタが自動的にエクスポートされるからである。

    一方、Shape のエクスポート文の後に括弧を付けないことで、Shape 型をエクスポートするけれどもその値コンストラクタは一切エクスポートしない、という選択もできる。
    すると、このモジュールのユーザは補助関数 baseCircle と baseRect 経由でしか図形を作れなくなる。

    以下のモジュールを Shapes.hs というファイル名で保存したうえで、

        ```
        module Shapes
        (Point, Shape, area, nudge, baseCircle, baseRect) where

        data Point = Point Float Float deriving (Show)
        data Shape =    Circle Point Float | 
                        Rectangle Point Point deriving (Show)

        area :: Shape -> Float
        area (Circle _ r) = pi * r ^ 2
        area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

        nudge :: Shape -> Float -> Float -> Shape
        nudge (Circle (Point x y) r) dx dy = Circle (Point (x+dx) (y+dy)) r
        nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy = Rectangle (Point (x1+dx) (y1+dy)) (Point (x2+dx) (y2+dy))

        baseCircle :: Float -> Shape
        baseCircle r = Circle (Point 0 0) r

        baseRect :: Float -> Float -> Shape
        baseRect width height = Rectangle (Point 0 0) (Point width height)
        ```

    以下のような Main.hs を作って動かしてみよう。

        ```
        import Shapes

        main = do
            print $ Circle (Point 10 20) 30
        ```

    こうすると、Circle も Point も知りませんよ、というエラーが出る。
    このようにして、Haskell は Shapes モジュールのプライバシーを完璧に守るのである
    （ちなみに、ここの Point は Shapes からエクスポートしている型としての Point ではなく、値コンストラクタとしての Point）。

    一方、Main.hs を次のように書き直せば、Shapes モジュールが許可しているものしか使っていないため、正しく動く。

        ```
        import Shapes
        main = do
            print $ nudge (baseCircle 30) 10 20
        ```

    値コンストラクタは、フィールドを引数に取ってデータ型（たとえば Shape 型）の値を返す関数に過ぎないのだった。
    値コンストラクタをエクスポートしない、という選択をすれば、このモジュールをインポートする人が値コンストラクタを関数として直接使うことを防ぐことになる。
    データ型の値コンストラクタをエクスポートしないことで、データ型の実装を隠し、データ型の抽象度を上げられる。
    また、値コンストラクタを使ったパターンマッチもできなくなる。
    ユーザには必ずモジュールの提供する補助関数（今回の例では baseCircle や baseRect）を使ってデータ型にアクセスしてほしい、という場合には、
    値コンストラクタをエクスポートしないことが望ましい選択になる。
    そうすれば、モジュールのユーザはモジュール内部の詳細を知る必要がなくなるうえ、モジュールの作者にとってもエクスポートした関数の振る舞いが同じであるかぎり内部の実装を自由に変えられるという利点がある。
    （このように、ユーザに必要なだけの操作法を提供するが、実装の詳細は隠されているようなデータ型のことを抽象データ型と呼ぶ）

    Data.Map はこのアプローチを取っている。値コンストラクタから直接 Map を作ることはできない（エクスポートされていないから）。
    だが、Map.fromList のような補助関数を使って Map を作ることは普通にできる。
    おかげで Data.Map の作者たちは、既存のプログラムを壊す心配なく、自由に Map の内部表現を変更できるのである。

    ただ、単純なデータ型に対しては、値コンストラクタをエクスポートするのもなんら問題ない方針である。
-}

-------------------------------
-- レコード構文
-------------------------------

{-
    今度はデータ型を作る別の方法を見ていこう。
    人物を記述するデータ型を作る仕事を任されたとする。
    人物に関しては、その名前、苗字、年齢、身長、電話番号、好きなアイスクリームの味を記録することにする。
    さっそくやってみよう。
-}

--data Person = Person String String Int Float String String
--    deriving (Show)

{- では人物を作ってみよう -}

--guy :: Person
--guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

-- guy
-- > Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

{-
    特定の情報を取り出すにはどうすればよいだろう？
    人物の名前を取り出す関数とか、苗字を取り出す関数とかが必要。
-}

{-
firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age' _ _ _) = age'

tall :: Person -> Float
tall (Person _ _ _ height' _ _) = height'

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number' _) = number'

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor') = flavor'
-}

{-
    こんなコード、書いていても楽しくない！が、この方法で動くことは動く。

        firstName guy
        > "Buddy"

    だが、Haskell にはもう 1 つ、データ型を書くための構文が備わっている。
    それは**レコード構文**である。
-}

data Person = Person {  firstName :: String
                    ,   lastName :: String
                    ,   age :: Int
                    ,   tall :: Float
                    ,   phoneNumber :: String
                    ,   flavor :: String } deriving (Show)

{-
    このようにレコード構文では、フィールドの型名だけをスペース区切りで列挙する代わりに、中括弧を使う。
    まずフィールドの名前（例えば firstName）を書き、次に :: を書き、それから型を書く。
    出来上がったデータ型は以前のものとまったく同じ。
    この幸運の利点は、データ型と同時にフィールドを取得する関数たちが作られることである。
    この型をレコード構文を使って定義したkとで、Haskell は firstName、lastName、age、height、phoneNumber、flavor という
    6 つの関数を自動的に作ってくれるのである。

    たとえばここで :t flavor とすると以下のように出力される。
        flavor :: Person -> String

    レコード構文にはもう 1 つ利点がある。
    Show のインスタンスを自動導出（derive）するとき、レコード構文を使って定義しインスタンス化した型は、そうでない型とは違う表示の仕方になる。
    例えば自動車を表す型があるとしよう。メーカー、モデル名、生産された年を記録したいとする。この型は、レコード構文を使わない場合は以下のように定義できる。
-}

data Car' = Car' String String Int deriving (Show)

{-
    自動車は、例えばこのように表示される。
        Car' "Ford" "Mustang" 1967
        > Car' "Ford" "Mustang" 1967
-}

{-
    では、レコード構文を使って定義した場合は何が起こるのだろうか？
-}

data Car = Car {    company :: String
                ,   model :: String
                ,   year :: Int
                } deriving (Show)

myCar :: Car
myCar = Car {company="Toyota", model="Prius", year=2020}

{-
    今度はこのようにして自動車が作れる。
        Car {company="Ford", model="Mustang", year=1967}
        > Car {company = "Ford", model = "Mustang", year = 1967}

    > 💡  レコード構文は、すでに定義済みの値をもとに、一部のフィールドだけが異なる値を作るのにも使える。
    >     myCar が Car 型の値であるときに、myCar{year=2013} は、myCar と同じ会社の同じモデル名の、2013 年版を表す。

    このときフィールドを元どおりの順番で指定する必要はない。ただし、すべてのフィールドを埋める必要はある。
    一方、レコード構文を使わない場合は、フィールドを順番どおりに指定する必要がある。

    値コンストラクタに複数のフィールドがあり、どのフィールドが何番目なのか紛らわしい場合はレコード構文を使うこと。
    三次元ベクトルのデータ型を `data Vector = Vector Int Int Int` として作った場合は、フィールドがベクトルの成分であることは明らかである。
    一方、Person や Car の例ではフィールドが何を指すのか自明ではないので、レコード構文を使うほうが便利である。
-}

-------------------------------
-- 型引数
-------------------------------

{-
    値コンストラクタは引数を取って新しい値を生み出すのだった。
    たとえば、Car という値コンストラクタは 3 つの値を受け取って自動車を表す値を作る。
    同様に、_型コンストラクタは型を引数に取って新しい型を作る_もの。

    型引数がどう使われるかのイメージを掴むために、すでに知っている型がどんな実装になっているか見てみよう。

        data Maybe a = Nothing | Just a

    この a が型引数である（c.f. someFunc :: [a] -> a のように、型定義の中で使う a のようなものは「型変数」と呼ぶのだった）。
    そして型引数を取っているので、Maybe は型コンストラクタと呼ばれる。
    Nothing でない場合に何をこのデータ型に保持させたいかに応じて、この型コンストラクタから Maybe Int、Maybe Car、Maybe String などの型を作れる。
    単なる Maybe という型の値は存在できない。
    なぜなら、Maybe は型コンストラクタであって、型ではないからである。
    型コンストラクタは、_すべての引数を埋めて初めて何かしらの値の型になれる_。

    Char を Maybe の型引数に渡すと Maybe Char という型が得られる。たとえば Just 'a' という値は Maybe Char という型を持つ。
    ほとんどの場合、型コンストラクタに型引数を渡して作った型を明示的に指定する必要はない。Haskell には型推論があるからである。

    たとえば Just 'a' という値を作ると、Haskell はその型が Maybe Char だと推論してくれる。

    明示的に型を型引数として渡したければ、型の世界で行う必要がある。
    Haskell では、:: 記号の右側が型の世界。
    この手法は、例えば Just 3 に Maybe Int という型を持って欲しい場合に便利である。
    デフォルトでは、Haskell はこの値の型を Just 3 :: Num a => Maybe a と推論するが、明示的な片注釈を使えば、型の制限を少しキツくできる。

        Just 3 :: Maybe Int
        > Just 3

    実は、Maybe を使うより前に型引数を使っていた。それはリスト型である。構文糖衣があるためわかりにくいが、リスト型は 1 つの型引数を取って具体型を生成するのである。
    [Int] 型の値、[Char] 型の値、[[String]] 型の値、などはあるが、[] 型の値というのは作れない。
        💡  具体型とは、型引数を 1 つも取らない型か、あるいは、型引数を取るけれどもそれがすべて埋まっている型のことを指す。
            前者の例は Int や Bool、後者の例は Maybe Char である。なんらかの値があったら、その型はつねに具体型である。

    Maybe 型で遊んでみよう。
-}

--- 以下は、スクリプトに記述するために型宣言も書いてしまっているが、ターミナルを使えば Just "Haha" などと打ち込むだけでよく、
--- :t Just "Haha" とすると :: Maybe [Char] などと出力されるので、Haskell が型推論してくれることがわかる。
haha :: Maybe String
haha = Just "Haha"

eightyFour :: Maybe Int
eightyFour = Just 84

{-
他）
    :t Nothing
    > Nothing :: Maybe a

    Just 10 :: Maybe Double
    > Just 10.0
-}

{-
    型引数が便利なのは、さまざまな型を収納するデータ型を作れるところである。
    たとえば Maybe を定義するのに、中身の型ごとに別々の型にすることもできなくはない。
-}

data IntMaybe = INothing | IJust Int
data StringMaybe = SNothing | SJust String
data ShapeMaybe = ShNothing | ShJust Shape

{- でも、型引数を使ってどんな型の値でも収納できる汎用の Maybe を作ったほうが便利　-}
data Maybe' a = MyNothing | MyJust a

{-
Nothing の型が Maybe a であることに注意。この型は型引数がある（つまり Maybe a の a がある）ので **多相的** である。
もし、Maybe Int を引数に取る関数があれば、それに Nothing を渡すことができる。
Nothing は値を含んでいないので、どのみち問題ないのである。

このように、Maybe a 型の値は、Maybe Int 型が要求される場所でも使える。
ちょうど、5 は Int としても Double としても使えるのと同じことである。
（現に、:t 5 とすると Int とかそういう確定的な型が出力されるのではなく、`5 :: Num p => p` というふうに、型変数を使った表示がされる）

同様に、空リストの型は [a] である。
これが、 [1, 2, 3] ++ [] という使い方も ["ha", "ha", "ha"] ++ [] という使い方も同時にできる理由である。
-}

nothingInMaybeInt :: Maybe Int
nothingInMaybeInt = Nothing

--- ¶　自動車は型引数を取るべきか？
{-
    型引数を使うのがよいのはどういうときだろうか？
    普通、型引数は Maybe a 型のような、どんな型をそこに持ってきても変わらず動作するようなデータ型に使うものである。
    ある型がなんらかの箱のように振舞うなら、型引数を使うとよいだろう。

    Car データ型を上記で以下のように定義したのだった。
        data Car = Car {    company :: String
                        ,   model :: String
                        ,   year :: Int
                        } deriving (Show)

    これを以下のように書き換えてみる。
-}
data Kuruma a b c = Kuruma {  company' :: a
                            , model'  :: b
                            , year' :: c } deriving (Show)

{-
    さて、これにメリットは・・・？　おそらく皆無。
    どうせ Kuruma String String Int を扱う関数しか作らないだろうから。
    例えば、上で定義した（レコード構文の）Car を使えば、自動車の情報を読みやすい形で表示する関数は次のように書ける。
-}

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y})
    = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
    -- これで tellCar myCar とすると、"This Toyota Prius was made in 2020" という出力が得られる。

{-
    では、Car が Car a b c だったらどうなるか？
        tellCar :: (Show a) => Car String String a -> String
        telLCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

    となって、型シグネチャが複雑になる。複雑になったわりに、特にメリットもない（Show 型クラスのインスタンスならなんでも c の箇所に取れるようになったことくらい）
    結論、Car を多相型にしても労力に見合わないということ。

    ふつう、型引数を使うのは、データ型の値コンストラクタに収納された型が、データ型自体の動作にそこまで重要な影響を与えないときである。
    たとえば「物」のリストは、中身の物がなんであれ、とりあえずリストはリストである。
    数のリストの合計を求めたくなったら、特に数のリストを引数に取る総和関数を後から作ればいいわけである。
    Maybe についても同じことが言える。Maybe は、ある物を 1 つ持つか持たないかの選択肢を表しているのであって、そのものが何型であるかとは関係ない。

    これまでに出てきた中では、Data.Map モジュールの Map k v も多層型である。
    k は Map のキーの型で、v は Map の値の型。
    これは型引数が大活躍できている例と言える。
    Map が多相化されていることで、どんな型からどんな型への Map でも作れる。
    もっとも、キーの型のほうは Ord に属しているという条件付きではあるが。

    ところで、Data.Map の作者には、データ型宣言にこうやって型クラス制約を加えるという選択もあっただろう。
        data (Ord k) => Map k v = ...
    しかし Haskell には、_データ宣言には決して型クラス制約を付けない_ という、とても強いコーディング規約がある。
    なぜなら、データ宣言に型クラス制約を付けても大した利益がない割に、型クラス制約をそこら中、必要ない箇所にまで書いてまわる羽目になるからである。
    もし Map k v のデータ宣言に Ord k という型クラス制約があっても、Map のキーが順序づけ可能であることを過程する関数には、やはり型クラス制約を書く必要がある。
    これに対し、データ宣言に型クラス制約がなければ、キーの型が順序づけ可能であることを仮定しない関数からは (Ord k) => を省くことができる。
    その一例が、Map を取って連想リストに変換する関数 toList である。これの型シグネチャは
        toList :: Map k a -> [(k, a)]
    である。
    もし Map k v のデータd宣言に型クラス制約が付いていたら、toList の型も toList :: (Ord k) => Map k a -> [(k, a)] となる必要があっただろう。
    toList 関数自体はキーの順序比較をまったく使わないのに。

    というわけで、このデータ宣言にはこの型クラス制約をつけるのが当然だろう、と思える場合でも、決して型クラス制約を付けないこと。
    どのみち関数の型宣言には型クラス制約をつける必要があるのだから。
-}

--- ¶ 三次元ベクトル
{-
    三次元ベクトルの型と、ベクトルの演算を作ってみよう。ベクトルは多相型にする（Int、Double、Flaot など複数の型をサポートしたいから）。
-}

data Vector a = Vector a a a deriving (Show) 
    -- 上で学んだとおり、ここには Num a => みたいな型クラス制約は書かない。

vplus :: Num a => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: Num a => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: Num a => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

{-
    上の式たちを見ると、これらの関数は Vector a という形であればどんな型でも扱えることがわかる。
    ただし、a は Num 型クラスのインスタンスであるという条件付き。
    データ宣言には Num 型クラス制約はつかず、結局、（仮にデータ宣言に型制約をつけたところで）関数内の型クラス制約（Num）を省くことはできないのである。

    念押しだが、型コンストラクタと値コンストラクタを区別しておくことはとても重要である。
    データ型を宣言するとき、= の前にあるのが型コンストラクタであり、後ろにある（あるいは | で区切られたその後ろにある）のが値コンストラクタである。
    たとえば、関数にこのような型を与えるのは間違い。
        Vector a a a -> Vector a a a -> a
    これが動かないのは、ベクトルの型は Vector a であって、Vector a a a ではないからである。
    ベクトルは、型としてはあくまで引数を 1 つだけ取る。値コンストラクタは 3 つの引数を取るが、それは別の話である。
        data Vector a = Vector a a a deriving (Show)
        型コンストラクタ　　こっちが値コンストラクタ
-}

-- 作ったベクトルで遊んでみよう。
vecSum :: Num a => Vector a
vecSum = Vector 3 5 8 `vplus` Vector 9 2 8 -- Vector 12 7 16

threeVecSum :: Num a => Vector a
threeVecSum = Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3 -- Vector 12 9 19

multedVec :: Num a => Vector a
multedVec = Vector 3 9 7 `vmult` 10 -- Vector 30 90 70

proded :: Num a => a
proded = Vector 4 9 5 `dotProd` Vector 9 2 4 -- 74

calcedVec :: Num a => Vector a
calcedVec = Vector 2 9 3 `vmult` (Vector 4 9 5 `dotProd` Vector 9 2 4) -- Vector 148 666 222
