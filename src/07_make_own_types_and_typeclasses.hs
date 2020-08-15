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

-------------------------------
-- インスタンスの自動導出
-------------------------------

{-
    型クラスについて改めて復習：型クラスは、あるふるまいを定義するインターフェースであり、ある型がその振る舞いをサポートしていれば、
    その型クラスのインスタンスにできるのだった。たとえば、Int 型は Eq 型クラスのインスタンスである。
    これは、Eq 型クラスは「等値生テストができるもの」という振る舞いを定義しているからである。
    整数には等値性が定義されているから、Int は Eq 型クラスに属することになる。
    これが便利なのは、Eq のインターフェースになる、== と /= という関数があるからである。
    ある型が Eq 型クラスのインスタンスであるなら、その型の値には == が使える。
    これこそが、 4 == 4 と "foo" == "bar" が両方とも型検査を透仕組みである。

    Haskell の型クラスは、Java、Python、C++ といった言語の「クラス」と紛らわしいので、引っかかりやすいところである。
    そういったオブジェクト指向言語でクラスといえば、何らかの動作をするオブジェクトを作るための青写真のことである。
    しかし、Haskell のクラスは、データを作る道具ではない。
    そうではなく、まずデータ型を作り、それから「このデータには何ができるのだろう？」と考えるのである。
    もしその型が、等値性をテストできるものであれば、Eq 型クラスのインスタンスにする。
    その型が代償比較できるものであれば、Ord 型クラスのインスタンスにする。

    Haskell は、特定の型クラスのインスタンス宣言を自動導出（derive）する能力を備えている。
    自動導出できる型クラスは Eq、Ord、Enum、Bounded、Show、Read である。
    自作のデータ型を作るとき、deriving キーワードを使えば、Haskell がこれらの型クラスの文脈での振る舞いを自動導出してくれる。
-}

--- ¶　人間の平等
{- 以下のデータ型を見よ。-}

{-
data Person_ = Person_ { firstName_ :: String
                       , lastName_ :: String
                       , age_ :: Int
                       }
-}

{-
    これは人間を表している。いま、この世に同姓同名で年齢まで一致する人はいない、と仮定しよう。
    では、二人の人の記録があるとき、その 2 つの記録が同一人物を表しているか判定することは妥当だろうか。
    もちろん、判定できてしかるべきである。
    だから、この型を Eq 型クラスに属させるのは妥当なことだろう。
    インスタンス宣言は自動導出してもらおう。
-}

data Person_ = Person_ { firstName_ :: String
                       , lastName_ :: String
                       , age_ :: Int
                       } deriving (Eq)

{-
    ある型に Eq を自動導出して == や /= で比較しようとすると、Haskell はまず値コンストラクタが一致しているかを調べる。
    それから、値コンストラクタの中に入っている各フィールドがすべて一致しているか、それぞれの組みを == を使って比較する。
    ただし落とし穴が 1 つある。
    すべてのフィールドの型が、Eq 型クラスのインスタ成すでないと自動どうshつには使えない。
    今回の場合は、String も Int もこの条件を満たすから OK である。

    まずは人物を何人か作ってみよう。
-}

mikeD :: Person_
mikeD = Person_ {firstName_ = "Michael", lastName_ = "Diamond", age_ = 43}

adRock :: Person_
adRock = Person_ {firstName_ = "Adam", lastName_ = "Horovitz", age_ = 41}

mca :: Person_
mca = Person_ {firstName_ = "Adam", lastName_ = "Yauch", age_ = 44}

{-
    Eq を自動導出したことにより、
    mca == adRock や mikeD == adRock、mikeD == mikeD が意味を持つようになる。
    （ターミナルに打ち込むとそれぞれ False, False, True と返ってくる）

    もちろん、Eq a という型クラス制約のついた関数の a のところならどこにでも使える。たとえば elem など。
    mikeD `elem` [mca, adRock, mikeD]
    > True
-}

--- ¶　読み方を書いてみせてよ
{-
    Show と Read はそれぞれ文字列へ変換できるものの型クラス、および文字列から変換できるものの型クラスである。
    Eq のときと同じく、ある型を Show や　Read のインスタンスにしたいなら、その型の値コンストラクタにフィールドがあれば、
    それらの型も Show や Read に属している必要がある。

    では、Person_ データ型を Show と Read にも属させてみよう。
-}

data Person__ = Person__ { firstName__ :: String
                        ,  lastName__ :: String
                        ,  age__ :: Int
                        } deriving (Eq, Show, Read)

{- これで人物の情報をターミナルに出力できる。 -}

mikeD' :: Person__
mikeD' = Person__ {firstName__ = "Michael", lastName__ = "Diamond", age__ = 43}

adRock' :: Person__
adRock' = Person__ {firstName__ = "Adam", lastName__ = "Horovitz", age__ = 41}

mca' :: Person__
mca' = Person__ {firstName__ = "Adam", lastName__ = "Yauch", age__ = 44}

{-
    deriving Read もしているので、ターミナルに以下のように乳右力すると、Person__ 型として出力を返してくれる。
        > read "Person__ {firstName__ = \"Mike\", lastName__ = \"Diablo\", age__ = 23}" :: Person__
        Person__ {firstName__ = "Mike", lastName__ = "Diablo", age__ = 23}

    多相型も読み取ることができるが、どの型がほしいか Haskell が推論できるだけの情報を与える必要がある。例えば、以下のようなのを試すとエラーになる。
        read "Just 3" :: Maybe a
    正しくは、read "Just 3" :: Maybe Int などとする必要がある（Maybe Double とか Maybe Float でもよい）。
-}

--- ¶　順番を守ってください！
{-
    順序づけ可能な型のための型クラス、Ord のインスタンスも自動導出できる。同じ型の値を 2 つ比較したとき、もし 2 つが異なる値コンストラクタから作られた物なら、
    先に定義されているほうが小さいとみなされる。
    例えば、Bool は False と True の値を取る。Bool を比較すると何が起こるかは、Bool は以下のように定義されていると考えればわかる。
        data Bool = False | True deriving (Ord)
    False 値コンストラクタが先に定義されていて、その後で True が指定されているので、Ord を自動導出すると、True は False より大きいのだろうと考えられる。
-}

comp1 :: Ordering
comp1 = True `compare` False -- GT

comp2 :: Bool
comp2 = True > False -- True

{-
    2 つの値が同じ値コンストラクタでできている場合、フィールドがなければ 2 つは等しいとされる。
    フィールドがあれば、フィールドどうしが比較され、どちらが大きいか決まる（この場合、フィールドの型もまた Ord に属している必要がある）。

    Maybe a データ型では、Nothing 値コンストラクタが Just 値コンストラクタの前に定義されているので、Nothing 値はつねに Just something より小さいとされる。
    一方、2 つの Just 値を指定したときは、Haskell は中身を比較してくれる。

    でも、Just (*3) > Just (*2) のようなことはできない。(*3) などは関数の型を持ち、関数は Ord のインスタンスではないから。
-}

comp3 :: Bool
comp3 = Nothing < Just (100 :: Int) -- True

comp4 :: Bool
comp4 = Nothing > Just (-999999 :: Integer) -- False

comp5 :: Ordering
comp5 = Just (3 :: Int) `compare` Just (2 :: Int) -- GT

comp6 :: Bool
comp6 = Just (100 :: Int) > Just (50 :: Int) -- True

--- ¶ 何曜日でもいいよ
{-
    代数データ型を使えば列挙型は簡単に作ることができる。
    その際には Enum と Bounded 型クラスが便利。こんなデータ型を考えてみよ。

        data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

    すべての値コンストラクタがゼロ引数（フィールドがない）なので、これを Enum 型クラスに属させることができる。
    Enum は、前者関数（pred）と後者関数（succ）を持つ型のための型クラスである。
    Day は上限と下限を持つ型の型クラスである Bounded のインスタンスにもできる。
    ついでに自動導出できるすべての型クラスのインスタンスにしてしまおう。
-}

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
        deriving (Eq, Ord, Show, Read, Bounded, Enum)

--　Day 型で何ができるのか？
{-
    Day は Show と Read のインスタンスであるから、その値を文字列にしたり文字列から変換したりできる。
    Wednesday
    > Wednesday

    show Wednesday
    > "Wednesday"

    read "Wednesday" :: Day
    > Wednesday

    Eq 型クラスと Ord 型クラスのインスタンスでもあるので、等号や不等号で比較できる。
    Saturday == Sunday
    > False

    Saturday == Saturday
    > True

    Saturday > Friday
    > True

    Monday `compare` Wednesday
    > LT

    さらに、Bounded のインスタンスでもあるから、上限と下限を取ることもできる。
-}

minBoundOfDay :: Day
minBoundOfDay = minBound :: Day -- Monday

maxBoundOfDay :: Day
maxBoundOfDay = maxBound :: Day -- Sunday

{-
    Enum のインスタンスでもあるので、昨日の曜日や明日の曜日を知ることができるし、範囲を指定してリストを作ることもできる。
-}

afterMon :: Day
afterMon = succ Monday -- Tuesday

beforeSat :: Day
beforeSat = pred Saturday -- Tuesday

daysList :: [Day]
daysList = [Thursday .. Sunday] -- [Thursday,Friday,Saturday,Sunday]

week :: [Day]
week = [minBound .. maxBound] -- [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]

-------------------------------
-- 型シノニム
-------------------------------

{-
    [Char] と String は同値で、交換可能であった。これは型シノニム（型同義名）を使って実装されている。
    型シノニムそのものは特に何もしない。ある型に別名を与えて、コードやドキュメントを読みやすくするだけ。
    標準ライブラリの String の定義は、以下のように [Char] の型シノニムになっている。
        type String = [Char]
    type というキーワードは誤解を招きやすいかもしれない。ここでは既存の型のシノニムが定義されているのであって、
    新しい型が作られているわけではない（それは data の役割である）。
-}

--- ¶　電話帳をかっこよくしよう
{-
前に Data.Map モジュールを扱ったとき、電話帳をまずは連想リスト（キー/値のペアのリスト）で表し、それから Map に変換した。
これが連想リストバージョンである。
-}

phoneBook :: [(String, String)]
phoneBook = 
    [ ("betty", "555-2938")
    , ("bonnie", "452-2928")
    , ("patsy", "493-2928")
    , ("lucille", "205-2928")
    , ("wendy", "939-8282")
    , ("penny", "853-2492")
    ]

{-
phoneBook の型は [(String, String)] である。この型宣言からは、これが文字列（キー）から文字列（バリュー）の連想リストであることは読み取れるが、それ以上はわからない。
型宣言にもっと有益な情報を載せるため、型シノニムを作ろう。
-}

--type PhoneBook = [(String, String)] -- あとで書き換えるためコメントアウト。

{- 
    これで電話帳の型は phoneBook :: PhoneBook となった。
    型宣言のところで phoneBook :: PhoneBook とすればよい。
    続けて、String の型シノニムも作ろう。
-}

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

{-
    自分のプログラムの中で使っている文字列について、「ただの文字列でなく実際はこれを表しているんだよ」という情報を伝えたいとき、
    String に型シノニムを与えればよい。
    というわけで、名前と電話番号を取って、そおの名前と電話番号の組みが電話帳に載っているか調べる関数を実装するときにも、
    何をする関数なのか一目でわかる型宣言を書けるようになった。
-}

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

{-
    型シノニムを使わなかったら、 String -> String -> [(String, String)] -> Bool という型宣言になっていた。型シノニムを使ったほうが理解しやすい。
    しかし、型シノニムは使いすぎてもいけない。
    型シノニムを作るのは、自作の関数内で既存の型が何を表しているかを示すことで型宣言のドキュメントとしての質を高めるためか、
    何度も出てくる長々しい型（[(String, String)] のような）が自作の関数の文脈では何か特定のものを表している場合に限る。
-}

--- ¶ 型シノニムの多相化
{-
    型シノニムも型引数を取るようにできる。
    たとえば、連想リストを表す型を作りたいけどキーや値の型は特定せず汎用にしておきたい、というのであれば、こう書ける。
        type AssocList k v = [(k, v)]
    これで、連想リストからキーを検索してくれる関数の型を (Eq k) => k -> AssocList k v -> Maybe v と書けるようになった。
    AssocList は 2 つの型を引数に取って、AssocList Int String のような具体型を返す型コンストラクタである。

    関数を部分適用して新しい関数を作れるのと同じように、型引数を部分適用すると新しい型コンストラクタが作れる。
    関数を呼ぶときに引数の数が足りないと、残りの引数を取る新しい関数が返ってくるのだった。
    同じように、型コンストラクタに型引数を一部しか与えないと、残りの型引数を取る型コンストラクタが返ってくる。
    例えば、Data.Map を使い、Int をキーとしてなんらかの値を返す Map の型を作りたければ、以下のようにする。

        type IntMap v = Map.Map Int v // import qualified Data.Map as Map してる前提。

    もしくは、こう。

        type IntMap v = Map.Map Int

    どちらの書き方にしても、IntMap は引数を 1 つ取る型コンストラクタになり、その引数こそが Int（キー）が指す値（バリュー）になる。
-}

-- ~~~ 具体的な使い方。
type IntMap = Map.Map Int

numToName :: IntMap String
numToName = Map.fromList [(1, "Alice"), (2, "Bob"), (3, "Chris")]

{-
    型コンストラクタと値コンストラクタの違いをちゃんと理解できているか？
    IntMap や AssocList という名の型シノニムを作ったからといって、AssocList [(1,2), (4,5), (6,7)] のようなものが作れるようになるわけではない。
    できるのは、すでにあるものの型を別の名前で呼ぶことだけである。
        [(1,2), (4,5), (6,7)] :: AssocList Int Int
    や
        [(1,2), (4,5), (6,7)] :: IntMap Int
    と書くことはできる。こうすることで、中身の数が Int 型だと推論される。
    このリストの型には特別な名前がついているが、整数のペアのリストが使える場所ならどこででも使うことができる。

    Haskell のソースコードは、値の領域、型の領域などに分かれていると考えられる。
    型の領域に属するのは、データ型や型シノニムの宣言、それから型宣言や型注釈に登場する :: の右側などである。
    型シノニムや、そのほか型の領域に属するものは、Haskell の一部である型の領域でしか使えないのである。
    AssocList [(1,2), (4,5), (6,7)] は、型の領域に属するべき AssocList を値の領域で使っているので、文法的に誤りだと言える。
-}

--- ¶　そこを左に行って、すぐ右へ
{-
    型引数を 2 つ取るデータ型といえば Either a b もある。
    Either の定義は、だいたい以下のような感じ。
        data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
    Either a b には値コンストラクタが 2 つある。Left を使うと、Either の中身は a 型になる。Right を使ったときは、中身は b 型になる。
    つまり、Either 型を使って「2 つの型のうちどちらか一方」という値を表せる。
    Either a b 型の値を受け取り、Left と Right をパターンマッチして、どちらの型に合致したかに応じて異なる処理をする、という使い方をよくする。

        > Right 20
        Right 20

        > Left "w00t"
        Left "w00t"

        > :t Right 'A'
        Right 'A' :: Either a Char

        > :t Left True
        Left True :: Either Bool b

    この例では、Left True の型を評価すると Either Bool b という型になっていることがわかる。
    Left 値コンストラクタで作った値なので、1 つ目の型引数は Bool に決まっているが、2 つ目の型引数は多相のまま残っている。
    これは、Nothing という値に Maybe a という型がつくのと同じ理屈である（:t Nothing とすると Nothing :: Maybe a が返る）。

    これまでのところ、失敗したかもしれない計算を表現するには Maybe a を使ってきた。
    しかし、Maybe a では足りない状況もある。Nothing では「何かが失敗した」以上の大した情報を持てないからである。
    Data.Map の lookup であれば、失敗するのはキーが Map に入っていなかったときだけなので何が起こったかは明白であり、Nothing で済む。

    しかし、関数がなぜ失敗したのか、どのように失敗したのかを知りたいとき、普通は Either a b 型の返り値を使う。
    ここで、a は失敗が起こった場合に何であるかを伝えてくれる型、b は成功した計算の型である。
    したがって、エラーは Left 値コンストラクタを、結果は Right 値コンストラクタを使って表す。

    例として、生徒の一人一人にロッカーが割り当てられる高校を考えてみよう。
    それぞれのロッカーには暗証番号がついている。新しいロッカーの割り当てが必要な生徒は、ロッカー管理人に欲しいロッカーの番号を伝え、
    管理人が暗証番号を渡す。しかし、そのロッカーを誰かがすでに使っている場合、生徒は新しいロッカーを選び直す必要がある。
    ロッカー全体を Data.Map の Map で表そう。ロッカー番号から、ロッカーが使用中かどうかのフラグと、ロッカーの暗証番号への Map である。
-}

-- import qualified Data.Map as Map している前提で
data LockerState = Taken | Free deriving (Show, Eq) -- ロッカーの状態。エラーは Left 値コンストラクタを、結果は Right 値コンストラクタを使って表す
type Code = String -- 暗証番号
type LockerMap = Map.Map Int (LockerState, Code) -- ロッカー全体の Map

{-
    ロッカーが埋まっているか空いているか表す新しいデータ型 LockerState を導入した。
    また、ロッカーの暗証番号には型シノニム　Code を与えた。
    さらに、整数から「ロッカーの状態と暗証番号の組」への Map にも型シノニム LockerMap を与えた。

    続いて、ロッカーを表す Map から暗証番号を検索する関数を作ろう（つまり、「生徒から希望のロッカー番号を聞いて、使用可能なロッカーであれば暗証番号を教える」ときに利用する）。
    この関数の結果は Either String Code 型で表すことにする。この関数が失敗するパターンは 2 通りあるからである。
    ロッカーを他の誰かが使っている場合に暗証番号を伝えるわけにはいかない。それから、ロッカー番号自体が存在しない可能性もある。
    検索が失敗したときは、単に String を返して何が起こったか説明することにしよう。
-}

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber lockermap = case Map.lookup lockerNumber lockermap of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!" -- Map.lookup lockerNumber lockermap の評価結果が Nothing だった場合（ロッカー自体がなかった場合） -> エラー（Left）を返す
    Just (state, code) -> if state /= Taken -- Map.lookup lockerNumber lockermap の評価結果が Just (state, code) だった場合（ロッカーの存在は確かである） -> state でさらに場合分け（Taken か Free か）
                            then Right code
                            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

{-
📜 【復習】case 式の構文（👉忘れている場合は 03_function.hs へ）
    case expression of  pattern -> result
                        pattern -> result
                        pattern -> result
                        ...
-}

-- ~~~ 次のようなロッカーの Map があったとしよう。
lockers :: LockerMap
lockers = Map.fromList
    [(100, (Taken, "ZD39I"))
    ,(101, (Free, "JAH3I"))
    ,(103, (Free, "IQSA9"))
    ,(105, (Free, "QOTSA"))
    ,(109, (Taken, "893JJ"))
    ,(110, (Taken, "99292"))
    ]

{-
    こんな感じの結果になる。
        *Main> lockerLookup 100 lockers
        Left "Locker 100 is already taken!"
        *Main> lockerLookup 101 lockers
        Right "JAH3I"
        *Main> lockerLookup 102 lockers
        Left "Locker 102doesn't exist!"

    結果を表すのに Maybe a を使うという手もあるが、そうしていtればロッカーの取得に失敗しても原因がわからない。
    だが、今は関数の返り値の型が失敗の情報を伝えられるようになっている。
-}

-------------------------------
-- 再帰的なデータ構造
-------------------------------

{-
    すでに見たように、代数データ型の値コンストラクタは複数のフィールドを持つこともできるし、フィールドを持たないこともできる。
    そして、各フィールドの型は具体型である必要がある。
    それなら、フィールドに持つ型は自分自身でもかまわないということだ！
    つまり、再帰的なデータ型（ある型の値の一部にまた同じ型の値が入っていて、そのまた一部にまたまた同じ型の値が入っていて、...というデータ型）が作れるということ。

    [5] というリストで考えてみよう。これは 5:[] の構文糖衣だった。
    : の左辺には値が 1 つある。右辺にはリストがある（今の場合は空リスト）。では、[4,5] というリストはどうか？
    これは 4:(5:[]) である。先頭の : をみると、やはり左辺には値が 1 つあり、右辺にはリスト (5:[]) がある。
    3:(4:(5:6:[])) のような長いリストも仕組みは同じである。この式は、: は右結合であることを使って 3:4:5:6:[] とも書けるし、構文糖衣を使えば [3,4,5,6] とも書ける。
    リストは、「空リスト」または「要素とリスト（空でもいい）を : で結合したもの」のいずれかの値を取るデータ構造である。
    では、代数データ型を使って独自のリスト型を実装してみよう！
-}

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord) -- Empty と Cons はいまここで自分で定義した値コンストラクタ（予約語ではない）。Kara, Ketsugou とかでもいい。

-- わかりにくかったら、レコード構文で考えてもよい。
data List' a = Empty' | Cons' { listhead :: a, listTail :: List a }
    deriving (Show, Read, Eq, Ord)

{-
    ここに出てくる Cons コンストラクタは : を言い換えたもの。
    Haskell 標準のリストにおける : も、値とリストを取ってリストを返す値コンストラクタなのである。
    別の言い方をすると、: には a 型と List a 型の 2 つのフィールドがあるということ（Cons a (List a) の部分を見ればわかる）。
-}

karaList :: List Int -- 後続の操作のために List Int 型にするが、空リストを定義するだけなら型変数を使って karaList :: List a でもよい。
karaList = Empty

fiveToKaraList :: List Int
fiveToKaraList = 5 `Cons` karaList -- fiveToKaraList の値は Cons 5 Empty になる。

suujiList :: List Int
suujiList = Cons 3 $ Cons 2 $ Cons 1 Empty -- Cons 3 (Cons 2 (Cons 1 Empty)) // Cons は前置でも中置でもいい

suujiList' :: List Int
suujiList' = 3 `Cons` (2 `Cons` (1 `Cons` Empty)) -- Cons 3 (Cons 2 (Cons 1 Empty))

--- ¶　リストの改善
{-
    記号文字だけを使って関数に名前をつけると、自動的に中置関数になる。
    値コンストラクタもデータ型を表す関数なので、同じルールに従う。
    ただし、1 つだけ制限がある。中置関数にする場合、値コンストラクタの名前はコロンで始まる必要がある。
-}

infixr 5 :-:
data List'' a = Empty'' | a :-: (List'' a) deriving (Show, Read, Eq, Ord) 
        -- :-: としているけど、Haskell の制約的には「コロンで始ま」ればよいので、必ずしもコロンで囲む必要はない（ :- でも　OK）

{-
    まず新しい構文要素に注目。データ宣言の前の行にある**結合性宣言**である。
    関数を演算子として定義した場合、その結合性（fixity）を宣言できる。
    結合性宣言は必須ではない。結合性宣言では、演算子の結合順位や、左結合なのか右結合なのかを指定する。
    たとえば、* 演算子の結合性は infixl 7 * で、+ 演算子の結合性は infixl 6 である。
    これは、* も + も左結合（4 * 3 * 2 は (4 * 3) * 2 と等しい）だが * は + より強く結合する（結合性宣言の数字が大きいから）ということを意味する。
    つまり、5 + 4 * 3 は 5 + (4 * 3) と同じ意味になる。
    　💡ちなみに、結合性宣言を省略した演算子はすべて infixl 9 になる。` で演算子化した関数に対しても結合性を宣言でい、省略した場合は同様に infixl 9 になる。
    結合性宣言を除くと、Cons a (List a) を a :-: (List a) に書き換えただけである。
    これで、リスト型に属するリストを以下のように書ける。
-}

myList :: List'' Int
myList = 3 :-: 4:-: 5 :-: Empty'' -- myList は 3 :-: (4 :-: (5 :-: Empty'')) になる

myListPlus100 :: List'' Int
myListPlus100 = 100 :-: myList -- 100 :-: (3 :-: (4 :-: (5 :-: Empty'')))

{-
    次に、2 つのリストを結合する関数を作ろう。標準のリストにおける ++ の定義は以下のようになっている。
        infixr 5 ++
        (++) :: [a] -> [a] -> [a]
        [] ++ ys = ys
        (x:xs) ++ ys = x: (xs ++ ys)
    このコードを使って、オリジナルの結合関数を作ってみよう。^++ という名前にする。
-}

--- ^++ は :-: と違って値コンストラクタではない（ただの（？）関数である）ので、: で始める必要はない。
infixr 5 ^++
(^++) :: List'' a -> List'' a -> List'' a
Empty'' ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

--- 試してみよう。

herList :: List'' Int
herList = 10 :-: 20 :-: 30 :-: Empty''

ourList :: List'' Int
ourList = myList ^++ herList -- 3 :-: (4 :-: (5 :-: (10 :-: (20 :-: (30 :-: Empty'')))))

{-
    さりげなく (x :-: xs) というパターンマッチを使っていることに注目。
    これが可能なのは、パターンマッチとは値コンストラクタをマッチさせることに他ならないからである。
    :-: は、我々が作ったリストの値コンストラクタであり、: は Haskell 組み込みのリストの値コンストラクタなので、どちらも当然パターンマッチできる。
    [] がパターンマッチできるのも同じ理由からである。
    パターンマッチは値コンストラクタであれば何に対しても使えるので、通常の前置コンストラクタに加えて 8 や 'a' といったものもパターンマッチできる。
    なんとなればこれらは数値型や文字型の値コンストラクタだからである。
-}

--- ¶　木を植えよう
{-
    Haskell の再帰的データ構造にもっと慣れるために、二分探索木を実装してみよう。
    二分探索木では、1 つの要素が 2 つの小要素へのポインタを持つ。一方は左の子で、もう一方は右の子である。
    左の子は親より小さく、右の子は親より大きいようにしておく。
    それぞれの子要素は、さらに 2 つ（あるいは 1 つ、または 0 個）の要素を持つ。
    結果として、各要素は 2 つ以下の部分木を持つことになる。

        📔  Data.Set や Data.Map が提供する Set や Map も木構造を使って実装されているが、ただの二分探索木ではなく、平衡二分探索木を使っている。
            木構造の平衡とは、左右の要素の深さがだいたい等しくなっていることを指す。平衡木の探索は普通の木よりも速くなる。
            だが、ここでは通常の二分探索木を実装することにする。

    木構造とは、空の木、もしくはなんらかの値（根）と 2 つの木（部分木）を含む要素からなる構造である。
    まるで代数データ型で表してくれといわんばかりの構造！
-}

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

{-
    手作業で木を作るかわりに、木と要素を取って要素を木に挿入する関数を作ろう。
    まず、新しい値をルート（根）の要素と比較する。その値がルートより小さければ左に、大きければ右に行く。
    空の木にたどり着くまで同じことを繰り返す。
    空の木が見つかったら、そこに新しい値を保持するノードを追加する。

    C のような言語なら、このような操作はポインタや値の更新を使って書くべきだろう。
    Haskell では、木を直接更新できないので、左右どちらに行くのか決めるたびに新しい部分木を作っていく必要がある。
    最終的に挿入関数は新しい木全体を作って返す。
    **Haskell にはポインタという概念はなく、値しかないからである**。
    したがって、これから作る挿入関数の型は a -> Tree a -> Tree a みたいになる。
    これは、ある要素とある木を取って、要素が挿入された新しい木を返す関数である。
    こう言うと効率が悪いように聞こえるかもしれないが、Haskell には古い木と新しい木の部分構造のほとんどを共有する仕組みが備わっているので心配無用。
    以下が木を作るための 2 つの関数である。
-}

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right -- すでに要素 x は挿入されているということなので、元の木をそのまま返す（右辺は Node a left right でもいい）
    | x < a = Node a (treeInsert x left) right
    | otherwise = Node a left (treeInsert x right) -- ここの otherwise は x > a の意味。

{-
    singleton は、要素が 1 つしかない木を作るための補助関数である。ルートに何かが入っていて左右の部分木は空であるようなノードを手軽に作れるように定義しただけ。
    要素 x を木に挿入するための関数は treeInsert である。まずは、再帰の基底部をパターンマッチで表現している。
    挿入先が空の木である場合は、目的地にたどり着いたということなので、x を唯一の要素として持つ木を挿入する。
    挿入先が空の木でない場合は調査が必要になる。まず、新しい要素とルート要素が等しければ、すでにその要素は挿入されているということなので、元の木をそのまま返す。
    新しい要素のほうが小さければ、「ルートの値と右部分木は元のままで、新しい要素が挿入された左部分木を持つ」木を返す。
    新しい要素のほうが大きかった場合は、新しい要素を右部分木に挿入した木を同様にして返す。

    では次は、ある要素が木に属しているかを判定する関数を作ろう。
    簡単なので解説は省略（すごい Haskell 楽しく学ぼう p. 141）。
-}

treeElem :: Ord a => a -> Tree a -> Bool
_ `treeElem` EmptyTree = False
x `treeElem` (Node a left right)
    | x == a = True
    | x < a = x `treeElem` left
    | otherwise = x `treeElem` right -- x > a

-- ~~~ それでは木で遊ぼう！　畳み込みを使ってリストから木を作ろう。リストを 1 要素ずつ辿って値を返す操作はたいがい畳み込みで実装できるということを覚えておくように。
-- ~~~ 空の木から始め、リストを右から辿ってアキュムレータ木に要素を追加していこう。

nums :: [Int]
nums = [8,6,4,1,7,3,5]

numsTree :: Tree Int
numsTree -- = foldr (\xs acc -> treeInsert xs acc) EmptyTree nums // 自分の answer。これでも間違いじゃないけど冗長
    = foldr treeInsert EmptyTree nums
        -- 出力は Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))

{-
    この foldr の畳み込みでは、treeInsert（リストの要素と木を取って新しい木を作る）が 2 引数関数であり、EmptyTree が初期のアキュムレータである。
    nums はもちろん、畳み込み対象のリストである。
    コンソールに表示される木は読みやすいものではないが、それでもなんとなく構造はわかる。
    ルートノードはどうやら 5 のようである。2 つの部分木を持っていて、それぞれのルートノードは 3 と 7 である。
    ある値が木に含まれているかどうかも調べられる。
-}

isInTree8 :: Bool
isInTree8 = 8 `treeElem` numsTree -- True

isInTree100 :: Bool
isInTree100 = 100 `treeElem` numsTree -- False

{- Haskell の代数データ型はとってもパワフルな概念。真理値や曜日の列挙型を手始めに、なんでも作れてしまう！ -}
-------------------------------
-- 型クラス　中級講座
-------------------------------

{-
    ここまで、Haskell に標準でついてくる型クラスや、それらにどの型が属しているかを学んできた。
    また、Haskell に自動導出してもらうことで独自の型を標準型クラスのインスタンスにする方法も学んだ。
    この節では、独自の型クラスを作り、そのインスタンスを手動で作る方法を学ぶ。

    型クラスについて軽く復讐しておこう。
    型クラスはインターフェイスのようなものである。型クラスは、特定の振る舞い（等値性判定とか、順序の比較とか、列挙とか）を定義うsる。
    定義されたとおりに振舞うことができる型は、その型クラスのインスタンスにされる。
    型クラスの振る舞いは、型クラス関数を定義することで得られる。
    型宣言だけして実装は後回しにしても構わない。

    というわけである型 T がある型クラス C のインスタンスであるとは、型クラス C が定義する関数（メソッド）たちを型 T に対して使える、ということを意味する。

    > 💡 再確認だが、「型クラス」は Java や Python のような言語に出てくる「クラス」とはなんの関係もない。
-}

--- ¶ Eq 型クラスの内部
{-
    Eq 型クラスを例に取ろう。Eq は等値性判定ができる値の型クラスであった。
    Eq は == と /= という関数（メソッド）を定義しているのだった。
    いま、Car という型があって、2 つの自動車を等値関数 == で比較することに意味があるのなら、Car を Eq のインスタンスにするのが理にかなっている。
    これが標準ライブラリにおける Eq の定義である。
-}

{-
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
-}


{-
    まず、class Eq a where は Ew という名の新しい型クラスの定義が始まることを意味する。
    a は型変数で、将来 Eq のインスタンスとなるのであろう型を表す（ちなみに、名前が a である必要もなく、一文字である必要もない。小文字から始まっていればよい）。

    次に、関数がいくつか定義されている。ただし、関数定義の実態を与えなくてもかまわない。必須なのは型宣言だけ。
    ここでは、Eq の定義する関数の実態が実装されている（デフォルト実装と呼ぶ）。今回は相互再帰という形をとっている。
    そのソースを読むと、「Eq に属する型を持つ 2 つの値は、それらが違いに異ならないならば等しく、違いに等しくないならば異なっている」と書いてある。
    こんなのが一体何の役に立つのか、後ほどわかる。

    型クラス定義に含まれる関数には、最終的にちょっと特別な型がつく。例えば、class Eq a where と宣言して、それからクラスの中で (==) :: a -> a -> Bool のような型宣言をしたとする。
    後でその関数の型を調べると、(Eq a) => a -> a -> Bool という型になっているだろう。 
-}

--- ¶　交通信号データ型
{-
    こうして作ったクラスだが、これで何ができるだろうか？　型をこのクラスのインスタンスにして、クラスの便利な機能を使うことができる。
    例えば以下の型を見よ。
-}

data TrafficLight = Red | Yellow | Green

{-
    これは交通信号の状態を定義する型である。見てのとおり、自動導出（deriving）は使っていない。これは、インスタンスを手で書いてみることが目的だからである。
    以下が Eq のインスタンスの作り方である。
-}

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

{-
    instance キーワードを使ってインスタンスを作った。そう、新しい型クラスを定義するのが class で、型を型クラスのインスタンスにするのが instance なのである。
    Eq を定義したときには class Eq a where と書き、a は将来インスタンスになりうるあらゆる型の 1 つを表しているんだといった。
    ここで、その言葉の意味が明らかになったと言えよう。
    なぜなら、インスタンスを作るにあたり instance Eq TrafficLight where と書いているからである。
    クラス定義の a がまさに実際の型で置き換えられている。　・・・（⭐️）

    さて、クラスを宣言したときには == を定義するのに /= を使い、逆に、 /= を定義するのにも == を使っていた。
    そのため、インスタンス宣言ではどちらか一方だけを上書きすればよいことになる。
    これは、型クラスの **最小完全定義（minimum complete definition）** と呼ばれる概念である。
    インスタンスになろうとする型をクラスの宣伝文句のとおりに振舞わせるために、最低限定義する必要のある関数たちがあるということである。
    Eq の最小完全定義を満たすには、== か /= のいずれかを上書きする必要がある。
    もし Eq の定義がこれ（↓）だけだったら
        class Eq a where
            (==) :: a -> a -> Bool
            (/=) :: a -> a -> Bool
    Eq のインスタンスを作るには、両方の関数を定義する必要があっただろう。
    Haskell には、この 2 つの関数の間に何か関係があるなんてわからないからである。
    この場合の最小完全定義は == と /= の両方ということになっただろう。

    見ての通り、ここでは単純なパターンマッチを使って == のほうを定義した。
    2 つの信号が **等しくない** ケースはもっといろいろあるので、まず **等しい** 場合をすべて指定し、最後に必ず合致するパターンを置いて
    「以上の組み合わせに当てはまらない場合は、等しくないとする」といったのである。

    Show のインスタンスにするのも手動でやってみよう。Show の最小完全定義を満たすには、値を取って文字列に変える show 関数を定義すれば十分である。
-}

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

{-
    どう動くかチェックしてみよう。

        *Main> Red == Red
        True
        *Main> Red == Yellow
        False
        *Main> Red `elem` [Red, Yellow, Green]
        True
        *Main> [Red, Yellow, Green]
        [Red light,Yellow light,Green light]

    Eq に関しては自動導出でも同じ効果が得られた。
    ただし、Show に関しては、自動導出を使うと、値コンストラクタがそのまま文字列に変換されて出るだけである。
    信号が Red light とかに表示されてほしいなら、インスタンス宣言を手動で書く必要がある。
-}

--- ¶　サブクラス化
{-
    別の型クラスのサブクラスである型クラスを作ることもできる。例えば Num の型クラス宣言は、全体はちょっと長いが最初の部分はこんなふうになっている。
        class (Eq a) => Num a where

    前述のとおり、型クラス制約を狭める場所というのがいろいろある。
    この例は、普通に class Num a where と書くのに似ているが a が Eq のインスタンスになっている必要があると言っている。
    要するに、ある型を Num のインスタンスにしたかったら、その前に Eq のインスタンスにする必要がある、と言っているのである。
    「ある型を数のようなものにしたいなら、等値比較くらいできないといけない」というのはうなずける話である。
    このようなとき、Num は Eq のサブクラスであると言う。

    サブクラス作りに必要なのはこれだけ。型クラス宣言に型クラス制約を付ければいいのである！
    こうしておけば、型クラス宣言やインスタンス宣言でメソッドの実体を書くとき、a という型は Eq に属すると推論できるから、a 型の値に対して Eq が利用できる。
-}

--- ¶　多相型を型クラスのインスタンスに
{-
    ところで、Maybe とかリストといった型は、どうやって型クラスのインスタンスになれるのだろうか？
    Maybe が TrafficLight のような普通の型と違うところは、Maybe それ自身は具体型ではないところである。
    Maybe は、型引数を 1 つ、例えば Char と取って、Maybe Char という具体型を返す型コンストラクタである。再び　Eq 型クラスを見てみよう。

        class Eq a where
            (==) :: a -> a -> Bool
            (/=) :: a -> a -> Bool
            x == y = not (x /= y)
            x /= y = not (x == y)

    型宣言をみると、a は具体型として使われていることがわかる。
    なぜなら、関数に現れる型はすべて具体型である必要があるからである。
    既知事項ながら、a -> Maybe という型の関数は決して作れない。しかし、a -> Maybe a という型なら、あるいは Maybe Int -> Maybe String という型の関数なら作れる。
    このような理由から、次のようなインスタンス宣言は不可能なのである。
        instance Eq Maybe where
            ...
    a には具体型が入るが、Maybe は違う。Maybe は型引数を 1 つ取って具体型を **生み出す** 型コンストラクタである。
    では、Maybe が型引数に取りうるすべての型ごとにインスタンス宣言をいちいち書いていく必要があるだろうか？　それはキリがない。

    ということで、型引数を単に変数として残すことが許されている。
-}

{-
instance Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False

※ ちなみに Maybe は data Maybe a = Nothing | Just a というように定義されている。
-}

{-
    これは、「Maybe something のような格好をしている型はまとめて Eq のインスタンスにしたい」と言っているようなものである。
    実は型変数名は小文字で始まっていさえすればなんでもよく、(Maybe something) と書いても文法的には正しいが、
    ここでは型変数の名前は 1 文字という Haskell の流儀に従っておく。

    ここでは (Maybe m) が class Eq a where という決まり文句における a の役割を果たしている。
    Maybe は具体型ではないが、Maybe m は具体型である。
    Maybe の型引数に m という型変数を与えることで、「任意の m に対して Maybe m という姿をとる型を Eq のインスタンスにしたい」と宣言しているのだ。

    ただし、これには 1 つだけ問題がある。
    Maybe の中身に == を使った。しかし、Maybe の中身が Eq として使える保証はどこにもないはずである。
    はい、ということで、型 m に対する型クラス制約が必要！
    そこで、インスタンス宣言を以下のように書き換えるわけだ。
-}

{-
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
-}

{-
    このインスタンス宣言なら、Maybe m の形をしている下tあをすべて Eq に属するようにしたい、ただし m（Maybe の中身）も Eq に属している型に限る、と伝えるおkとができる。
    実はこれは Haskell の自動導出がやってくれることと同じである。

    ほとんどの場合、型クラス宣言での型クラス制約を使うのは、ある型クラスを別の型クラスのサブクラスにする場合で、
    インスタンス宣言での型クラス制約を使うのは、型の中身に対する必要条件を記述する場合である。
    たとえばここでは Maybe の中身が型クラス Eq に属していることを要求した。

    ある型クラスのメンバ関数の型宣言において、
    インスタンス型（※型クラスの宣言において、将来この型クラスのインスタンスになるものだよーと仮定して置いた変数 a（例えば）の型）が具体型（例えば a -> a -> Bool の a）として使われているなら、
    その型クラスのインスタンスを宣言するにあたって、型コンストラクタに必要な数の引数を与えて、括弧で囲み、具体型を仕立てなければならない。

    これからインスタンスにしようとしている型はクラス宣言における型変数の位置を占める、と考えること（←　上記の (⭐️) の部分あたりに書いてある）。
    クラス宣言 class Eq a where における a は、インスタンスを作ると実際の型で置き換えられる。
    であるから、頭の中でその型を関数型宣言に代入してみよう。
    以下のような型宣言は意味をなさない。
        (==) :: Maybe -> Maybe -> Bool
    しかし、次のような形なら大丈夫。
        (==) :: (Eq m) -> Maybe m -> Maybe m -> bool
    ちょっと考えたらわかる。== は、どんなインスタンスを作ろうと、常に (==) :: (Eq a) => a -> a -> Bool という型を持つものだから。

    それからもう 1 つ。型クラスのインスタンスが何者かを知りたければ、GHCi で `:info YourTypeClass` と打ってみよ（`:i` でも OK）。
    例えば、:info Num と打てば、型クラス Num が定義している関数、および Num に属する型のリストが表示される。
    :info は型や型コンストラクタにも使える。たとえば、:info Maybe とすれば、Maybe がインスタンスとなっている型クラスの情報がすべて表示される。

    例。
    *Main> :info Maybe
    data Maybe a = Nothing | Just a         -- Defined in ‘GHC.Maybe’
    instance Applicative Maybe -- Defined in ‘GHC.Base’
    instance Eq a => Eq (Maybe a) -- Defined in ‘GHC.Maybe’
    instance Functor Maybe -- Defined in ‘GHC.Base’
    instance Monad Maybe -- Defined in ‘GHC.Base’
    instance Semigroup a => Monoid (Maybe a) -- Defined in ‘GHC.Base’
    instance Ord a => Ord (Maybe a) -- Defined in ‘GHC.Maybe’
    instance Semigroup a => Semigroup (Maybe a)
    -- Defined in ‘GHC.Base’
    instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
    instance Read a => Read (Maybe a) -- Defined in ‘GHC.Read’
    instance Foldable Maybe -- Defined in ‘Data.Foldable’
    instance Traversable Maybe -- Defined in ‘Data.Traversable’
    instance MonadFail Maybe -- Defined in ‘Control.Monad.Fail’
-}

-------------------------------
-- Yes と No の型クラス
-------------------------------

{-
    JavaScript をはじめ、いくつかの弱く型付けされた言語では、if 式の中にほとんど何でも書くことができる。
    例えば、JavaScript では以下のようなことができる。
        if (0) alert("YEAH!") else alert("No!")
    あるいは
        if ("") alert("YEAH!") else alert("No!")
    こんなことも
        if (false) alert("YEAH!") else alert("No!")

    上のコードはすべて No! というアラートを表示する。

    一方、JavaScript では空でない文字列はすべて true 値と解釈されるため、以下のコードは YEAH! というアラートを出す。
        if ("WHAT") alert("YEAH!") else alert("No!")

    真理値の意味論が必要なところでは厳密に Bool 型を使うのが Haskell の流儀だが、
    JavaScript 的な振る舞いを実装してみるのも面白そうではある。
-}

class YesNo a where
    yesno :: a -> Bool

{-
    いたって単純。YesNo 型クラスはメソッドを 1 つだけ定義している。
    その関数は、「真理値の概念を何らかの形で含むとみなせる型」の値を鳥、それが true であるか田舎を返す。
    関数の中での a の使われ方からして、a は具体型でないとダメである。

    では、これのインスタンスを定義していこう。
    数に関しては、0 でない数は真理値として解釈した場合は真（truthy）になり、0 は偽になる（falsy）という前提にしよう（JavaScript と同じ）。
-}

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

-- 空リストは no っぽい値（falsy）で、空でないリストは yes っぽい値（truthy）である。

instance YesNo [a] where -- ここの [a] の部分は [] a と書いてもよい（Maybe a などと書き方が一致するという利点あり）
    yesno [] = False
    yesno _ = True

-- Bool 自身も真偽の概念を含んでいるはず。
instance YesNo Bool where
    yesno = id --- ❗️id は、引数を 1 つ取って同じものを返すだけの標準ライブラリ関数。

-- Maybe a もインスタンスにしてみよう
instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno _ = True -- テキストでは、yesno (Just _) = True と yesno Nothing = False でパターンマッチしている

{-
    ↑　今回は型クラス制約は不要。Maybe の中身に関しては、Just 値ならば truthy で、Nothing なら falsy と定義することにしたからである。
    それでも、Maybe でなく (Maybe a) と書く必要はある。
    なにしろ Maybe は具体型ではないので、Maybe a -> Bool という型なら何の問題もないが、Maybe -> Bool という型の関数は存在を許されない。
    とはいえ、これで Maybe something 型の値は中身の something が何であろうとも YesNo のインスタンスになったわけだから、すごいことである。

    そういえば、前に Tree a という二分探索木を表す型を作った。
    あれも、空の木は falsy で、空じゃない木は truthy ということにしよう。

        💭ちなみに。。
        *Main> :i Tree
        data Tree a = EmptyTree | Node a (Tree a) (Tree a)
                -- Defined at src/07_make_own_types_and_typeclasses.hs:1130:1
        instance [safe] Show a => Show (Tree a)
        -- Defined at src/07_make_own_types_and_typeclasses.hs:1130:62
-}

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

{-
    信号機も yes/no 値になれるだろうか？　もちろん。
    信号は、赤なら止まれ、青なら進めである（黄色も進めにする）。
-}

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

{-
    インスタンスが出そろったので遊んでみよう🎉

    *Main> yesno $ length []
    False
    *Main> yesno "haha"
    True
    *Main> yesno $ Just 0
    True
    *Main> yesno $ EmptyTree 
    False
    *Main> yesno []
    False
    *Main> yesno [0, 0, 0]
    True
    *Main> :t yesno
    yesno :: YesNo a => a -> Bool
-}

--- ~~ if の真似をして、YesNo 値を取る関数を作ってみよう。
yesnoIf :: YesNo y => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult
    | yesno yesnoVal == True = yesResult
    | otherwise = noResult -- yesno yesnoVal == False の場合。
{-
    ここではガードを使っているが、以下のように if then else でも ok
        `yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult`

    🐓　試してみよう！
        *Main> yesnoIf [] "YEAH!" "NO!"
        "NO!"
        *Main> yesnoIf [2,3,4] "YEAH!" "NO!"
        "YEAH!"
        *Main> yesnoIf True "YEAH!" "NO!"
        "YEAH!"
        *Main> yesnoIf (Just 500) "YEAH!" "NO!"
        "YEAH!"
        *Main> yesnoIf Nothing "YEAH!" "NO!"
        "NO!"
-}

