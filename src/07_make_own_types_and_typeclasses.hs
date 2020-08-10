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

