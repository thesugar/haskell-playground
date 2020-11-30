{-# OPTIONS -Wall -Werror #-}
import Control.Monad(liftM2)
import Control.Arrow

{-
(*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

Example in Haskell:

λ> isPalindrome [1,2,3]
False
λ> isPalindrome "madamimadam"
True
λ> isPalindrome [1,2,4,8,16,8,4,2,1]
True
-}

test :: ([Int] -> Bool) -> (String -> Bool) -> ([Int] -> Bool) -> Bool
test solution solution' solution'' = and [test1 solution, test2 solution', test3 solution'']
    where test1 sol = ((sol [1 :: Int, 2 :: Int, 3 :: Int]) == False)
          test2 sol' = ((sol' "madamimadam") == True)
          test3 sol'' = ((sol'' [1 :: Int, 2 :: Int, 4 :: Int, 8 :: Int, 16 :: Int, 8 :: Int, 4 :: Int, 2 :: Int, 1 :: Int]) == True)

-- 自分の答え
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = (reverse x == x)

-- 解答
isPalindrome1 :: Eq a => [a] -> Bool
isPalindrome1 [] = True
isPalindrome1 [_] = True
isPalindrome1 xs = (head xs == last xs) && (isPalindrome1 $ init . tail $ xs) -- init . tail で 最初と最後の要素を落としたものが得られる。空リストに対して使うと例外発生するので注意。

-- 上記の解答（isPalindrome や isPalindrome1）に比べると効率は悪いらしい
isPalindrome2 :: Eq a => [a] -> Bool
isPalindrome2 xs = foldl (\acc (a,b) -> if a == b then acc else False) True input
    where input = zip xs (reverse xs) -- おもしろい！　zip "hoge" (reverse "hoge") = [('h','e'),('o','g'),('g','o'),('e','h')]

isPalindrome3 :: Eq a => [a] -> Bool
isPalindrome3 = liftM2 (==) id reverse
    -- 🎯liftM2 は引数が 2 つある関数を持ち上げる。
    -- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r 
    -- 例えば liftM2 (+) [1] [1] は、+ を持ち上げて 2 つのリストモナドに適用して、[2] という結果が得られる。
    -- 今回はリストモナドではなく関数モナドに使っている。
    -- 端的に言えば、isPalindrome3 が受け取る引数に対して id と reverse を適用して、その適用結果を == で比較して Bool を返すよということ。
    -- 持ち上げの部分の挙動は liftM2 (++) id reverse "hoge" とすれば id, reverse が "hoge" に適用されて結果はそれぞれ "hoge", "egoh" になり、
    -- それが ++ されるため "hogeegoh" が最終的な答えになることを考えれば納得できるのではなかろうか。

isPalindrome4 :: (Eq a) => [a] -> Bool
isPalindrome4 = (==) <*> reverse
    -- (<*>) :: f (a -> b) -> f a -> f b
    -- <*> は fmap の強化版。fmap が通常の関数（`a -> b`）とファンクター値（`f a`）を引数に取って、関数をファンクター値の中の値に適用してくれる（結果として `f b` が得られる）のに対して、
    -- <*> は関数の入っているファンクター値（`f (a -> b)`）と値の入っているファンクター値（`f a`）を引数に取って、
    -- 1 つ目のファンクターの中身である関数を 2 つ目のファンクターの中身に適用する（結果として `f b` が得られる）のである。
    -- 🤔う〜んよくわからない。Just (++ "!") <*> Just "hoge" はたしかに (++ "!") 関数の入っている Maybe ファンクターと値 "hoge" の入っている Maybe ファンクターを引数に取って
    -- 結果として Just "hoge!" という Maybe ファンクターが得られることはわかるけど
    -- 今回は (==) という関数が入っている___ファンクター（？）と値___の入っている___ファンクター reverse を引数に取って。。。？？？
    -- 手がかり：(++) <*> reverse $ "hoge" は結果が "hogeegoh" になる
    -- 🔥いや待てよ
    -- isPalindrome4 は以下のように書き直せる。こう考えたらわかりやすいかも。。？？？
    -- (==) f は、引数を待ち構えている。この引数とは isPalindrome4' に与えられるリストのことだ（と思う）。f には reverse が入ることになる。。
isPalindrome4' :: (Eq a) => [a] -> Bool
isPalindrome4' = (\f -> (==) f) <*> reverse

    -- ❗️いやさらに待とう！！！！
    -- すごい H 本 11 章の「関数もアプリカティブだよ」によると、関数アプリカティブにおける <*> の実装は以下のとおり
    {-
        instance Applicative ((->) r) where
            pure x = (\_ -> x)
            f <*> g = \x -> f x (g x)
    -}
    -- だから (==) <*> reverse は \x -> (==) x (reverse x) という関数になる。
    -- つーことで、f <*> g = \x -> f x (g x) 自体がわかればこの問題自体は納得できる。

-- <*> の代わりに >>= を使う
isPalindromeM :: (Eq a) => [a] -> Bool
isPalindromeM = reverse >>= (==)

-- これも書き換えてみる。
isPalindromeM' :: (Eq a) => [a] -> Bool
isPalindromeM' = reverse >>= (\f -> (==) f)
    -- >>= は、(文脈に入った値(モナド)) >>= (普通の値を取って文脈に入った値を返す関数) という形で使って、右辺の関数をあたかも「文脈に入った値（モナド）を取ってモナドを返す関数」のごとく使えるというものだった
    -- Just 10 >>= (\x -> Just (x+20)) とすると結果は Just 30
    -- ✋✋✋いや待てよ
    -- 関数モナド（すごいH本 14 章の説明をみると）
    {-
        -- 関数モナドについての説明。
        instance Monad ((->) r) where
            return x = \_ -> x
            h >>= f = \w -> f (h w) w
    -}
    -- だから reverse >>= (==) とすると \w -> (==) (reverse w) w という関数になるんだ。
    -- だから、そもそものモナドの実装 h >>= f = \w -> f (h w) w さえ理解できればこれはわかる。

-- 半分の比較だけをする😲
isPalindrome5 :: (Eq a) => [a] -> Bool
isPalindrome5 xs = p [] xs xs
    where p rev (x:xs') (_:_:ys) = p (x:rev) xs' ys
          p rev (_:xs') [_] = rev == xs'
          p rev xs' [] = rev == xs'
          p _ [] (_:_) = error ""
-- わかりづらさあるが、(_:_:ys) としているから、2 つ目の引数として渡したリストは 2 つずつ削られていくから早くなる。
-- 最初、p [] "abcdEdcba" "abcdEdcba" -> p "a" "bcdEdcba" "cdEdcba" -> p "ba" "cdEdcba" "dEdcba" -> ... というふうに、2 つ目のリストのほうも 1 個ずつ要素がなくなっていくものと勘違いしてた。
-- 今回実装した関数は具体的には以下のようになる。
-- p [] "abcdEdcba" "abcdEdcba"
-- -> p "a" "bcdEdcba" "cdEdcba"
-- -> p "ba" "cdEdcba" "Edcba"
-- -> p "cba" "dEdcba" "cba"
-- -> p "dcba" "Edcba" "a" -- ここで 2 行目のパターンマッチに落ちてきて、"dcba" (rev) と "dcba" (xs') が一致するから True になる


palindrome6 :: (Eq a) => [a] -> Bool
palindrome6 xs = foldr (&&) True $ zipWith (==) xs (reverse xs)
    -- zipWith 以降の部分がミソ。zipWith (+) [1,2,3] [10,20,30] とすれば [11,22,33] という結果が得られるように、
    -- zipWith (==) [1,2,3] [3,2,1] とすれば [False,True,False] という結果になる。
    -- foldr (&&) True [False, True, False] は、True をアキュムレータの初期値として && で畳み込むというもの。
-- 畳み込みの部分は以下のように and 関数で代用できる。
palindrome6' :: (Eq a) => [a] -> Bool
palindrome6' xs = and $ zipWith (==) xs (reverse xs) -- same, but easier

-- 「要素数の半分と先頭から取ったもの」「要素数の半分を先頭から捨てて、捨てた結果（も要素数の半分）を逆順にしたもの」を比較して等しければ回文だよねという素直な解法
-- 要素数が奇数のときの処理で div とか mod を使っている点に注意すれば、あとは容易
isPalindrome7 :: (Eq a) => [a] -> Bool
isPalindrome7 list = take half_len list == reverse (drop (half_len + (len `mod` 2)) list)
    where len = length list
          half_len = len `div` 2 -- `div` であまりを捨てる。13 `div` 2 は 6 になる。

-- これも同様。splitAt 2 "abcde" は ("ab","cde") になる。上のは `drop (half_len + (len `mod` 2)) list` あたりがゴチャつくがそれに比べればスマートだろう、か、、？
isPalindrome7' :: (Eq a) => [a] -> Bool
isPalindrome7' list = f_part == reverse s_part
    where len = length list
          half_len = len `div` 2
          (f_part, s_part') = splitAt half_len list
          s_part = drop (len `mod` 2) s_part'


-- Using Control.Arrow (&&&) fan out operator.
{-
id を適用した値と reverse を適用した値が等しければ(==)回文だよねということだろうし、書いている雰囲気も理解できるけど
&&& って何、ていうのはちゃんとはわからない。Arrow 型クラスについて勉強すべし
https://r-west.hatenablog.com/entry/20070529/1180455881
https://upload.wikimedia.org/wikipedia/commons/5/57/ArrowsConveyors_ampersand2.png

*Main> (id &&& reverse) [1,2,3]
([1,2,3],[3,2,1])

*Main> uncurry (++) ("hoge", "fuga")
"hogefuga"

-}

--With monomorphism restriction:

isPalindrome8 :: (Eq a) => [a] -> Bool
isPalindrome8 xs = (uncurry (==) . (id &&& reverse)) xs

-- Point free with no monomorphism restriction:
isPalindrome8' :: (Eq a) => [a] -> Bool
isPalindrome8' = (uncurry (==) . (id &&& reverse))