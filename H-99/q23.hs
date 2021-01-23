{-# OPTIONS -Wall -Werror #-}

{-
       Extract a given number of randomly selected elements from a list.

       Example in Haskell:

       λ> rnd_select "abcdefgh" 3 >>= putStrLn
       eda
-}

-- うーん、わからない

-- 解答
-- どの解答も、無限リストに対しては使えない
import System.Random
import Control.Monad (replicateM)
import Data.List (nub)

-- これだと aab とか aaa みたいに同じ文字が重複して取り出されることもあるけどいいのか？
rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select l n
       | n < 0 = error "N must be greater than zero."
       | otherwise = do
              pos <- replicateM n $ getStdRandom $ randomR (0, (length l)-1)
              return [l !! p | p <- pos]

{-

🎲 getStdRandom 関数

       ✅ getStdRandom 関数が random 関数を引数に取ると、範囲指定のない整数値の乱数を発生させる
       *Main> getStdRandom random
       -3994297687513298038

       ✅ getStdRandom 関数が randomR 関数を引数に取ると、範囲指定のある乱数を発生させることができる
       *Main> getStdRandom $ randomR (1, 6)
       3   
       -- ⭐️rnd_select3 の解法にも登場するが、randomRIO という関数を使うと、これも簡単に範囲指定のある乱数を生成できる
       randomRIO (1,6)
       > 2

       なお、生成される乱数は IO a 型（つまり、上の 2 とかも Int 型ではなく IO Int 型）
       https://tnomura9.exblog.jp/14366882/

🐛 replicateM 関数

       ✅ アクションに対して使う replicate。指定された回数だけアクションから値を取り出してリスト化する。

       import Control.Monad
       import System.Random

       dice :: IO Int
       dice = getStdRandom $ randomR (1, 6)

       main = do
              print $   replicate  5 1
              print =<< replicateM 5 (return 1)
              print =<< replicateM 5 dice

       -- 結果（毎回異なる）
       [1,1,1,1,1]
       [1,1,1,1,1]
       [4,6,2,1,2]
-}

-- これも同様に重複して取り出されることはある
rnd_select2 :: [a] -> Int -> IO [a]
rnd_select2 xs n = do
       gen <- getStdGen
       return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]

{-
*Main> :t getStdGen 
getStdGen :: IO StdGen

*Main> :t randomRs
randomRs :: (Random a, RandomGen g) => (a, a) -> g -> [a]
-}

rnd_select3 :: [a] -> Int -> IO [a]
rnd_select3 xs n 
       | n < 0     = error "N must be greater than zero."
       | otherwise = replicateM n rand
              where rand = do 
                     r <- randomRIO (0, (length xs) - 1)
                     return (xs !! r)


--- 重複を許さないようにする。
-- Q 20 の removeAt 関数
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)

rnd_select' :: RandomGen g => [a] -> Int -> g -> ([a], g)
rnd_select' _ 0 gen = ([], gen) -- gen は、後続部で定義する rnd_selectIO 関数の実装を見ればわかるとおり getStdRandom 関数から提供される
rnd_select' [] _ gen = ([], gen)
rnd_select' l count gen
       | count == (length l) = (l, gen)
       | otherwise = rnd_select' (snd (removeAt (k+1) l)) count gen' -- 「ランダムに数を取り出して、取り出した数は元のリストから削除してからまたランダムに取り出す」のではなく、
                                                                     -- 「ランダムに数を削除して、元のリストの要素数が取り出したい要素数と同じ数にまで減ったら、その元のリストをぜんぶ取り出す」という考え方。
              where (k, gen') = randomR (0, (length l) - 1) gen

rnd_selectIO :: [a] -> Int -> IO [a]
rnd_selectIO l count = getStdRandom $ rnd_select' l count -- :t getStdRandom は getStdRandom :: (StdGen -> (a, StdGen)) -> IO a である。
                                                          -- getStdRandom 関数を使えば、ランダムシートを必要とする関数にランダムシートを提供できる

{-
rnd_selectIO "abcdefg" 3 >>= putStrLn こういう形で実行する。
-}

-- 上の回答は、元のリストからランダムに要素を削除して残ったやつを所望のリストとして提示する。所望のリストが長い場合は効率がいいけど、長いリストからランダムに数個だけ取り出したい場合などは効率が悪い。
-- そういった場合の効率を求めるなら以下。
-- これは素直。主な処理は rnd_slct というヘルパー関数でやっている。
-- アキュムレータ（初期値 []）を用意してやって、ランダムに数を取り出してはアキュムレータに追加していく。同時に、カウンター（引数として与えられる数字）を減算していって、0 になったら終わり。
-- 最初に出てくる ol, ocount, ogen とかいう引数の名前が意味不明（original list? original count?）なのが一番のつまづきどころ（？）
rnd_select'' :: RandomGen g => [a] -> Int -> g -> ([a], g)
rnd_select'' ol ocount ogen = rnd_slct ol [] ocount ogen
   where
      rnd_slct l acc count gen
         | count == 0 = (acc, gen)
         | otherwise   = rnd_slct (snd (removeAt (k+1) l)) ((l !! k) : acc) (count - 1) gen'
                         where (k, gen') = randomR (0, (length l) - 1) gen

rnd_selectIO' :: [a] -> Int -> IO [a]
rnd_selectIO' l count = getStdRandom $ rnd_select'' l count

-- O(N) アルゴリズム
-- 「ランダムに数を選択する」のではなく、リストの各要素を拾うか捨てるかがランダムに決まる、というイメージ。
-- ランダムに数を選択する方法は素直だが、結果として得たいリストの長さぶん繰り返し抽出処理を行うのでたぶん最悪 O(N^2)
-- 一方、それぞれの数を拾うか捨てるかなら、当然最悪でも O(N)。（リストの各要素に対する処理だから）
-- これ、絶対昇順になるじゃん（最後に並べ替えの処理を書けばいいんだろうけど）
rnd_select4 :: [a] -> Int -> IO [a]
rnd_select4 _ 0 = return []
rnd_select4 (x:xs) n = -- こうやってパターンマッチしてるので、x が主人公の冒険がここから始まるということ
    do r <- randomRIO (0, (length xs)) -- 0 ~ length xs の範囲にあるランダムな数を選ぶ（x くんがくじ引きを引くイメージ）
       if r < n -- 正しいルート（以下）に進んだ場合は、n が減算されるので、r < n という条件により、最終的に n 個の要素が取り出されることを保証している。
           then do -- 正しいルート（イメージ）x が最終結果に残れるパターン
               rest <- rnd_select4 xs (n-1) -- x は一抜けして、残りのリスト（xs）に対して同じことを行う
               return (x : rest)
           else rnd_select4 xs n -- ドボンルート。x は捨てられ、残りのリスト（xs）に対して同じことを行う
rnd_select4 _ _ = error "unknown error"

-- A solution returns random results even when the number of items we want is the same as the number of items in the list:
rnd_select5 :: [a] -> Int -> IO [a]
rnd_select5 _  0 = return []
rnd_select5 [] _ = return []
rnd_select5 xs count = do
       r <- randomRIO (0, (length xs)-1)
       rest <- rnd_select5 (snd (removeAt (r+1) xs)) (count-1)
       return ((xs !! r) : rest)

-- import Data.List (nub)
rnd_select6 :: Int -> [a] -> [a]
rnd_select6 n x = map (x!!) is -- map (x !!) [インデックス] という書き方も面白い。map ("hoge" !!) [1,3] は "oe" になる
       where is = take n . nub $ randomRs (0, length x - 1) (mkStdGen 100)
-- 0 ~ length x - 1 の範囲でランダムに数を抽出し、nub で重複を取り除く。こうして重複のないランダムなインデックスを得たら
-- map (x !!) is で求めたい結果を得る
{-
*Main> nub [1,2,3]
[1,2,3]
*Main> nub [1,1,2]
[1,2]
*Main> nub [1,1,1]
[1]
-}

rnd_select7 :: [a] -> Int -> IO [a]
rnd_select7 lst n = map (lst !!) <$> indices -- indices は IO [Int] だから <$> を使う
       where indices = take n . nub . randomRs (0, length lst - 1) <$> getStdGen

{-
*Main> randomR (0, 10) <$> getStdGen
(2,1442595510 535353314)

*Main> :t (<$>)
(<$>) :: Functor f => (a -> b) -> f a -> f b

*Main> :t randomR (0, 10)
randomR (0, 10) :: (Random a, RandomGen g, Num a) => g -> (a, g)

*Main> :t getStdGen
getStdGen :: IO StdGen
-}