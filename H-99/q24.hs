{-# OPTIONS -Wall -Werror #-}

{-
Lotto: Draw N different random numbers from the set 1..M.

Example in Haskell:

λ> diff_select 6 49
[23,1,17,33,21,37]
-}

import System.Random
import Control.Monad (replicateM)
import Data.List (nub)

-- ❌これだと、相異なる数を取り出せない
diff_select :: Int -> Int -> IO [Int]
diff_select n m = replicateM n $ getStdRandom $ randomR (1, m)

-- う〜ん、修行が足りない

-- 解答

-- 数を取り出しては、take, drop でその数を除外したリストを作成し、そこからまた取り出すということを繰り返す。
-- do を使っているからというか IO たる乱数を使っているから当然ちゃあ当然だけど、手続き型っぽい書き方になるな
--import System.Random
diff_select1 :: Int -> Int -> IO [Int]
diff_select1 n to = diffPicker n [1..to]
       where
              diffPicker 0 _ = return []
              diffPicker _ [] = error "too few elements to choose from"
              diffPicker n' xs = do
                     r <- randomRIO (0, (length xs)-1)
                     let remaining = take r xs ++ drop (r+1) xs
                     rest <- diffPicker (n'-1) remaining
                     return ((xs !! r) : rest)

-- また、Q23 の結果を使えば今回の問題は自明

diff_select2 :: Int -> Int -> IO [Int]
diff_select2 n to = rnd_selectIO [1..to] n

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)

rnd_select :: RandomGen g => [a] -> Int -> g -> ([a], g)
rnd_select ol ocount ogen = rnd_slct ol [] ocount ogen
   where
      rnd_slct l acc count gen
         | count == 0 = (acc, gen)
         | otherwise   = rnd_slct (snd (removeAt (k+1) l)) ((l !! k) : acc) (count - 1) gen'
                         where (k, gen') = randomR (0, (length l) - 1) gen

rnd_selectIO :: [a] -> Int -> IO [a]
rnd_selectIO l count = getStdRandom $ rnd_select l count

-----
-- 重複を許してしまうじゃん。。
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
       gen <- getStdGen
       return $ take n $ randomRs (1, m) gen

diffSelect' :: Int -> Int -> [Int]
diffSelect' n m = take n . nub . randomRs (1, m) $ mkStdGen 100

diffSelect'' :: Int -> Int -> IO [Int]
diffSelect'' n m = take n . nub . randomRs (1, m) <$> getStdGen

{-
nub :: Eq a => [a] -> [a]

だから

randomRs :: (Random a, RandomGen g) => (a, a) -> g -> [a]

の結果を引数に取ることはできるけど、

randomRIO :: Random a => (a, a) -> IO a
や
getStdRandom :: (StdGen -> (a, StdGen)) -> IO a
は IO a が返り値の型なので、少なくともこの結果を直接引数として nub に渡すことはできない
（まあでも >>= なりアプリカティブスタイルなり使えばいけそうな気はする）
-}