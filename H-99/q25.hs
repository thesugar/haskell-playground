{-# OPTIONS -Wall -Werror #-}

{-
Generate a random permutation of the elements of a list.

Example in Haskell:

λ> rnd_permu "abcdef"
"badcef"
-}

import System.Random
--import Control.Monad (replicateM)
--import Data.List (nub)

-- Q24 の解答例を参考にしようと思ったら、ほぼそのままだった。
-- というわけで、独力で解けたとは言いがたい（Q24 の解答を見てやったから）
-- これって計算量 O(N^2) で、効率悪そうでもあるな。。
rnd_permu :: [a] -> IO [a]
rnd_permu [] = return []
rnd_permu xs = do
       r <- randomRIO (0, (length xs) - 1)
       let remain = take r xs ++ drop (r+1) xs
       rest <- rnd_permu remain
       return ((xs !! r) : rest)

-- 解答
--- q23 の rnd_select を使う
rnd_permu1 :: [a] -> IO [a]
rnd_permu1 xs = rnd_selectIO xs (length xs)

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


-- 再帰
-- 自分の解答では (x:xs) のパターンマッチは使わなかったけど、以下の解答では使っている。
-- x の置き場所をランダムに決めるという発想だけど、この解法も効率の良さでいえば怪しさあり。。
rnd_permu2 :: [a] -> IO [a]
rnd_permu2 [] = return []
rnd_permu2 (x:xs) = do
    rand <- randomRIO (0, (length xs))
    rest <- rnd_permu2 xs
    return $ let (ys,zs) = splitAt rand rest
             in ys ++ (x:zs)

-- 自分の解答と同じ（take, drop を使わずに splitAt を使っているけれども）
-- 解答例にも　WARNING: this may choke long lists　と書いてあるし、長いリストに対してはまともに使えない
rnd_permu' :: [a] -> IO [a]
rnd_permu' [] = return []
rnd_permu' xs = do
    rand <- randomRIO (0, (length xs)-1)
    rest <- let (ys,(_:zs)) = splitAt rand xs
            in rnd_permu' $ ys ++ zs
    return $ (xs!!rand):rest