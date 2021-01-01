{-# OPTIONS -Wall -Werror #-}


{-
(**) Extract a slice from a list.

Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included).
Start counting the elements with 1.

Example in Haskell:

λ> slice ['a','b','c','d','e','f','g','h','i','j'] 3 7
"cdefg"
-}

slice :: [a] -> Int -> Int -> [a]
slice _ i k | i > k = error "The first index must be equal or greater than the second one."
slice (x:xs) 1 k = if k == 1 then [x] else x: (slice xs 1 (k-1))
slice (_:xs) i k = slice xs (i-1) (k-1) -- i が 1 になるまでは先頭要素を捨てつつ i も k も減算していく
slice [] _ _ = []

-- シンプルだけど無限リストに対しては使えない slice' [1..] 3 7 とすると [3,4,5,6,7] を期待するところ、[3,4,5,6,7 で処理が止まる
slice' :: [a] -> Int -> Int -> [a]
slice' xs i k = map snd $ filter (\(idx, _) -> idx >= i && idx <= k) $ zip [1..k] xs -- いや、ここを zip [1..k] xs とすれば無限リストにも正しく動作する。そうすると filter 関数内の述語のうち、 idx <= k は自明だからいらなくなる

-- q17 のじぶんの解答風の解答だけどやっぱり reverse を使うことになる
slice'' :: [a] -> Int -> Int -> [a]
slice'' ls i k = reverse $ func i k ls []
       where
              (func start end) (x:xs) acc
                     | start == 1 && end > 1 = (func start (end-1)) xs (x:acc)
                     | start == 1 && end == 1 = x:acc
                     | start > 1 = (func (start-1) (end-1)) xs acc
                     | otherwise = error "hoge"
              func _ _ [] _ = []

-- 解答
-- take, drop を使えば確かに簡単に書ける（ずるい）
slice1 :: [a] -> Int -> Int -> [a]
slice1 xs i k | i > 0 = take (k-i+1) $ drop (i-1) xs
slice1 _ _ _ = error "The first index must be larger than the second one."

-- paranoid version とのこと
slice2 :: [a] -> Int -> Int -> Maybe [a]
slice2 [] _ _ = Just []
slice2 xs k n
       | k == n = Just []
       | k > n || k > length xs || n > length xs || k < 0 || n < 0 = Nothing
       | k == 0 = Just (take n xs)
       | otherwise = Just (drop (k-1) $ take n xs)
              -- これはパラノイドだわ

-- n m の再帰を降ろしていくのはメインになる関数でやって、ヘルパー関数にはインデックスを一つしか渡さないという違いはあるけど、自分の解答 slice'' と似てる。reverse も使ってるし。
slice3 :: [a] -> Int -> Int -> [a]
slice3 lst 1 m = slice3' lst m []
       where
              slice3' _ 0 acc = reverse acc
              slice3' (y:ys) num acc = slice3' ys (num - 1) (y:acc)
              slice3' [] _ _ = []
slice3 (_:xs) n m = slice3 xs (n - 1) (m - 1)
slice3 [] _ _ = []

-- きれいな解答✨
slice4 :: [a] -> Int -> Int -> [a]
slice4 [] _ _  = []
slice4 (x:xs) i k
       | i > 1 = slice4 xs (i - 1) (k - 1)
       | k < 1 = []
       | otherwise  = x:(slice4 xs (i - 1) (k - 1)) -- otherwise と言いつつ（？）、ここがメインの処理。ここの i - 1 は i でもいい（ここに来る時点で i = 1 になっていて、あとは i を 1 のままにしとこうが、i を減らして行こうが関係ない）

-- splitAt を使った解答。これよりは take, drop 使った解答の方がよいだろう
slice5 :: [a] -> Int -> Int -> [a]
slice5 xs i k = chunk
  where
         chop  = snd $ splitAt i' xs          -- Get the piece starting at i
         chunk = fst $ splitAt (k - i') chop  -- Remove the part after k
         i' = i - 1

-- こうすれば splitAt でもワンライナーでかけてしまう
slice6 :: [a] -> Int -> Int -> [a]
slice6 xs i k = snd $ splitAt (i-1) (fst (splitAt k xs))

-- 自分の解答 slice' と同じ。無限リストに対しては使えない。
slice7 :: [a] -> Int -> Int -> [a]
slice7 xs i j = map snd
               $ filter (\(x,_) -> x >= i && x <= j)
               $ zip [1..j] xs -- いや、でもここを [1..j] にすれば無限リストに対しても正しく動くね、そうすると filter 関数にいれる述語中で、 x <= j は自明だからいらなくなる

-- リスト内包を使う
slice8 :: [a] -> Int -> Int -> [a]
slice8 xs i k = [x | (x,j) <- zip xs [1..k], i <= j]

-- take と drop
-- slice1 とほぼ同じ
slice9 :: [a] -> Int -> Int -> [a]
slice9 l i k 
       | i > k = []
       | otherwise = (take (k-i+1) (drop (i-1) l))

-- zip のほどき方の違いだけで、slice7 とかとほぼ同じ
slice10 :: [a] -> Int -> Int -> [a]
slice10 xs a b = fst $ unzip $ filter ((>=a) . snd) $ zip xs [1..b]

-- take, drop を使った解はここまで drop してから take してたけど、take してから drop もできるよねという話
slice11 :: [a] -> Int -> Int -> [a]
slice11 xs i k = drop (i-1) $ take k xs

-- fold 利用
-- 前の問題の解答でも同じようなのがあったと思うけど、アキュムレータとして単なるリストではなう (カウンタ, []) というペアを使っているのがスマートな気がする
-- 一方、やはり foldl で ++ を使うことになるのはどうかなーと。
slice12 :: [a] -> Int -> Int -> [a]
slice12 ls begin end = snd $ foldl helper (1, []) ls
    where helper (i, acc) x = if (i >= begin) && (i <= end) then (i+1, acc ++ [x]) else (i+1, acc)

-- Another simple solution from first principles:
-- first principles ってなんすか？　自分の解答その 1 と似ている。
slice13 :: [a] -> Int -> Int -> [a]
slice13 [] _ _ = []
slice13 _ _ 0 = []
slice13 (x:xs) 1 n = x:(slice13 xs 1 (n-1))
slice13 (_:xs) m n = slice13 xs (m-1) (n-1)