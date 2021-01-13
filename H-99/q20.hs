-- {-# OPTIONS -Wall -Werror #-}


{-
(*) Remove the K'th element from a list.

Example in Haskell:

λ> removeAt 2 "abcd"
('b',"acd")
-}

-- 基本的な再帰。let ~ in のところがほんのちょっとの工夫要素ではあるが、まあタプルを使った再帰ってこういう書き方になるよね。。
removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = let (elm, ls) = removeAt (n-1) xs in (elm, x:ls) 

-- 🎉 foldr を使った書き方。解答例にも似たようなのがない！　2 週間ほど前はこれ書けなかっただろう。
removeAt' :: Int -> [a] -> (a, [a])
removeAt' n xs = let (first, second, third) = foldr f (length xs, [], []) xs in (head second, third)
       where f elm (idx, forHead, ls) = if idx == n then (idx - 1, elm:forHead, ls) else (idx - 1, forHead, elm:ls)

-- いちばん素直？な書き方。解答例では Maybe モナドを使ったりして安全策を取っているものもある。
removeAt'' :: Int -> [a] -> (a, [a])
removeAt'' n xs = (xs !! (n-1), take (n-1) xs ++ drop n xs)


-- 解答

-- splitAt を使った解き方
removeAt1 :: Int -> [a] -> (a, [a])
removeAt1 k xs = case back of
       [] -> error "removeAt: index too large"
       x:rest -> (x, front ++ rest)
       where (front, back) = splitAt (k - 1) xs

-- 自分の解答 removeAt'' と同じ
removeAt2 n xs = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)

-- それの Maybe バージョン
removeAt3 n xs | n > 0 && n <= length xs = (Just (xs !! index), take index xs ++ drop n xs)
               | otherwise = (Nothing, xs)
                     where index = n - 1

-- リスト内包を使ったバージョン
removeAt4 :: (Enum a) => Int -> [a] -> (a, [a])
removeAt4 n xs = ((xs !! (n-1)), [ x | (i, x) <- zip [1..] xs, i /= n ])

-- これは、Maybe モナドを使っているかどうかという違いはあるが、自分の解答 removeAt とほぼ同じ
removeAt5 :: Int -> [a] -> (Maybe a, [a])
removeAt5 _ [] = (Nothing, [])
removeAt5 1 (x:xs) = (Just x, xs)
removeAt5 k (x:xs) = let (a, r) = removeAt5 (k - 1) xs in (a, x:r)

-- こちらは、Maybe モナドと splitAt を使っている
removeAt6 :: Int -> [a] -> (Maybe a, [a]) 
removeAt6 _ [] = (Nothing, [])
removeAt6 0 xs = (Nothing, xs)
removeAt6 nr xs 
       | nr > length xs = (Nothing, xs)
       | otherwise = (Just (xs !! nr), fst splitted ++ (tail . snd) splitted)
              where splitted = splitAt nr xs


removeAt7 :: Int -> [a] -> (a, [a])
removeAt7 n xs = let (front, back) = splitAt n xs in (last front, init front ++ back)

-- ポイントフリースタイル
removeAt8 n = (\(a, b) -> (head b, a ++ tail b)) . splitAt (n - 1)

-- シンプルな再帰。let in を where で代用しているというだけで、自分の解答 removeAt とかと基本は同じだよね
removeAt9 1 (x:xs) = (x, xs)
removeAt9 n (x:xs) = (l, x:r)
       where (l, r) = removeAt (n - 1) xs