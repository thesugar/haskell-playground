{-# OPTIONS -Wall -Werror #-}

{-
Group the elements of a set into disjoint subsets.

a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? 
Write a function that generates all the possibilities and returns them in a list.

b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). 
However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).

You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".

Example in Haskell:

λ> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
[[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
(altogether 1260 solutions)

λ> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
[[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
(altogether 756 solutions)
-}

--- わからない。解答

-- n 個選ぶ combination 関数をつくる。ただし、n 個選んだときに、捨てた n 個以外の要素の情報も保持しておく。（タプルは、([拾った n 個],[n 個以外の捨てた要素]) になる）
combination :: Int -> [a] -> [([a], [a])]
combination 0 xs = [([], xs)]
combination _ [] = []
combination n (x:xs) = ts ++ ds
   where
      ts = [(x:ys, zs) | (ys, zs) <- combination (n-1) xs] -- 先頭の x を拾う場合は、残りの要素から (n-1) 個を選ぶ
      ds = [(ys, x:zs) | (ys, zs) <- combination n xs] -- 先頭の x を捨てる場合は、残りの要素から n 個を選ぶ

group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n:ns) xs = 
   [ g:gs | (g, rs) <- combination n xs, gs <- group ns rs] -- 先頭のリストには n 個（この n は n:ns の n）の要素が入っていて、先頭ではないリストの残りの部分には、n 個を選ぶときに捨てられた残りの要素を ns（n:ns が [2,2,5] なら ns は [2,5]）で分ける。
   
-- First of all we acknowledge that we need something like combination from the above problem. 
-- Actually we need more than the elements we selected, we also need the elements we did not select. 
-- Therefore we cannot use the tails function because it throws too much information away. 
-- But in general this function works like the one above. 
-- In each step of the recursion we have to decide whether we want to take the first element of the list (x:xs) in the combination 
-- (we collect the possibilities for this choice in ts) or if we don't want it in the combination (ds collects the possibilities for this case).

-- Now we need a function group that does the needed work. 
-- First we denote that if we don't want any group there is only one solution: a list of no groups. 
-- But if we want at least one group with n members we have to select n elements of xs into a group g and the remaining elements into rs. 
-- Afterwards we group those remaining elements, get a list of groups gs and prepend g as the first group.

group' :: [Int] -> [a] -> [[[a]]]
group' [] = const [[]]
group' (n:ns) = concatMap (uncurry $ (. group' ns) . map . (:)) . combination n

group'' :: [Int] -> [a] -> [[[a]]]
group'' [] _ = [[]]
group'' (g:gs) xs = concatMap helper $ combination g xs
          where helper (as, bs) = map (as:) (group'' gs bs)