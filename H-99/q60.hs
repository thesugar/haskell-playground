import Data.Maybe (fromJust)
import Data.List (findIndex)

{-
(**) Construct height-balanced binary trees with a given number of nodes

Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain?

Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This question is more difficult. 
Try to find a recursive statement and turn it into a function minNodes that returns the minimum number of nodes in a height-balanced binary tree of height H.

On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have? Write a function maxHeight that computes this.

Now, we can attack the main problem: construct all the height-balanced binary trees with a given number of nodes. Find out how many height-balanced trees exist for N = 15.

Example in Haskell:

λ> length $ hbalTreeNodes 'x' 15
1553
λ> map (hbalTreeNodes 'x') [0..3]
[[Empty],
 [Branch 'x' Empty Empty],
 [Branch 'x' Empty (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty Empty) Empty],
 [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]]
-}

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

-- 解答

-- まず hbalTree (q 59)
hbalTree :: a -> Int -> [Tree a]
hbalTree x = map fst . hbalTree'
    where hbalTree' 0 = [(Empty, 0)]
          hbalTree' 1 = [(Branch x Empty Empty, 1)]
          hbalTree' n =
              let t = hbalTree' (n-2) ++ hbalTree' (n-1) -- 高さが最大でも n。ただし、一番上の根要素を除くと高さは最大 n-1。hbalTree は左右の高さが 1 異なることはありうるから n-2 も考えている。
              in [(Branch x lb rb, h) | (lb, lh) <- t, (rb, rh) <- t
                                        , let h = 1 + max lh rh, h == n]

hbalTreeNodes :: a -> Int -> [Tree a]                             
hbalTreeNodes _ 0 = [Empty]
hbalTreeNodes x n = concatMap toFilteredTrees [minHeight .. maxHeight]
  where toFilteredTrees = filter ((n ==) . countNodes) . hbalTree x -- 高さがバランスしている hbalTree のうち、ノード数が n に等しいものをフィルタしている。

        minNodesSeq = 0:1:zipWith((+).(1+)) minNodesSeq (tail minNodesSeq) -- Similar to the Fibonacci sequence but adds 1 in each step.（＊）
        minNodes = (minNodesSeq !!)

        -- ノード数 n のときにつくれる最小の高さと最大の高さを求めている
        -- そうしないとすべての高さに対してノード数 n でバランス木を作れるか試そうとするので処理が終わらない
        -- でも難しい
        minHeight = ceiling $ logBase 2 $ fromIntegral (n+1)
        maxHeight = (fromJust $ findIndex (> n) minNodesSeq) - 1

        -- 以下はノード数を数える補助関数。これは簡単。
        countNodes Empty = 0
        countNodes (Branch _ l r) = countNodes l + countNodes r + 1

{-
（＊）minNodesSeq について。

高さ h の高さ平衡木のうち【ノードの数が最小】なものは、例えば
1. てっぺんのノード一個
2. 高さ (h-1) の左の（最小）サブツリー
3. （高さ平衡を保つため、と最小を満たすためには）高さ (h-2) の右の（最小）サブツリー

で作ることが出来る。これがフィボナッチぽく minNodesSeq を構成している理由。but adds 1 と書いてあるのも、てっぺんのノード一個のこと。
-}

-- 別解
-- ほしい木をそのまま作ってしまう解答。
-- maximum number of nodes in a weight-balanced tree of height h
maxNodes :: Int -> Int
maxNodes h = 2^h - 1

-- minimum height of a weight-balanced tree of n nodes
minHeight :: Int -> Int
minHeight n = ceiling $ logBase 2 $ fromIntegral (n+1)

-- minimum number of nodes in a weight-balanced tree of height h
minNodes :: Int -> Int
minNodes h = fibs !! (h+2) - 1

-- maximum height of a weight-balanced tree of n nodes
maxHeight :: Int -> Int
maxHeight n = length (takeWhile (<= n+1) fibs) - 3

-- Fibonacci numbers
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

hbalTreeNodes' :: a -> Int -> [Tree a]
hbalTreeNodes' x n = [t | h <- [minHeight n .. maxHeight n], t <- baltree h n]
  where
        -- baltree h n = weight-balanced trees of height h with n nodes
        -- assuming minNodes h <= n <= maxNodes h
        baltree 0 n = [Empty]
        baltree 1 n = [Branch x Empty Empty]
        baltree h n = [Branch x l r |
                (hl,hr) <- [(h-2,h-1), (h-1,h-1), (h-1,h-2)],
                let min_nl = max (minNodes hl) (n - 1 - maxNodes hr),
                let max_nl = min (maxNodes hl) (n - 1 - minNodes hr),
                nl <- [min_nl .. max_nl],
                let nr = n - 1 - nl,
                l <- baltree hl nl,
                r <- baltree hr nr]