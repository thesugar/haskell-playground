{-# OPTIONS -Wall -Werror #-}

import Data.List
import Data.Ord (comparing)
{-
(***) Huffman codes.

We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. 
Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. 
Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word for the symbol S. 
In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. 
The task shall be performed by the predicate huffman/2 defined as follows:

% huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs

Example in Haskell:

λ> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
[('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
-}

-- 解答
data HTree a = Leaf a | Branch (HTree a) (HTree a) deriving (Show)

huffman :: (Ord a, Ord w, Num w) => [(a, w)] -> [(a, String)]
huffman freq = sortBy (comparing fst) $ serialize $ -- ③ htree 関数の結果を serialize 関数に渡す。⑤ serialize 関数の返り値を fst（問題の例で言えば 'a' などの文字）でソートして最終的な結果にする。
               htree $ sortBy (comparing fst) $ [(w, Leaf x) | (x, w) <- freq] -- ① まずは頻度表 freq を (x, w) に束縛して、(w, Leaf x) の形にする。それを w（頻度）でソートし htree 関数に渡す。
  where htree [] = error ""
        htree [(_, t)] = t
        htree ((w1, t1): (w2, t2):wts) =
              htree $ insertBy (comparing fst) (w1 + w2, Branch t1 t2) wts -- ② htree 関数は、ハフマン木を作るときに頻度をもとに親の木をつくる過程を表現している。
        serialize (Branch l r) =
              [(x, '0':code) | (x, code) <- serialize l] ++ [(x, '1':code) | (x, code) <- serialize r] -- ④ serialize 関数は、出来上がったハフマン木を根からたどって、左右に 0, 1 と番号づけしていく過程を表現している。
        serialize (Leaf x) = [(x, "")]

{-
comparing は Data.Ord の関数。
sortBy, insertBy は Data.List の関数。

💡 comparing 
比較する 2 要素（第二・第三引数）に対して、直接の大小ではなく事前にそれぞれに対して第一引数の関数を適用した状態で大小判定結果を戻す。
たとえば、comparing fst は、部分適用によりタプルの先頭要素の大小比較結果を返す関数となるので、sortBy (comparing fst) はタプルの先頭要素を使って、タプル自体を昇順ソートする関数になる。

> comparing fst ('a', 10) ('b', 20)
LT

> sortBy (comparing fst) [(10, "hoge"), (2, "mana"), (100, "fuga"), (40, "popo")]
[(2,"mana"),(10,"hoge"),(40,"popo"),(100,"fuga")]

🔥 insertBy は要素の挿入

> insertBy (comparing fst) (10, "hoge") [(3, "fuga"), (55, "piko")]
[(3,"fuga"),(10,"hoge"),(55,"piko")]

-}