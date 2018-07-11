{-# OPTIONS_GHC -Wall #-}
module Kladko03 where

data BinomTree a = Node a Int [BinomTree a]
               deriving (Eq, Ord, Show)
type BinomHeap a = [BinomTree a]

-- Задача 1 -----------------------------------------
combineTrees :: Ord a => BinomTree a -> BinomTree a -> BinomTree a
combineTrees (Node a1 degree1 bintr1) (Node a2 degree2 bintr2)
    | (degree1 /= degree2) = (error "We can't compare trees with different degrees")
    | (a1 <= a2) = (Node a1 (degree1 + 1) ((Node a2 degree2 bintr2) : bintr1))
    | otherwise = (Node a2 (degree2 + 1) ((Node a1 degree1 bintr1) : bintr2))

-- Задача 2 -----------------------------------------
extractMin :: Ord a => BinomHeap a -> a
extractMin [] = error "Heap is empty"
extractMin (x:xs) = nodeVal (foldl minNode x xs)

nodeVal :: BinomTree a -> a
nodeVal (Node a _ _) = a

-- function returns the tree with min node value
minNode :: Ord a => BinomTree a -> BinomTree a -> BinomTree a
minNode tree1 tree2
    | nodeVal tree1 > nodeVal tree2 = tree2
    | otherwise = tree1

-- Задача 3 -----------------------------------------
mergeHeaps :: Ord a => BinomHeap a -> BinomHeap a -> BinomHeap a
mergeHeaps binh1 [] = binh1
mergeHeaps [] binh2 = binh2
mergeHeaps (x:xs) (y:ys) | degree x > degree y = y : (mergeHeaps (x:xs) ys)
                         | degree x < degree y = x : (mergeHeaps xs (y:ys))
                         | otherwise = mergeHeaps (mergeHeaps xs ys) [(combineTrees x y)] 

-- function returns degree of the tree
degree :: BinomTree a -> Int
degree (Node _ n _) = n

-- Задача 4 -----------------------------------------
insert :: Ord a => a -> BinomHeap a -> BinomHeap a
insert val h = mergeHeaps h [(Node val 0 [])]

-- Задача 5 -----------------------------------------
deleteMin :: Ord a => BinomHeap a -> BinomHeap a
deleteMin h = let bintree = (filter (\t -> (extractMin h) == nodeVal t) h)!!0
              in mergeHeaps (reverse (childelem bintree)) (filter (/= bintree) h)

-- function returns child elements of the tree
childelem :: BinomTree a -> BinomHeap a
childelem (Node _ _ list) = list

-- Задача 6 -----------------------------------------
binomSort :: Ord a => [a] -> [a]
binomSort [] = [] 
binomSort l = addl (foldr insert [] l) 
 
addl :: Ord a => BinomHeap a -> [a] 
addl [] = [] 
addl binomh = (extractMin binomh) : (addl (deleteMin binomh))

-- Задача 7 -----------------------------------------
toBinary :: BinomHeap a -> [Int] 
toBinary [] = [] 
toBinary h = map (\n -> (length (filter (\bintree -> n == degree bintree) h))) 
                           (reverse [0..(foldl (max) 0 (map degree h))])


-----------------------------------------------------  
-- Приклади деяких дерев...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinomTree Int
--  Зауваження: t7 - результат злиття t5 і t6

-- t1 .. t4 з'являються на Мал. 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 і t6 зліва на Мал.2; t7 - справа на Мал.2
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- Додаткове дерево...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Приклади деяких куп...

h1, h2, h3, h4, h5, h6, h7 :: BinomHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 показана на Мал.3...
h3 = [t1, t2, t4]

-- Дві додаткові купи використовуються далі. Вони зліва на Мал.4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 - результат злиття h4 і h5, справа на Мал.4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 показана на Мал.5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]  