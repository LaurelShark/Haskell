{-# OPTIONS_GHC -Wall #-}
module Kladko08 where

import Data.List (sort)

data SuffixTree = Leaf Int | Node [(String, SuffixTree)]
                deriving (Eq, Ord, Show)

-- ������ 1 -----------------------------------------
isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) | x == y = isPrefix xs ys
                       | otherwise = False

-- ������ 2 -----------------------------------------
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition string1 string2 = partBuilder string1 string2 []

partBuilder :: Eq a => [a] -> [a] -> [a] -> ([a], [a], [a])
partBuilder [] string2 accumulator = (accumulator, [], string2)
partBuilder string1 [] accumulator = (accumulator, string1, [])
partBuilder (x:xs) (y:ys) accumulator | x == y = partBuilder xs ys (accumulator ++ [x])
                                      | otherwise = (accumulator, x:xs, y:ys)

-- ������ 3 -----------------------------------------
suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes string@(_:xs) = string : suffixes xs

-- ������ 4 -----------------------------------------
isSubstring :: String -> String -> Bool
isSubstring string1 string2 = any (isPrefix string1) (suffixes string2)

-- ������ 5 -----------------------------------------
findSubstrings :: String -> String -> [Int]
findSubstrings [] _ = []
findSubstrings string1 string2 = map fst $ filter ((isPrefix string1).snd) $ zip [0..] $ (suffixes) string2

-- ������ 6 -----------------------------------------
getIndices :: SuffixTree -> [Int]
getIndices (Leaf n) = [n]
getIndices (Node couples) = sort $ foldl (++) [] $ map (getIndices.snd) couples

-- ������ 7 -----------------------------------------
findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' _ (Leaf n) = [n]
findSubstrings' string (Node couples) = head $ filter (not.null) $ map
    (\couple -> let a = fst couple
                    node = snd couple
               in (if isPrefix string a then getIndices node
                    else (if isPrefix a string then findSubstrings' (drop (length a) string) node
                          else []))) couples

-- ������ 8 -----------------------------------------
insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (_, _) sufftree@(Leaf _) = sufftree
insert (s, n) (Node [])         = Node [(s, Leaf n)]
insert (s, n) (Node (st@(a, tree):couples))
  | (null.takefirst) (partition a s) 
     = Node (st:(takeNValue (insert (s, n) (Node couples))))
  | isPrefix a s                = let s_p = drop (length a) s
                                  in (Node ((a, insert (s_p, n) tree):couples))
  | otherwise                   = let (back, middle, forward) = partition a s
                                  in (Node ((back, (Node [(middle, tree), (forward, Leaf n)])):couples))

takeNValue :: SuffixTree -> [(String, SuffixTree)]
takeNValue (Node t) = t
takeNValue (Leaf _) = error "Can not support!"

takefirst :: (m,m,m) -> m
takefirst (m, _, _) = m

-- This function is given
buildTree :: String -> SuffixTree
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])


-- ������ 9 -----------------------------------------
longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring = undefined


------------------------------------------------------
-- �������� ������ � ��������� �����..

s1 :: String
s1 = "banana"

s2 :: String
s2  = "mississippi" 

t1 :: SuffixTree
t1 = Node [("banana", Leaf 0),
          ("a", Node [("na", Node [("na", Leaf 1),
                                   ("", Leaf 3)]),
                     ("", Leaf 5)]),
          ("na", Node [("na", Leaf 2),
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 = Node [("mississippi", Leaf 0),
          ("i", Node [("ssi", Node [("ssippi", Leaf 1),
                                    ("ppi", Leaf 4)]),
                      ("ppi", Leaf 7),
                      ("", Leaf 10)]),
          ("s", Node [("si", Node [("ssippi", Leaf 2),
                                   ("ppi", Leaf 5)]),
                      ("i", Node [("ssippi", Leaf 3),
                                  ("ppi", Leaf 6)])]),
          ("p", Node [("pi", Leaf 8),
                      ("i", Leaf 9)])]
