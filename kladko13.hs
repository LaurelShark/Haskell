{-# OPTIONS_GHC -Wall #-}
module Kladko13 where

import Data.List

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Задача 1 -----------------------------------------
simplify :: RE -> RE   
simplify (Plus re) = (Seq re (Rep re)) 
simplify (Opt re) = (Alt re Null)
simplify (Seq reg1 reg2) = Seq (simplify reg1) (simplify reg2)
simplify (Alt reg1 reg2) = Alt (simplify reg1) (simplify reg2)
simplify (Rep re) = Rep (simplify re)
simplify re = re

-- Задача 2 -----------------------------------------
startState     :: Automation -> State
terminalStates :: Automation -> [State]
transitions    :: Automation -> [Transition] 

startState (beg, _, _) = beg
terminalStates (_, fin, _) = fin
transitions (_, _, nxt) = nxt

-- Задача 3 -----------------------------------------
isTerminal :: State -> Automation -> Bool 
isTerminal s aut = elem s (terminalStates aut)

-- Задача 4 -----------------------------------------
transitionsFrom :: State -> Automation -> [Transition]
transitionsFrom s aut = filter(\(st, _, _) -> st == s) (transitions aut) 

-- Задача 5 -----------------------------------------
labels :: [Transition] -> [Label]
labels trx = nub (filter (/= Eps) (map (\(_, _, t) -> t) trx)) 

-- Задача 6 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts aut st = accepts' (startState aut) st
  where
    accepts' s st
      | isTerminal s aut && null st = True
      | otherwise = any (try st) (transitionsFrom s aut)
        where
          try st (_, s', Eps) = accepts' s' st
          try [] (_, _, C _)  = False
          try (x:xs) (_, s', C c)
            | c == x   = accepts' s' xs
            | otherwise = False

-- Задача 7 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make Null beg fin nxt = ([(beg, fin, Eps)], nxt)
make (Term c) beg fin nxt = ([(beg, fin, C c)], nxt)
make (Seq r1 r2) beg fin nxt = ((nxt, nxt + 1, Eps) : t1 ++ t2, k2)
   where
     (t1, k1) = make r1 beg nxt (nxt + 2)
     (t2, k2) = make r2 (nxt + 1) fin k1
make (Alt r1 r2) beg fin nxt = ((beg, nxt, Eps) : (nxt + 1, fin, Eps) : (beg, nxt + 2, Eps) : (nxt + 3, fin, Eps) : t1 ++ t2, k2)
   where
     (t1, k1) = make r1 nxt (nxt + 1) (nxt + 4)
     (t2, k2) = make r2 (nxt + 2) (nxt + 3) (k1)
make (Rep r) beg fin nxt = ((beg, nxt, Eps) : (beg, fin, Eps) : (nxt + 1, fin, Eps) : (nxt + 1, nxt, Eps) : t1, k1)
   where
     (t1, k1) = make r nxt (nxt + 1) (nxt + 2)

-- Задача 8 -----------------------------------------
-- Передумова: Довільний цикл в НСА включає хоча б один не-Eps перехід.  
getFrontier :: State -> Automation -> [Transition]
getFrontier = undefined

groupTransitions :: [Transition] -> [(Label, [State])]
groupTransitions = undefined

makeDA' :: Automation -> [State] -> [MetaState] -> [MetaTransition] 
                   -> (MetaState, [MetaState], [MetaTransition])
makeDA' = undefined  

makeDA :: Automation -> Automation
makeDA  = undefined
-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigure, re1, re2, re3, re4, re5 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Alt (Term 'a') Null) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

ndaFigure, nda1, nda2, nda3, nda4, nda5 :: Automation
daFigure, da1, da2, da3, da4, da5 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

