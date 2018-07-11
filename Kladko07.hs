{-# OPTIONS_GHC -Wall #-}
module Kladko07 where

import Data.List
import Data.Ord

type Index = Int
data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Index, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Index, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

-- Çàäà÷à 1 -----------------------------------------
checkSat :: BDD -> Env -> Bool 
checkSat (1, _) _ = True
checkSat (0, _) _ = False
checkSat (rootId, listOfNodes) env = let node = head $ filter (\n -> fst n == rootId) listOfNodes
                                         refIdx = nodeRefId node
                                         envV = snd $ head $ filter (\tuple -> fst tuple == refIdx) env
                                         sonsId = if envV then (nodeTrue node) else (nodeFalse node)
                                     in checkSat (sonsId, listOfNodes) env

nodeRefId :: BDDNode -> Index 
nodeRefId (_, (i, _, _)) = i

nodeFalse :: BDDNode -> NodeId
nodeFalse (_, (_, n, _)) = n

nodeTrue :: BDDNode -> NodeId
nodeTrue (_, (_, _, n)) = n

-- Çàäà÷à 2 -----------------------------------------
sat :: BDD -> [[(Index, Bool)]]
sat (_, []) = []
sat bdd = filter (not.null) $ buildSat bdd []

buildSat :: BDD -> Env -> [[(Index, Bool)]]
buildSat (1, _) env = [env]
buildSat (0, _) _ = [[]]
buildSat (rootId, listOfNodes) env =  let node = head $ filter (\n -> fst n == rootId) listOfNodes
                                          refId = nodeRefId node
                                      in (buildSat (nodeFalse node, listOfNodes) (env ++ [(refId, False)])) ++
                                         (buildSat (nodeTrue node, listOfNodes) (env ++ [(refId, True)]))

-- Çàäà÷à 3 -----------------------------------------
simplify :: BExp -> BExp
simplify (And (Prim m) (Prim n)) = Prim (m && n)
simplify (Or (Prim m) (Prim n)) = Prim (m || n)
simplify (Not (Prim b)) = Prim $ not b
simplify e = e

-- Çàäà÷à 4 -----------------------------------------
restrict :: BExp -> Index -> Bool -> BExp
restrict inp@(Prim _) _ _  = inp
restrict inp@(IdRef i) index b | i == index = Prim b
                               | otherwise  = inp

restrict (And n m) i b = simplify $ And (restrict n i b) (restrict m i b)
restrict (Or n m) i b  = simplify $ Or (restrict n i b) (restrict m i b)
restrict (Not n) i b   = simplify $ Not (restrict n i b)

-- Çàäà÷à 5 -----------------------------------------
-- Ïåðåäóìîâà: Êîæíà çì³ííà (³íäåêñ) â áóëüîâîìó âèðàç³ (BExp) ç"ÿâëÿºòüñÿ 
--    òî÷íî îäèí ðàç â ñïèñêó ³íäåêñ³â (Index); íåìàº ³íøèõ åëåìåíò³â
buildBDD :: BExp -> [Index] -> BDD
buildBDD e xs = buildBDD' e 2 xs 

buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' (Prim b) _ _ | b = (1, [])
                       | otherwise = (0, [])
buildBDD' e n xs = (n, builder e n xs)

builder :: BExp -> NodeId -> [Index] -> [BDDNode]
builder _ _ [] = []
builder e n (i:[]) = let
                           falseExpr  = restrict e i False
                           trueExpr = restrict e i True
                           leftVal = if falseExpr == (Prim True) then 1 else 0
                           rightVal = if trueExpr == (Prim True) then 1 else 0
                          in [(n, (i, leftVal, rightVal))]
                          
builder e n (i:xs) = let
                           falseExpr  = restrict e i False
                           trueExpr = restrict e i True
                           leftVal = n * 2
                           rightVal = n * 2 + 1
                          in [(n, (i, leftVal, rightVal))] ++ (builder falseExpr leftVal xs) ++
                             (builder trueExpr rightVal xs)
-- Çàäà÷à 6 -----------------------------------------
-- Ïåðåäóìîâà: Êîæíà çì³ííà (³íäåêñ) â áóëüîâîìó âèðàç³ (BExp) ç"ÿâëÿºòüñÿ 
--    òî÷íî îäèí ðàç â ñïèñêó ³íäåêñ³â (Index); íåìàº ³íøèõ åëåìåíò³â
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD = undefined

------------------------------------------------------
-- Ïðèêëàäè äëÿ òåñòóâàííÿ..

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))
b9 = And (IdRef 3) (Or (IdRef 2) (And (Not (IdRef 2)) (IdRef 1)))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])
bdd9 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,0,0)),(9,(3,0,1)),(5,(2,10,11)),(10,(3,0,1)),(11,(3,0,1))])



