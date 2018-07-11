{-# OPTIONS_GHC -Wall #-}
module Kladko12 where
import Data.Maybe

data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Show, Ord)

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr 
          deriving (Eq, Show, Ord) 

type TypeTable = [(String, Type)]
type TEnv      = TypeTable    -- тобто [(String, Type)]
type Sub       = TypeTable    -- тобто [(String, Type)]  

-- Задача 1 -----------------------------------------
-- Передумова: Єлемент, що шукається, є в таблиці
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp a abx = (snd.head) (filter (\n -> fst n == a) abx)

-- Задача 2 -----------------------------------------
tryToLookUp :: Eq a => a -> b -> [(a, b)] -> b
tryToLookUp a b abx = if length (filter (\n -> fst n == a) abx) == 0 then b
                                                                     else lookUp a abx
-- Задача 3 -----------------------------------------
reverseLookUp :: Eq b => b -> [(a, b)] -> [a]
reverseLookUp b abx = map fst (filter (\t -> snd t == b) abx)

-- Задача 4 -----------------------------------------
occurs :: String -> Type -> Bool
occurs nm (TVar t) = nm == t
occurs nm (TFun t1 t2) = (occurs nm t1) || (occurs nm t2)
occurs _ _ = False

-- Задача 5 -----------------------------------------
-- Передумова: Немає функцій визначених користувачем (конструктор Fun)
-- Передумова: Всі змінні типів (з виразів) мають зв"язування в середовищі типів 
inferType :: Expr -> TEnv -> Type
inferType (Number _) _ = TInt
inferType (Boolean _) _ = TBool
inferType (Id kc) env' = tryToLookUp kc TErr env'
inferType (Prim kc) _ = tryToLookUp kc TErr primTypes
inferType (Cond a b c) env' | a1 == TErr || not (isBoolean a1) || b1 /= c1 = TErr
                            | otherwise = b1
                      where a1 = inferType a env'
                            b1 = inferType b env'
                            c1 = inferType c env'
inferType (App c v) env' | not (boolFunc temp) = TErr
                         | inferType v env' == p = l
                         | otherwise = TErr
                      where temp = inferType c env'
                            (TFun p l) = temp
inferType (Fun _ xe) env' = inferType xe env'

isBoolean ::Type -> Bool
isBoolean TBool = True
isBoolean _ = False

boolFunc :: Type -> Bool
boolFunc (TFun _ _) = True
boolFunc _ = False

-- Задача 6 -----------------------------------------
applySub :: Sub -> Type -> Type
applySub _ TInt = TInt
applySub _ TBool = TBool
applySub sub' (TFun t1 t2) = TFun (applySub sub' t1) (applySub sub' t2)
applySub sub' (TVar str) = tryToLookUp str (TVar str) sub'
applySub _ TErr = TErr

-- Задача 7 -----------------------------------------
unify :: Type -> Type -> Maybe Sub
unify t t' = unifyPairs [(t, t')] []

unifyPairs :: [(Type, Type)] -> Sub -> Maybe Sub
unifyPairs ((TInt, TInt):xs) sub = unifyPairs xs sub
unifyPairs ((TBool, TBool):xs) sub = unifyPairs xs sub
unifyPairs ((TVar v, TVar v'):xs) sub | v == v' = unifyPairs xs sub
                                      | otherwise = Nothing
unifyPairs ((TVar v, t):xs) sub | occurs v t = Nothing
                                | otherwise  = unifyPairs (map (\(p1,p2) -> (appSub p1, appSub p2)) xs) 
                                                          ((v, t):sub) where appSub = applySub [(v, t)]
unifyPairs ((t, t'@(TVar _)):xs) sub = unifyPairs ((t', t):xs) sub
unifyPairs ((TFun tp1 tp2, TFun tp1' tp2'):xs) sub = unifyPairs ((tp1, tp1'):(tp2, tp2'):xs) sub
unifyPairs [] sub = Just sub
unifyPairs _ _  = Nothing

-- Задача 8 -----------------------------------------
inferPolyType :: Expr -> Type
inferPolyType e =
    let vx   = ['a' : show n | n <- [(1::Int)..]]
        (_, t, _) = inferPolyType' e [] vx    
    in t  

inferPolyType' :: Expr -> TEnv -> [String] -> (Sub, Type, [String])
inferPolyType' (Number _) _ nx = ([], TInt, nx)
inferPolyType' (Boolean _) _ nx = ([], TBool, nx)
inferPolyType' (Id x) env' nx = ([], lookUp x env', nx)
inferPolyType' (Prim x) _ nx = ([], lookUp x primTypes, nx)
inferPolyType' (Fun x e) env' (nx:list) = (sub, res, list')
                                    where (sub, te, list')  = inferPolyType' e ((x, TVar nx):env') list
                                          sth = applySub sub (TVar nx) 
                                          res = if te == TErr then TErr else TFun sth te
inferPolyType' (App f e) env' (nx:list) | isJust resSub = ((combineSubs [fromJust resSub, sub', sub]),
                                                          applySub (fromJust resSub) (TVar nx), list2)
                                        | otherwise = ([], TErr, [])
                                    where (sub, tf, list1) = inferPolyType' f env' list
                                          (sub', te, list2) = inferPolyType' e (updateTEnv env' sub) list1
                                          resSub = unify tf (TFun te (TVar nx)) 

--------------------------------------------------------------------
showT :: Type -> String
showT TInt        = "Int"
showT TBool       = "Bool"
showT (TFun t t') = "(" ++ showT t ++ " -> " ++ showT t' ++ ")"
showT (TVar a)    = a
showT TErr        = "Type error"

-- Типи базових операцій (примітивів)...
primTypes :: TypeTable
primTypes 
  = [("+", TFun TInt (TFun TInt TInt)),
     (">", TFun TInt (TFun TInt TBool)),
     ("==", TFun TInt (TFun TInt TBool)),
     ("not", TFun TBool TBool)]

---------------------------------------------------
-- Допоміжні функції 
updateTEnv :: TEnv -> Sub -> TEnv
updateTEnv tenv tsub = map modify tenv
  where modify (v, t) = (v, applySub tsub t)

combine :: Sub -> Sub -> Sub
combine sNew sOld = sNew ++ updateTEnv sOld sNew

-- В combineSubs [s1, s2,..., sn], s1 повинна бути *самою останньою* 
-- підстановкою і повинна бути застосована *заключною*
combineSubs :: [Sub] -> Sub
combineSubs = foldr1 combine

------------------------------------------------------
-- Вивод мономорфного типу - приклади тестів...
env :: TEnv
env = [("x",TInt),("y",TInt),("b",TBool),("c",TBool)]

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 :: Expr
type1, type2, type3, type4, type5, type6, type7, type8 :: Type

ex1 = Number 9
type1 = TInt

ex2 = Boolean False
type2 = TBool

ex3 = Prim "not"
type3 =  TFun TBool TBool

ex4 = App (Prim "not") (Boolean True)
type4 = TBool

ex5 = App (Prim ">") (Number 0)
type5 = TFun TInt TBool

ex6 = App (App (Prim "+") (Boolean True)) (Number 5)
type6 = TErr

ex7 = Cond (Boolean True) (Boolean False) (Id "c")
type7 = TBool

ex8 = Cond (App (Prim "==") (Number 4)) (Id "b") (Id "c")
type8 = TErr

------------------------------------------------------
-- Приклади для тестування уніфікації (unify)...
u1a, u1b, u2a, u2b, u3a, u3b, u4a, u4b, u5a, u5b, u6a, u6b :: Type
sub1, sub2, sub3, sub4, sub5, sub6 :: Maybe Sub

u1a = TFun (TVar "a") TInt
u1b = TVar "b"
sub1 = Just [("b",TFun (TVar "a") TInt)]

u2a = TFun TBool TBool
u2b = TFun TBool TBool
sub2 = Just []

u3a = TFun (TVar "a") TInt
u3b = TFun TBool TInt
sub3 = Just [("a",TBool)]

u4a = TBool
u4b = TFun TInt TBool
sub4 = Nothing

u5a = TFun (TVar "a") TInt
u5b = TFun TBool (TVar "b")
sub5 = Just [("b",TInt),("a",TBool)]

u6a = TFun (TVar "a") (TVar "a")
u6b = TVar "a"
sub6 = Nothing

------------------------------------------------------
-- Вивод поліморфного типу - приклади тестів...
ex9, ex10, ex11, ex12, ex13, ex14 :: Expr
type9, type10, type11, type12, type13, type14 :: Type

ex9 = Fun "x" (Boolean True)
type9 = TFun (TVar "a1") TBool

ex10 = Fun "x" (Id "x")
type10 = TFun (TVar "a1") (TVar "a1")

ex11 = Fun "x" (App (Prim "not") (Id "x"))
type11 = TFun TBool TBool

ex12 = Fun "x" (Fun "y" (App (Id "y") (Id "x")))
type12 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TVar "a3")) (TVar "a3"))

ex13 = Fun "x" (Fun "y" (App (App (Id "y") (Id "x")) (Number 7)))
type13 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TFun TInt (TVar "a3"))) 
              (TVar "a3"))

ex14 = Fun "x" (Fun "y" (App (Id "x") (Prim "+"))) 
type14 = TFun (TFun (TFun TInt (TFun TInt TInt)) (TVar "a3")) 
              (TFun (TVar "a2") (TVar "a3"))
