{-# OPTIONS_GHC -Wall #-}
module Kladko11 where

import Data.Maybe

-- Всі програми і їх елементи являються вірними (well-formed) в наступному значенні:
--   Всі оператори, функції і процедури застосовуються
--      до вірної кількості аргументів, кожний з яких має відповідний тип.
--   Вирази, які повинні обчислювати логічні значення, будуть завжди обчислюватися
--     до 0 (false) або 1 (true).
--   В присвоєнні масиву  a[i] = e масив завжди визначений (в області дії).
--   Процедура, що викликається як x := p(e1, …, en), завжди буде повертати значення
--     (закінчує своє обчислення оператором return e)
--   Оператор return завжди останній оператор для виконання в блоку процедури
--     (тобто немає "мертвого коду")

--------------------------------------------------------------------
type Id = String
data Value = I Int | A [(Int, Int)]
           deriving (Eq, Show)
data Op = Add | Minus | Mul | Less | Equal | Index
          deriving (Eq, Show)
data Exp = Const Value |
           Var Id |
           OpApp Op Exp Exp |
           Cond Exp Exp Exp |
           FunApp Id [Exp]
         deriving (Eq, Show)

data VarDef  =  Arr Id  | Int Id
               deriving (Eq, Show)
type FunDef  =  (Id, ([VarDef], Exp))

data Scope = Local | Global
           deriving (Eq, Show)
type Binding = (Id, (Scope, Value))
type State = [Binding]

data Statement = Assign Id Exp |
                 AssignA Id Exp Exp |
                 If Exp Block Block |
                 While Exp Block |
                 Call Id Id [Exp] |
                 Return Exp
               deriving (Eq, Show)

type Block     = [Statement]
type ProcDef   = (Id, ([VarDef], Block))
type Program   = ([VarDef], [FunDef], [ProcDef])

-- Задача 1 -----------------------------------------
getValue :: Id -> State -> Value
-- Передумова: Значення змінної Id є в стані State
getValue x state = lookUp x (map (\(key, value) -> (key, snd value)) state)

-- Задача 2 -----------------------------------------
getLocals :: State -> State
getLocals [] = []
getLocals (bind@(_, (Local, _)):xs) = bind : getLocals xs
getLocals ((_, (Global, _)):xs) = getLocals xs

getGlobals :: State -> State
getGlobals [] = []
getGlobals ((_, (Local, _)):xs) = getGlobals xs
getGlobals (bind@(_, (Global, _)):xs) = bind : getGlobals xs

-- Задача 3 -----------------------------------------
assignArray :: Value -> Value -> Value -> Value
-- Аргументи - масив, індекс і (нове) значення відповідно
-- Передумова: Три аргумента (Value)  мають значення відповідного типу
--     (масив (A),  ціле (I) і ціле (I)) відповідно.
assignArray (A array) (I index) (I newVal) = A ((index, newVal) : (filter ((index /=).fst) array))
assignArray val _ _ = val

-- Задача 4 -----------------------------------------
updateVar :: (Id, Value) -> State -> State
updateVar (idd, newVal) [] = [(idd, (Local, newVal))]
updateVar couple@(idd, newVal) (bind@(bidd, (actionScope, _)):xs)
    | idd == bidd = (bidd, (actionScope, newVal)) : xs 
    | otherwise = bind : updateVar couple xs

-- Задача 5 -----------------------------------------
applyOp :: Op -> Value -> Value -> Value
-- Передумова: Значення мають відповідні типи (I або A) для кожної операції
applyOp Index (A []) _ = I 0
applyOp Index (A ((i, val):xs)) val2@(I index) 
 | i == index = I val
 | otherwise = applyOp Index (A xs) val2
applyOp Index _ _ = error "can't support!"
applyOp Add (I x) (I y) = I (x + y)
applyOp Add _ _ = error "can't support!"
applyOp Minus (I x) (I y) = I (x - y)
applyOp Minus _ _ = error "can't support!"
applyOp Mul (I x) (I y) = I (x * y)
applyOp Mul _ _ = error "can't support!"
applyOp Less (I x) (I y) = I (if x < y then 1 else 0)
applyOp Less _ _ = error "can't support!"
applyOp Equal (I x) (I y) = I (if x == y then 1 else 0)
applyOp Equal _ _ = error "can't support!"

-- Задача 6 -----------------------------------------
bindArgs :: [Id] -> [Value] -> State
-- Передумова: списки мають однакову довжину
bindArgs [] _ = []
bindArgs _ [] = []
bindArgs (index:xs) (value:ys) = (index, (Local, value)) : bindArgs xs ys

-- Задача 7 -----------------------------------------
eval :: Exp -> [FunDef] -> State -> Value
eval (Const c) _ _ = c
eval (Var v) _ st = getValue v st
eval (OpApp op x y) dfx st = applyOp op (eval x dfx st) (eval y dfx st)
eval (Cond expIf expThen expElse) dfx st = if (eval expIf dfx st) == (I 1) then (eval expThen dfx st)
                                                                           else (eval expElse dfx st)
eval (FunApp funcId funcEs) dfx st = let fun     = head $ filter ((funcId ==).fst) dfx
                                         funvars = head $ map (fst.snd) [fun]
                                         funexpr = head $ map (snd.snd) [fun]
                                         values  = evalArgs funcEs [fun] st
                                         bindedState = bindArgs (map takeId funvars) values
                                     in (eval funexpr dfx bindedState)
                                     
takeId :: VarDef -> Id
takeId (Arr v) = v
takeId (Int v) = v

evalArgs :: [Exp] -> [FunDef] -> State -> [Value]
evalArgs es dfx st = map (\expr -> eval expr dfx st) es

-- Задача 8 -----------------------------------------
executeStatement :: Statement -> [FunDef] -> [ProcDef] -> State -> State
executeStatement (Assign i e) dfx dpx st = updateVar (i, (eval e dfx st)) st
executeStatement (AssignA i e1 e2) dfx dpx st = updateVar (i, assignArray (getValue i st)
                                                                          (eval e1 dfx st)
                                                                          (eval e2 dfx st)) st
executeStatement (If iff thenn elsee) dfx dpx st = if (eval iff dfx st) == (I 1)
                                                      then (executeBlock thenn dfx dpx st)
                                                      else (executeBlock elsee dfx dpx st)
executeStatement while@(While cond block) dfx dpx st = if (eval cond dfx st) == (I 1)
                                                          then executeStatement while dfx dpx
                                                               (executeBlock block dfx dpx st) 
                                                          else st 
executeStatement (Call targetVar procName argExps) funs procs state = newState
    where
      (procLocals, procBlock) = lookUp procName procs
      procValues = evalArgs argExps funs state
      callingState = (bindArgs (map getVarId procLocals) procValues) ++ getGlobals state
      returnState = executeBlock procBlock funs procs callingState
      modifiedState = getLocals state ++ getGlobals returnState
      newState
        | targetVar /= "" = updateVar newBinding modifiedState
        | otherwise       = modifiedState
      newBinding = (targetVar, (getValue "$res" returnState))


executeBlock :: Block -> [FunDef] -> [ProcDef] -> State -> State
executeBlock [] _  _ st  = st 
executeBlock (stmt:rest) dfx dpx st  = executeBlock rest dfx dpx (executeStatement stmt dfx dpx st )
---------------------------------------------------------------------
-- Допоміжні функції і дані для тестування...
-- Функція пошуку...
lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp x t = fromMaybe (error ("\nСпроба знайти  " ++ show x ++
                      " в таблиці котра має лише звязування: " ++
                      show (map fst t)))
              (lookup x t)

-- Стан для тестування
sampleState :: State
sampleState  = [("x", (Local, I 5)), ("y", (Global, I 2)), ("a", (Global, listToVal [4,2,7]))]

-- Перетворює список цілих в масив Value...
listToVal :: [Int] -> Value
listToVal xs  = A (zip [0..] xs)

 -- Перетворює ціле в Exp...
intToExp :: Int -> Exp
intToExp n
  = Const (I n)

-- Реалізація виконання програми
program :: Program -> State
program (dvx, dfx, dpx) =
   let initv :: VarDef -> Binding
       initv (Arr v) = (v, (Global, A []))
       initv (Int v) = (v, (Global, I 0))
       state = map initv dvx
       ( _, bl) = lookUp "main" dpx
   in  executeBlock bl dfx dpx state

-- fib чиста функція
-- Функція fib, що обчислює число Фібоначчі
-- function  fib(integer n) =
--     (n < 3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib
  = ("fib",
     ([Int "n"], Cond (OpApp Less (Var "n") (Const (I 3)))
                  (Const (I 1))
                  (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const (I 1))])
                             (FunApp "fib" [OpApp Minus (Var "n") (Const (I 2))]))
     )
    )

-- Масив
sampleArray :: Exp
sampleArray
  = Const (listToVal [9,5,7,1])

-- Сума елементів масиву 0..n ...
sumA1 :: ProcDef
sumA1
  = ("sumA1",
     ([Arr "a", Int "n"],
                  [Assign "s" (Const (I 0)),
                   Assign "i" (Const (I 0)),
                   Assign "limit" (OpApp Add (Var "n") (Const (I 1))),
                   While (OpApp Less (Var "i") (Var "limit"))
                         [Assign "s" (OpApp Add (Var "s")
                                                (OpApp Index (Var "a") (Var "i"))),
                          Assign "i" (OpApp Add (Var "i") (Const (I 1)))
                         ],
                   Return (Var "s")]
     )
    )

-- Додавання двох чисел...
gAdd :: ProcDef
gAdd
  = ("gAdd",
     ([Int "x", Int "y"], [Assign "gSum" (OpApp Add (Var "x") (Var "y"))])
    )

-- Повна програма
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],[Call "" "gAdd" [intToExp 5, intToExp 10] ]))])
  