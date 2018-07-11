{-# OPTIONS_GHC -Wall #-}
module Kladko02 where

-- Код - просто список символів - десяткових цифр '0' ..'9'
type Code = String

-- Крок гри (Move) будує конструктор Move використовуючи спробу (Code) і два цілих:  
--    кількість "биків" і "корів"  у пропозиції-спробі по відношенню до коду-числа 
data Move = Move Code Int Int
          deriving (Show, Eq)

-- Задача 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
exactMatches _ [] = 0
exactMatches [] _ = 0
exactMatches (x:cd) (y:att) | (x == y)    = 1 + (exactMatches cd att)
                            | otherwise = 0 + (exactMatches cd att)

-- Задача 2 -----------------------------------------
countDigits :: Code -> [Int]
countDigits cd = (map (\n -> 
                          (conditionfor (\chr ->
                                       (calculateone chr) == n) cd))
                      [0..9])


conditionfor :: (Char -> Bool ) -> Code -> Int
conditionfor p xs = length (filter p xs)


calculateone :: Char -> Int
calculateone bin = fromEnum bin - fromEnum '0'

-- Задача 3 ----------------------------------------- 
matches :: Code -> Code -> Int
matches cd att = foldl (+) 0
                     (everynum (countDigits cd) (countDigits att))

everynum :: [Int] -> [Int] -> [Int]
everynum _ [] = []
everynum [] _ = []
everynum (x:xs) (y:ys) = (min x y) : (everynum xs ys)
 
-- Задача 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove cd att = let
                   bulls = (exactMatches cd att)
                   alls = (matches cd att)
                 in (Move att bulls (alls - bulls))


-- Задача 5 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent (Move att bulls alls) cd = (Move att bulls alls) == (getMove cd att)

-- Задача 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes mv cdx = filter (\cd -> isConsistent mv cd)
                            cdx

-- Задача 7 -----------------------------------------
allCodes :: Int -> [Code]
allCodes n = extand n []

extand :: Int -> [String] -> [String]
extand 0 lst = lst
extand n []  = extand (n - 1)
                  (addingNum [])
extand n lst = extand (n - 1)
                  (concat
                      (map (\str -> addingNum str)
                           lst))

addingNum :: String -> [String]
addingNum str = map (\chr -> str ++ [chr])
                    ['0','1','2','3','4','5','6','7','8','9']

   
-- Задача 7 -----------------------------------------
solve :: Code -> [Move]
solve cd = resulting cd (allCodes (length cd))

resulting :: Code -> [Code] -> [Move]
resulting cd cds = let mv = getMove cd (head cds)
                     in  mv : 
                         (if (lastcheck mv) then []
                          else (resulting cd (filterCodes mv (tail cds))))

lastcheck :: Move -> Bool
lastcheck (Move att f p) = (f == (length att)) && (p == 0)
 
