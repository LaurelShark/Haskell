{-# OPTIONS_GHC -Wall #-}
module Kladko05 where

data AbstractInteger = Zero
                     | Succ AbstractInteger
                     | Pred AbstractInteger
                     deriving (Show, Eq)

-- Задача 1 -----------------------------------------
instance Ord AbstractInteger where
   (<=) Zero Zero = True
   (<=) Zero (Succ _) = True
   (<=) (Succ _) Zero = False
   (<=) Zero (Pred _) = False
   (<=) (Pred _) Zero = True
   (<=) (Pred _) (Succ _) = True
   (<=) (Succ _) (Pred _) = False
   (<=) (Succ n) (Succ m) = n <= m
   (<=) (Pred n) (Pred m) = n <= m
   
-- Задача 2 ----------------------------------------
aiToInteger :: AbstractInteger -> Integer
aiToInteger Zero = 0
aiToInteger (Succ n) = 1 + (aiToInteger n)
aiToInteger (Pred n) = (aiToInteger n) - 1
 
-- Задача 3 -----------------------------------------
plusAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
plusAbs Zero Zero = Zero
plusAbs Zero n = n
plusAbs n Zero = n
plusAbs (Succ n) (Succ m) = Succ $ Succ $ plusAbs n m
plusAbs (Pred n) (Pred m) = Pred $ Pred $ plusAbs n m
plusAbs (Succ n) (Pred m) = plusAbs n m
plusAbs (Pred n) (Succ m) = plusAbs n m

-- Задача 4 -----------------------------------------
timesAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbs Zero _ = Zero
timesAbs _ Zero = Zero
timesAbs (Succ Zero) x = x
timesAbs x (Succ Zero) = x
timesAbs (Pred Zero) (Pred Zero) = Succ Zero
timesAbs (Succ n) x@(Pred Zero) = Pred $ timesAbs n x
timesAbs (Pred n) x@(Pred Zero) = Succ $ timesAbs n x
timesAbs x@(Pred Zero) y = timesAbs y x
timesAbs x@(Pred _) (Pred y) = let minusX = (timesAbs x (Pred Zero))
                               in plusAbs minusX $ timesAbs x y
timesAbs x y = plusAbs x $ timesAbs x $ takeAttr y

takeAttr :: AbstractInteger -> AbstractInteger
takeAttr Zero = Zero
takeAttr (Succ n) = n
takeAttr (Pred n) = n

-- Задача 5 -----------------------------------------
instance Num AbstractInteger  where
    (+)   = plusAbs
    (*)   = timesAbs
    negate x = timesAbs x $ Pred Zero
    fromInteger n | n == 0 = Zero
                  | n < 0  = Pred $ fromInteger $ n + 1
                  | otherwise = Succ $ fromInteger $ n - 1
    abs     Zero = Zero
    abs    x@(Pred _) = negate x
    abs    x@(Succ _) = x
    signum  Zero = Zero
    signum (Pred _) = Pred Zero
    signum (Succ _) = Succ Zero

-- Задача 6 -----------------------------------------
factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial 1 = 1
factorial x = x * factorial (x-1) 

-- Задача  7 -----------------------------------------
data Quaternion = Quaternion Double Double Double Double deriving (Eq)

instance Show Quaternion where
    show (Quaternion h i j k) =
      show h ++
      (if i >= 0 then "+" else "") ++ show i ++ "i" ++
      (if j >= 0 then "+" else "") ++ show j ++ "j" ++
      (if k >= 0 then "+" else "") ++ show k ++ "k"

-- Задача 8 -----------------------------------------
plusQuaternion :: Quaternion -> Quaternion -> Quaternion
plusQuaternion (Quaternion h1 i1 j1 k1) (Quaternion h2 i2 j2 k2) =
  (Quaternion (h1 + h2) (i1 + i2) (j1 + j2) (k1 + k2))

-- Задача 9 -----------------------------------------
timesQuaternion :: Quaternion -> Quaternion -> Quaternion
timesQuaternion (Quaternion h1 i1 j1 k1) (Quaternion h2 i2 j2 k2) =
  (Quaternion (h1 * h2 - i1 * i2 - j1 * j2 - k1 * k2)
              (h1 * i2 + i1 * h2 + j1 * k2 - k1 * j2)
              (h1 * j2 - i1 * k2 + j1 * h2 + k1 * i2)
              (h1 * k2 + i1 * j2 - j1 * i2 + k1 * h2))


--- Задача 10 ----------------------------------------
instance Num Quaternion  where
    (+)   = plusQuaternion
    (*)   = timesQuaternion
    a - b   = a + (fromInteger (-1)) * b
    fromInteger n = Quaternion (fromIntegral n) 0 0 0
    negate quatertion' = (fromInteger 0) - quatertion'
    abs (Quaternion a i j k) = Quaternion (sqrt $ a*a + i*i + j*j + k*k) 0 0 0
    signum quat@(Quaternion a i j k) = let m = takefirst $ abs quat
                                               in Quaternion (a/m) (i/m) (j/m) (k/m)

takefirst :: Quaternion -> Double
takefirst (Quaternion a _ _ _) = a