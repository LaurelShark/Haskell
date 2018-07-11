{-# OPTIONS_GHC -Wall #-}
module Main where
import System.Environment

seqWord :: String -> String
seqWord s = unlines (map saveLines (lines s)) 

saveLines::String -> String
saveLines s = let list = removeDuplicates $ strToList s
              in foldl (\line word -> line ++ " " ++ word) (head list) (tail list)

takeWord::String -> String
takeWord str = takeWhile (/= ' ') str

dropWord::String -> String
dropWord = (safeTail . (dropWhile (/= ' ')))

safeTail::String -> String
safeTail [] = []
safeTail str = tail str

strToList::String -> [String]
strToList [] = []
strToList str = takeWord str : (strToList $ dropWord str)

removeDuplicates::[String] -> [String]
removeDuplicates = foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []

main :: IO ()
main = do args <- getArgs -- gets list of args passed to the console
          if null args then do
            s <- getContents
            putStrLn (seqWord s)
          else
         		do let fname = (args!!0)
         		   s <- (readFile fname)
         		   putStrLn (seqWord s) 



