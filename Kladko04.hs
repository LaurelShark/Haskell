{-# OPTIONS_GHC -Wall #-}
module Kladko04 where

import Data.Char

type Name = String
type Attributes = [(Name, String)]
data XML = Text String | Element Name Attributes [XML]
         deriving (Eq, Show)
type Stack = [XML]

-- Задача 1 -----------------------------------------
skipSpace :: String -> String
skipSpace [] = []
skipSpace s@(h:ht) | isSpace h = skipSpace ht
                   | otherwise = s

-- Задача 2 -----------------------------------------
getAttribute :: String -> XML -> String
getAttribute _ (Text _) = ""
getAttribute s (Element _ attributes' _) =
  let pairs = filter ((== s).fst) attributes'
  in if not$null pairs then snd$head pairs 
    else ""

-- Задача 3 -----------------------------------------
getChildren :: String -> XML -> [XML]
getChildren _ (Text _) = []
getChildren s (Element _ _ childelem) = filter ((== s).takeName) childelem

takeName :: XML -> String
takeName (Text _) = ""
takeName (Element s _ _) = s

-- Задача 4 -----------------------------------------
getChild :: String -> XML -> XML
getChild s xml = let child = getChildren s xml
                    in if not$null child then head child
                       else Text ""

-- Задача 5 -----------------------------------------
addChild :: XML -> XML -> XML
-- Передумова: другий аргумент - завжди побудований конструктором Element
addChild _ (Text _) = error "We cannot compare this types"
addChild xml1 (Element s attributes' child) = (Element s attributes' (child ++ [xml1]))

-- Задача 6 -----------------------------------------
getValue :: XML -> XML
getValue t@(Text _) = t
getValue (Element _ _ []) = Text ""
getValue (Element _ _ child) =
    let txt = foldl (++) "" $ map (takeIn.getValue) child in Text txt

takeIn :: XML -> String
takeIn (Text txt) = txt
takeIn (Element _ _ _) = ""

-- Задача 7 -----------------------------------------
addText :: String -> Stack -> Stack
-- Передумова: Є по крайній мірі один елемент Element в стеку
addText _ [] = error "We cannot do this operation"
addText s (h:ht) = (addChild (Text s) h) : ht

-- Задача 8 -----------------------------------------
popAndAdd :: Stack -> Stack
-- Передумова: Є по крайній мірі два елемента Elements в стеку
popAndAdd [] = error "We cannot do this operation"
popAndAdd (_:[]) = error "We cannot do this operation"
popAndAdd (s:(h:ht)) = addChild s h : ht

-- Початковий елемент стеку
sentinel :: XML
sentinel = Element "" [] []

-- Задача 9 -----------------------------------------
parseAttributes :: String -> (Attributes, String)
-- Передумова: Рядок, що містить XML-атрибути, синтаксично вірний
parseAttributes [] = ([], "")
parseAttributes s =
  let
    pair = cutAfter s '>'
    attributes' = (parseName.skipSpace.fst) pair
    name = fst attributes'
    cont = snd pair
    cnt = length $ filter (== '=') $ snd attributes'
    cutTwoTimes = (cutAfter (snd $ cutAfter (snd attributes') '\"') '\"')
    value = skipSpace $ filter (\c -> notElem c "/=") $ fst cutTwoTimes
    attsRest = snd cutTwoTimes
  in if cnt > 1
     then ((name, value) : (fst $ parseAttributes attsRest), cont)
     else ([(name, value)], cont)


cutAfter :: String -> Char -> (String, String)
cutAfter str sep =
  let
    pair = break (== sep) str
    val2 = if (null.snd) pair then "" else (tail.snd) pair
  in (fst pair, val2)

-- Аналіз імені елемента/атрибута
parseName :: String -> (Name, String)
parseName []
  = error "Error: attempt to read empty name"
parseName s@(c1 : _)
  | isAlpha c1 = break (not . isNameChar) s
  | otherwise = error ("parseName error: name " ++ show s ++
                      " must begin with a letter")
  where
    isNameChar c = isAlpha c || isDigit c || elem c "-."

-- Задача 10 -----------------------------------------
parse :: String -> XML
-- Передумова: рядок, що містить XML-документ, синтаксично вірний
parse s = parse' (skipSpace s) [sentinel]

parse' :: String -> Stack -> XML
parse' _ [] = error "You wrote wrong charachers"
parse' [] st = head $ takeChilds $ head st
parse' [_] (_:_) = error "You wrote wrong charachers"
parse' str@(x:substr@(y:_)) st | x == '<' &&
                                 y == '/' =  let right = snd $ cutAfter str '>'
                                 in parse' right $ popAndAdd st
                               | x == '<' =  let
                                 pair = cutAfter substr '>'
                                 left = fst pair
                                 right = snd pair
                                 name = fst $ parseName left
                                 attributes' = fst $ parseAttributes $ drop (length name) left
                                 in parse' right ((Element name attributes' []):st)
                               | otherwise = let
                                 pair = break (== '<') str
                                 left = fst pair
                                 right = snd pair
                                 in parse' right $ popAndAdd $ (Text left):st


takeChilds :: XML -> [XML]
takeChilds (Text _) = error "Empty"
takeChilds (Element _ _ xmlList) = xmlList

-----------------------------------------------------------------------
-- Деякі корисні функції перетворення в рядок і виводу
-- Функція перетворення в рядок ('show' function) для XML об'єктів
showXML :: XML -> String
showXML (Text t) = t
showXML (Element n as es)
     = "<" ++ n ++ showAtts as ++ ">" ++ concatMap showXML es ++ "</" ++ n ++ ">"
       where
          showAtts ast = concatMap showAtt ast
          showAtt (n1, v) = " " ++ n1 ++ "=" ++ "\"" ++ v ++ "\""
-- Функція перетворення в рядок ('show' function) для списку XML об'єктів
showXMLs :: [XML] -> String
showXMLs = concatMap showXML
-- Функція виводу XML об'єкта на екран
printXML :: XML -> IO()
printXML = putStrLn . showXML

-------------------------------------------------------------------------
-- Тестові дані
-- Прості тести XML-об'єктів (без проміжків)
s1, s2, s3 :: String
s1 = "<a>A</a>"
s2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
s3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>"
-- Результати аналізу попередніх XML-об'єктів
x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a"
            []
            [Element "b"
                     []
                     [Element "c"
                              [("att","att1")]
                              [Text "text1"],
                      Element "c"
                              [("att","att2")]
                              [Text "text2"]],
             Element "b"
                     []
                     [Element "c"
                              [("att","att3")]
                              [Text "text3"],
                      Element "d"
                              []
                              [Text "text4"]]]

casablanca :: String
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML
casablancaParsed
  = Element "film"
            [("title","Casablanca")]
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]

-- XML-документ з Мал.1
films :: String
films
  = "<filmlist>\n\
    \  <film title = \"Rear Window\">\n\
    \    <director>Alfred Hitchcock</director>\n\
    \    <composer>Franz Waxman</composer>\n\
    \    <year>1954</year>\n\
    \  </film>\n\
    \  <film   title =  \"2001: A Space Odyssey\">\n\
    \    <director>Stanley Kubrick</director>\n\
    \    <composer>Richard Strauss</composer>\n\
    \    <composer>Gyorgy Ligeti</composer>\n\
    \    <composer>Johann Strauss</composer>\n\
    \    <year>1968</year>\n\
    \  </film>\n\
    \  <film title=\"Lawrence of Arabia\"  >\n\
    \    <duration>228</duration>\n\
    \    <director>David Lean</director>\n\
    \    <composer>Maurice Jarre</composer>\n\
    \  </film>\n\
    \</filmlist>\n\n\n"

-- Результат аналізу  попереднього докуменнту ('parse films')
filmsParsed :: XML
filmsParsed
  = Element "filmlist"
            []
            [Text "\n  ",
             Element "film" [("title","Rear Window")]
                            [Text "\n    ",
                             Element "director" [] [Text "Alfred Hitchcock"],
                             Text "\n    ",
                             Element "composer" [] [Text "Franz Waxman"],
                             Text "\n    ",
                             Element "year" [] [Text "1954"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","2001: A Space Odyssey")]
                            [Text "\n    ",
                             Element "director" [] [Text "Stanley Kubrick"],
                             Text "\n    ",
                             Element "composer" [] [Text "Richard Strauss"],
                             Text "\n    ",
                             Element "composer" [] [Text "Gyorgy Ligeti"],
                             Text "\n    ",
                             Element "composer" [] [Text "Johann Strauss"],
                             Text "\n    ",
                             Element "year" [] [Text "1968"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","Lawrence of Arabia")]
                            [Text "\n    ",
                             Element "duration" [] [Text "228"],
                             Text "\n    ",
                             Element "director" [] [Text "David Lean"],
                             Text "\n    ",
                             Element "composer" [] [Text "Maurice Jarre"],
                             Text "\n  "],
             Text "\n"]
