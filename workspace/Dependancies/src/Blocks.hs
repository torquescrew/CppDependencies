module Main where

import RemoveComments
import Data.List.Utils
import CodeToStatements
import StatementsToTokens


source ::  [Char]
source = "bool operator()(const IChartRecordDrawer *p1,const IChartRecordDrawer *p2)\n   {\n   long drawLayer1;\n   p1->GetLayer(&drawLayer1);\n   long drawLayer2;\n   p2->GetLayer(&drawLayer2);\n   return drawLayer1<drawLayer2;\n   }"

myFile :: [Char]
myFile = "/Users/tobysuggate/Desktop/LCWM4/LabChartEssentials/LabChart/ChartDraw/ChartDrawer.h"

--ae = "test.cpp"
ae ::  [Char]
ae = "/Users/tobysuggate/Documents/build_tool/AllEntities.cpp"
--myFile = "/Users/tobysuggate/Desktop/LCWM4/Libs/CEF/include/internal/cef_types_wrappers.h"

main :: IO ()
main = do
       inpStr <- readFile ae
       putStr (parseFile inpStr)


parseFile :: String -> String
parseFile code = join "\n" (map toT (toS code))
--parseFile code = join "\n"  (codeToTokens code)

--parse code = tokenStatements code

toS ::  String -> [String]
toS = (toStatements . removeComments)

-- to token string
toT ::  String -> String
toT = (join " ") . (toTokens)


{-
want tokens in list of lists

TODO:
 - relocate codeToTokens

for each statement of tokens, check for type dec, if true, add to list of types
so we need a list of found types? or just return that list?

foldr (:) []


-}

--checkFindTypes = findTypes [["class","cheese"],["struct","stru"]]




findTypes :: [[String]] -> [String]
findTypes (s:ss) | isTypeDec s = (getTypeName s):findTypes ss
                 | otherwise   = findTypes ss
findTypes _ = []

ft ss = filter isTypeDec (codeToTokens ss)

codeToTokens :: String -> [[String]]
codeToTokens code = map toTokens ((toStatements . removeComments) code)


--displayTokens code = join "\n" (map (join " ") (codeToTokens code))


typeDecKeywords ::  [[Char]]
typeDecKeywords = ["class", "struct", "typedef", "enum", "enum class"]


isTypeDec :: [String] -> Bool
isTypeDec statement = (head statement) `elem` typeDecKeywords


getTypeName :: [String] -> String
getTypeName ("class":name:_)        = name
getTypeName ("struct":name:_)       = name
getTypeName ("typedef":statement)   = head(tail(reverse statement))
getTypeName ("enum":"class":name:_) = name
getTypeName ("enum":"{":_)          = "constants in enum"
getTypeName ("enum":name:_)         = name
getTypeName _ = error "not a type"

