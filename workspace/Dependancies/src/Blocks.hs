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
       inpStr <- readFile myFile
       putStr (parseFile inpStr)


parseFile :: String -> String
--parseFile code = join "\n" (map toT (toS code))
parseFile code = join "\n"  (ft code)

--parse code = tokenStatements code

toS ::  String -> [String]
toS = (toStatements . removeComments)

-- to token string
toT ::  String -> String
toT = (join " ") . (toTokens)


testStatements = [["class","c1"],["struct","s1"]]
test1 = "class c1 {} struct s1 {};"


ft :: String -> [String]
ft ss = map getTypeName (filter isTypeDec (codeToTokens ss))


typeDecStatements ss = filter isTypeDec (codeToTokens ss)


codeToTokens :: String -> [[String]]
codeToTokens code = map toTokens ((toStatements . removeComments) code)


--displayTokens code = join "\n" (map (join " ") (codeToTokens code))


typeDecKeywords ::  [String]
typeDecKeywords = ["class", "struct", "typedef", "enum", "enum class"]


isTypeDec :: [String] -> Bool
isTypeDec (s:ss) = s `elem` typeDecKeywords
isTypeDec _      = False


getTypeName :: [String] -> String
getTypeName ("class":name:_)        = name
getTypeName ("struct":name:_)       = name
getTypeName ("typedef":statement)   = head(tail(reverse statement))
getTypeName ("enum":"class":name:_) = name
getTypeName ("enum":"{":_)          = "constants in enum"
getTypeName ("enum":name:_)         = name
getTypeName _ = ""

