module Main where

import RemoveComments
import Data.List.Utils

myFile :: [Char]
myFile = "/Users/tobysuggate/Desktop/LCWM4/LabChartEssentials/LabChart/ChartDraw/ChartDrawer.h"

--ae = "test.cpp"
ae = "/Users/tobysuggate/Documents/build_tool/AllEntities.cpp"

--myFile = "/Users/tobysuggate/Desktop/LCWM4/Libs/CEF/include/internal/cef_types_wrappers.h"

main :: IO ()
main = do
       inpStr <- readFile ae
       putStr (parseFile inpStr)

parseFile :: String -> String
parseFile code = join "\n" (toStatements (removeComments code))  --parseFile = toStatements
--parseFile code = removeComments code  --parseFile = toStatements

typeDecKeywords = ["class", "struct", "typedef", "enum", "enum class"]
statementBreaks = ";{}"
operators = ":;!<>{}*()&,-+= .?"
doubleOps = ["--","++","->","==","<=",">=","::","&&","||","**","/=","!=","+=","-=","*=","()"]


source = "bool operator()(const IChartRecordDrawer *p1,const IChartRecordDrawer *p2)\n   {\n   long drawLayer1;\n   p1->GetLayer(&drawLayer1);\n   long drawLayer2;\n   p2->GetLayer(&drawLayer2);\n   return drawLayer1<drawLayer2;\n   }"

extrStr :: String -> (String, String)
extrStr code = extrStr' "" code

extrStr' :: String -> String -> (String, String)
extrStr' ""  ('"':code) = extrStr' "\"" code
extrStr' str ('"':code) = (reverse('"':str), code)
extrStr' str (c  :code) = extrStr' (c:str) code
extrStr' str  code      = (str, code)


beginsWithOp :: String -> Bool
beginsWithOp code = hasAny [take 2 code] doubleOps


--TODO
toTokens :: String -> String -> [String]
toTokens statement c@('"':code) = (fst res):toTokens statement (snd res)
                              	  where res = extrStr c
toTokens statement code | beginsWithOp code = [code]


hasBreak :: String -> Bool
hasBreak code@(c:_) = hasAny [c] statementBreaks -- || endOfInclude code
hasBreak _          = False


endOfInclude :: String -> Bool
endOfInclude statement = startswith "#include " statement && (length statement) > 11


toStatements :: String -> [String]
toStatements = toStatements' ""

isNewLine :: String -> Bool
isNewLine code = startswith "\n" code || startswith "\r" code || startswith "\r\n" code

splitAtNL :: String -> (String, String)
splitAtNL code = spltNL "" code


spltNL :: String -> String -> (String, String)
spltNL line code | isNewLine code = ((reverse line), code)
                 | null code      = ("", (reverse line))
                 | otherwise      = spltNL ((head code):line) (tail code)


toStatements' :: String -> String -> [String]
toStatements' statement code
              | hasBreak code        = ns:(toStatements' [] (tail code))
                                       where ns = reverse $ (head code):statement
toStatements' statement code@('#':_) = ns:(toStatements' "" rem)
                                       where both = splitAtNL code
                                             ns   = fst both
                                             rem  = snd both
toStatements' statement (c:code)     = toStatements' (c:statement) code
toStatements' statement []           = []






