module Main where

import RemoveComments
import Data.List.Utils
import Data.Char

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


doubleOp :: String -> Bool
doubleOp code = hasAny [take 2 code] doubleOps

singleOp :: String -> Bool
singleOp (c:code) = c `elem` operators
singleOp []       = False

{-
--TODO: convert statement into tokens
toTokens :: String -> String -> [String]
toTokens token c@('"':code) = token:(fst res):toTokens "" (snd res)
                              where res = extrStr c
toTokens token (' ':code) = (reverse token):toTokens "" code
toTokens token code | doubleOp code = (reverse token):(take 2 code):toTokens "" (drop 2 code)
toTokens token code | singleOp code = (reverse token):[head code]:toTokens "" (tail code)
toTokens token [] = (reverse token):[]
toTokens token (c:code) = toTokens (c:token) code
-}


toTokens :: String -> [String]
toTokens code = filter (not . null) (tt "" code)

tt :: String -> String -> [String]
tt token code 
    | null code           = rToken:[]
    | isQuote code        = rToken:(fst string_code):tt "" (snd string_code)
    | isSpace (head code) = rToken:tt "" (tail code)
    | doubleOp code       = rToken:(take 2 code):tt "" (drop 2 code)
    | singleOp code       = rToken:[head code]:tt "" (tail code)
    | otherwise           = tt ((head code):token) (tail code)
      where string_code = extrStr code
            rToken      = reverse token


isQuote :: String -> Bool
isQuote ('"':_) = True
isQuote  _      = False


hasBreak :: String -> Bool
hasBreak code@(c:_) = hasAny [c] statementBreaks -- || endOfInclude code
hasBreak _          = False


endOfInclude :: String -> Bool
endOfInclude statement = startswith "#include " statement && (length statement) > 11


toStatements :: String -> [String]
toStatements = ts ""

isNewLine :: String -> Bool
isNewLine code = startswith "\n" code || startswith "\r" code || startswith "\r\n" code

splitAtNL :: String -> (String, String)
splitAtNL code = spltNL "" code


spltNL :: String -> String -> (String, String)
spltNL line code | isNewLine code = ((reverse line), code)
                 | null code      = ("", (reverse line))
                 | otherwise      = spltNL ((head code):line) (tail code)

{-
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
-}



ts :: String -> String -> [String]
ts line code 
    | hasBreak code       = ns:(ts [] (tail code))
    | startswith "#" code = (fst lines):(ts "" (snd lines))
    | null code           = []
    | otherwise           = ts ((head code):line) (tail code)
       where ns    = reverse $ (head code):line
             lines = splitAtNL code



