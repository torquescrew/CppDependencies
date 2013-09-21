module Main where

import RemoveComments
import Data.List.Utils

myFile :: [Char]
myFile = "/Users/tobysuggate/Desktop/LCWM4/LabChartEssentials/LabChart/ChartDraw/ChartDrawer.h"

--myFile = "/Users/tobysuggate/Desktop/LCWM4/Libs/CEF/include/internal/cef_types_wrappers.h"

main :: IO ()
main = do
       inpStr <- readFile myFile
       putStr (removeComments inpStr)


--a = "\"wib\");"
typeDecKeywords = ["class", "struct", "typedef", "enum", "enum class"]
statementBreaks = ";{}"
operators = [':',';','!','<','>','{','}','*','(',')','&',',','-','+','=',' ','.','?']
doubleOps = ["--","++","->","==","<=",">=","::","&&","||","**","/=","!=","+=","-=","*=","()"]


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
hasBreak code@(c:_) = hasAny [c] statementBreaks || endOfInclude code


endOfInclude :: String -> Bool
endOfInclude statement = startswith "#include " statement && (length statement) > 11


toStatements :: String -> String -> [String]
toStatements statement code 
             | hasBreak code      = newStatement:(toStatements [] (tail code))
                                    where newStatement = reverse $ (head code):statement
toStatements statement ('#':code) = statement:(toStatements "#" code)
toStatements statement (c:code)   = toStatements (c:statement) code
toStatements statement []         = [[]]
