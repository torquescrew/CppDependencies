module Main where

import RemoveComments

myFile :: [Char]
myFile = "/Users/tobysuggate/Desktop/LCWM4/LabChartEssentials/LabChart/ChartDraw/ChartDrawer.h"

--myFile = "/Users/tobysuggate/Desktop/LCWM4/Libs/CEF/include/internal/cef_types_wrappers.h"

main :: IO ()
main = do
       inpStr <- readFile myFile
       putStr (removeComments inpStr)


--a = "\"wib\");"
typeDecKeywords = ["class", "struct", "typedef", "enum", "enum class"]
operators = [':',';','!','<','>','{','}','*','(',')','&',',','-','+','=',' ','.','?']
doubleOps = ["--","++","->","==","<=",">=","::","&&","||","**","/=","!=","+=","-=","*=","()"]


extrStr :: String -> (String, String)
extrStr code = extrStr' "" code

extrStr' :: String -> String -> (String, String)
extrStr' ""  ('"':code) = extrStr' "\"" code
extrStr' str ('"':code) = (reverse('"':str), code)
extrStr' str (c  :code) = extrStr' (c:str) code
extrStr' str  code      = (str, code)


toStatement :: String -> String -> [String]
toStatement part c@('"':code) = (fst res):toStatement (snd res)
                                where res = extrStr c
toStatement code 