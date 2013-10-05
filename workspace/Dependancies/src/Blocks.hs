module Main where

import RemoveComments
import Data.List.Utils
import CodeToStatements
import StatementsToTokens

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
parseFile code = join "\n" (map toT (toS code))


toS = (toStatements . removeComments)


toT = (join " ") . (toTokens)


source = "bool operator()(const IChartRecordDrawer *p1,const IChartRecordDrawer *p2)\n   {\n   long drawLayer1;\n   p1->GetLayer(&drawLayer1);\n   long drawLayer2;\n   p2->GetLayer(&drawLayer2);\n   return drawLayer1<drawLayer2;\n   }"


