module Blocks where

import RemoveComments

myFile :: [Char]
myFile = "/Users/tobysuggate/Desktop/LCWM4/LabChartEssentials/LabChart/ChartDraw/ChartDrawer.h"

main :: IO ()
main = do
       inpStr <- readFile myFile
       putStr (removeComments inpStr)

--parseStatement :: String -> [String]


