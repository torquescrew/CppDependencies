{-# LANGUAGE DoAndIfThenElse #-}

import qualified Data.ByteString.Char8 as L
import           Data.String.Utils
import           System.Directory      (doesFileExist, getDirectoryContents)
import           System.FilePath       ((</>))

process :: FilePath -> IO ()
process filename | isCpp filename  =  do
                   contents <- L.readFile filename
                   putStr (getIncludes (lines (L.unpack contents)))
                   --putStrLn (info (lines (L.unpack contents)))
                 | otherwise = return()


mapDir :: (FilePath -> IO ()) -> FilePath -> IO ()
mapDir proc fp = do
  isFile <- doesFileExist fp -- is a file of fp
  if isFile then proc fp     -- process the file
  else getDirectoryContents fp >>=
       mapM_ (mapDir proc . (fp </>)) . filter (`notElem` [".", ".."])



getIncludes :: [String] -> String
getIncludes l = unlines (map getIncludeName (filter isIncludeStatement l))


info :: [String] -> String
info l = "Number of includes: " ++ show (length l)


isCpp :: [Char] -> Bool
isCpp fileName = endswith ".h" fileName || endswith ".cpp" fileName


isIncludeStatement :: String -> Bool
isIncludeStatement = startswith "#include" . lstrip

getIncludeName :: String -> String
getIncludeName = removeFolders . head . tail . words . replaceQuotes


replaceQuotes :: String -> String
replaceQuotes ('"':cs) = ' ' : replaceQuotes cs
replaceQuotes ('<':cs) = ' ' : replaceQuotes cs
replaceQuotes ('>':cs) = ' ' : replaceQuotes cs
replaceQuotes (c:cs)   =  c  : replaceQuotes cs
replaceQuotes _        = []


removeFolders :: String -> String
removeFolders s = last (words (replace "/" " " s))



main :: IO ()
main = mapDir process "/Users/tobysuggate/Desktop/LCWM4/"
--main = mapDir process "/Users/tobysuggate/Desktop/LCWM4/CrossPlatform/Portable/CrossPlatform/ADIDat/DatCore/"
--main = mapDir process "/Users/tobysuggate/Desktop/untitledfolder/"
--main = mapDir process "/Users/tobysuggate/Desktop/LCWM4/LabChart/PCDevelop/LabChart/Calculations/"
