{-# LANGUAGE DoAndIfThenElse #-}

import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import Data.String.Utils
import qualified Data.ByteString.Char8 as L

process :: FilePath -> IO ()
process filename | isCpp filename  =  do 
					contents <- L.readFile filename
					putStr (getIncludes (lines (L.unpack contents)))
                 | otherwise = return()


mapDir :: (FilePath -> IO ()) -> FilePath -> IO ()
mapDir proc fp = do
  isFile <- doesFileExist fp -- is a file of fp
  if isFile then proc fp     -- process the file
  else getDirectoryContents fp >>=
       mapM_ (mapDir proc . (fp </>)) . filter (`notElem` [".", ".."])



getIncludes :: [String] -> String
getIncludes l = unlines (filter isIncludeStatement l)


isCpp :: [Char] -> Bool
isCpp fileName = endswith ".h" fileName || endswith ".cpp" fileName


isIncludeStatement :: String -> Bool
isIncludeStatement = startswith "#include" . lstrip

getIncludeName :: String -> String
getIncludeName s = takeWhile (\x -> x /= '"') s


main :: IO ()
--main = mapDir process "/Users/tobysuggate/Desktop/LCWM4/"
main = mapDir process "/Users/tobysuggate/Desktop/LCWM4/CrossPlatform/Portable/CrossPlatform/ADIDat/DatCore/"
--main = mapDir process "/Users/tobysuggate/Desktop/untitledfolder/"
--main = mapDir process "/Users/tobysuggate/Desktop/LCWM4/LabChart/PCDevelop/LabChart/Calculations/"