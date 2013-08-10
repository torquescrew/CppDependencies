{-# LANGUAGE DoAndIfThenElse #-}

--import qualified Data.ByteString as B
--import Data.Text (Text)
--import Data.Text.Encoding (encodeUtf8)
--import qualified Data.Text as T
--import qualified Data.Text.IO as I
--import qualified Data.ByteString as B

import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import Data.String.Utils
import qualified Data.ByteString.Char8 as L
--import qualified Data.ByteString.Lazy.Char8 as L

process :: FilePath -> IO ()
process filename | isCpp filename  =  do 
					contents <- L.readFile filename
					--L.putStr (L.unlines (L.lines contents))
					--return()
					--putStr_Utf16LE contents
					putStr (getIncludes (lines (L.unpack contents)))
                 | otherwise = return()

mapDir :: (FilePath -> IO ()) -> FilePath -> IO ()
mapDir proc fp = do
  isFile <- doesFileExist fp -- is a file of fp
  if isFile then proc fp     -- process the file
  else getDirectoryContents fp >>=
       mapM_ (mapDir proc . (fp </>)) . filter (`notElem` [".", ".."])


--getIncludes' :: [Text] -> Text
--getIncludes' l = unlines (filter )


getIncludes :: [String] -> String
getIncludes l = unlines (filter isIncludeStatement l)


isCpp :: [Char] -> Bool
isCpp fileName = endswith ".h" fileName || endswith ".cpp" fileName


isIncludeStatement :: String -> Bool
isIncludeStatement = startswith "#include" . lstrip


--isIncludeStatement' :: Text -> Bool
--isIncludeStatement' = startswith "#include" . lstrip

--myPutStr :: String -> IO ()
--myPutStr t = putStr_Utf16LE (T.pack t)

--putStr_Utf16LE :: T.Text -> IO ()
--putStr_Utf16LE t = B.putStr (encodeUtf8 t)	

main :: IO ()
main = mapDir process "/Users/tobysuggate/Desktop/LCWM4/"
--main = mapDir process "/Users/tobysuggate/Desktop/LCWM4/CrossPlatform/Portable/CrossPlatform/ADIDat/DatCore/"
--main = mapDir process "/Users/tobysuggate/Desktop/untitledfolder/"
--main = mapDir process "/Users/tobysuggate/Desktop/LCWM4/LabChart/PCDevelop/LabChart/Calculations/"