module CodeToLines where

import Data.List.Utils
import Data.Char


lineBreaks :: [Char]
lineBreaks = ";{}"


visBreak :: [String]
visBreak = ["public","private","protected"]


toLines :: String -> [String]
toLines = ts ""


ts :: String -> String -> [String]
-- ts line (
ts line code 
    | hasBreak code       = ns:(ts [] (tail code))
    | startswith "#" code = (fst lines'):(ts "" (snd lines'))
    -- TODO isVisMod code
    | null code           = [] -- does this throw away line?
    | otherwise           = ts ((head code):line) (tail code)
       where ns     = reverse $ (head code):line
             lines' = splitAtNL code



hasBreak :: String -> Bool
hasBreak (c:_) = hasAny [c] lineBreaks
hasBreak  _    = False


isNewLine :: String -> Bool
isNewLine code = startswith "\n" code || startswith "\r" code || startswith "\r\n" code


splitAtNL :: String -> (String, String)
splitAtNL code = spltNL "" code


spltNL :: String -> String -> (String, String)
spltNL line code | isNewLine code = ((reverse line), code)
                 | null code      = ((reverse line), "")
                 | otherwise      = spltNL ((head code):line) (tail code)


isVisMod :: String -> Bool
isVisMod c | startswith "public" c = startswith ":" (dropWhile isSpace (drop 6 c))
isVisMod c | startswith "private" c = startswith ":" (dropWhile isSpace (drop 7 c))
isVisMod c | startswith "protected" c = startswith ":" (dropWhile isSpace (drop 9 c))
isVisMod _ = False
