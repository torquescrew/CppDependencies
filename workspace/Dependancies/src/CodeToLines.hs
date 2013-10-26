module CodeToLines where

import Data.List
import Data.List.Utils
import Data.Char
import Data.Maybe


lineBreaks :: [Char]
lineBreaks = ";{}"



toLines :: String -> [String]
toLines = ts2 ""


ts2 :: String -> String -> [String]
ts2 l  c | hasBreak c              = nl:(ts2 [] (tail c))
                                     where nl = reverse (head c:l)

ts2 _  c | settingVis              = (take i c):ts2 "" (drop i c)
                                     where vis        = isVisMod' c
                                           settingVis = isJust vis
                                           i          = fromJust vis + 1

ts2 _  c | startswith "#" c        = (fst lines'):(ts2 "" (snd lines'))
                                     where lines' = breakList isNewLine c

ts2 _  c | startswith "template" c 
         && '>' `elem` c           = (fst code):(ts2 "" (snd code))
                                     where code = breakAfter (=='>') c

ts2 [] []                          = []
ts2 l  []                          = reverse l:[]
ts2 l  c                           = ts2 ((head c):l) (tail c)




hasBreak :: String -> Bool
hasBreak (c:_) = hasAny [c] lineBreaks
hasBreak  _    = False


isNewLine :: String -> Bool
isNewLine code = startswith "\n" code || startswith "\r" code || startswith "\r\n" code



breakAfter :: (a -> Bool) -> [a] -> ([a], [a])
breakAfter p l = splitAt i l
                 where i = fromJust (findIndex p l) + 1


isVisMod' :: String -> Maybe Int
isVisMod' c | startswith "public" c && startswith ":" (dropWhile isSpace (drop 6 c)) = elemIndex ':' c
isVisMod' c | startswith "private" c && startswith ":" (dropWhile isSpace (drop 7 c)) = elemIndex ':' c
isVisMod' c | startswith "protected" c && startswith ":" (dropWhile isSpace (drop 9 c)) = elemIndex ':' c
isVisMod' _ = Nothing


