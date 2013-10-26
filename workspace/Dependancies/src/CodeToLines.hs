module CodeToLines where

import Data.List
import Data.List.Utils
import Data.Char
import Data.Maybe


lineBreaks :: [Char]
lineBreaks = ";{}"


-- visBreak :: [String]
-- visBreak = ["public","private","protected"]


toLines :: String -> [String]
toLines = ts2 ""


-- ts :: String -> String -> [String]
-- -- ts line (
-- ts line code 
--     | hasBreak code       = ns:(ts [] (tail code))
--     | startswith "#" code = (fst lines'):(ts "" (snd lines'))
--     -- TODO isVisMod code
--     -- | isVisMod' code      
--     | null code           = [] -- does this throw away line?
--     | otherwise           = ts ((head code):line) (tail code)
--        where ns     = reverse $ (head code):line
--              lines' = splitAtNL code


--TODO: need to handle template < >, should result in new line
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


-- hasTemplate :: String -> Bool
-- hasTemplate 


isNewLine :: String -> Bool
isNewLine code = startswith "\n" code || startswith "\r" code || startswith "\r\n" code



breakAfter :: (a -> Bool) -> [a] -> ([a], [a])
breakAfter p l = splitAt i l
                 where i = fromJust (findIndex p l) + 1



-- splitAtNL :: String -> (String, String)
-- splitAtNL code = spltNL "" code


-- spltNL :: String -> String -> (String, String)
-- spltNL line code | isNewLine code = ((reverse line), code)
--                  | null code      = ((reverse line), "")
--                  | otherwise      = spltNL ((head code):line) (tail code)


-- breakM :: ([a] -> Bool) -> [a] -> [a] -> ([a], [a])
-- breakM p l c | p c = ((reverse l), c)
--              | null c = ((reverse l), [])
--              | otherwise = breakM p ((head c):l) (tail c)


-- isVisMod :: String -> Bool
-- isVisMod c | startswith "public" c = startswith ":" (dropWhile isSpace (drop 6 c))
-- isVisMod c | startswith "private" c = startswith ":" (dropWhile isSpace (drop 7 c))
-- isVisMod c | startswith "protected" c = startswith ":" (dropWhile isSpace (drop 9 c))
-- isVisMod _ = False

isVisMod' :: String -> Maybe Int
isVisMod' c | startswith "public" c && startswith ":" (dropWhile isSpace (drop 6 c)) = (elemIndex ':' c)
isVisMod' c | startswith "private" c && startswith ":" (dropWhile isSpace (drop 7 c)) = elemIndex ':' c
isVisMod' c | startswith "protected" c && startswith ":" (dropWhile isSpace (drop 9 c)) = elemIndex ':' c
isVisMod' _ = Nothing

-- splitVis :: String -> [String]
-- splitVis code 
