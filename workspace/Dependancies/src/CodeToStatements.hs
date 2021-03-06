module CodeToLines where

import Data.List.Utils


lineBreaks :: [Char]
lineBreaks = ";{}"


toLines :: String -> [String]
toLines = ts ""


ts :: String -> String -> [String]
ts line code 
    | hasBreak code       = ns:(ts [] (tail code))
    | startswith "#" code = (fst lines'):(ts "" (snd lines'))
    | null code           = [] -- does this throw away line?
    | otherwise           = ts ((head code):line) (tail code)
       where ns     = reverse $ (head code):line
             lines' = splitAtNL code

--tss line (c:code)


hasBreak :: String -> Bool
hasBreak (c:_) = hasAny [c] lineBreaks
hasBreak  _    = False


endOfInclude :: String -> Bool
endOfInclude statement = startswith "#include " statement && (length statement) > 11


isNewLine :: String -> Bool
isNewLine code = startswith "\n" code || startswith "\r" code || startswith "\r\n" code


splitAtNL :: String -> (String, String)
splitAtNL code = spltNL "" code


spltNL :: String -> String -> (String, String)
spltNL line code | isNewLine code = ((reverse line), code)
                 | null code      = ((reverse line), "")
                 | otherwise      = spltNL ((head code):line) (tail code)



