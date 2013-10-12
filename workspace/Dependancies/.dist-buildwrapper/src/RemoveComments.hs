module RemoveComments where

import Data.List.Utils (join)

removeComments :: String -> String
removeComments = rmMultComment . rmLineComments 

rmLineComments :: String -> String
rmLineComments code = join "\n" (map rmLineComment (lines code))


rmLineComment :: String -> String
rmLineComment ('/':'/':_) = ""
rmLineComment (c:line)    = c : rmLineComment line
rmLineComment (_)         = ""


rmMultComment' :: String -> String
rmMultComment' ('*':'/':code) = code
rmMultComment' ( _ : code)    = rmMultComment' code
rmMultComment' ( _ )          = []


rmMultComment :: String -> String
rmMultComment ('/':'*':code) = rmMultComment (rmMultComment' code)
rmMultComment ( c : code)    = c : rmMultComment code
rmMultComment ( _ )          = []

