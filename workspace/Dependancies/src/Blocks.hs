module Main where

import RemoveComments
import Data.List.Utils
import CodeToLines
import StatementsToTokens
import Strings
import Statement


main :: IO ()
main = do
       inpStr <- readFile chartDrawer
       putStr (parseFile inpStr)


parseFile :: String -> String
-- parseFile code = join "\n" (map toT (toS code))
parseFile code = join "\n" (map show2 (codeToStatements code))


toS ::  String -> [String]
toS = (toLines . removeComments)

-- to token string
toT ::  String -> String
toT = (join " ") . (toTokens)


ft :: String -> [String]
ft ss = map getTypeName (filter isTypeDec (codeToTokens ss))


typeDecStatements :: String -> [[String]]
typeDecStatements ss = filter isTypeDec (codeToTokens ss)


codeToTokens :: String -> [[String]]
codeToTokens code = map toTokens ((toLines . removeComments) code)


codeToStatements :: String -> [Statement]
codeToStatements code = toStatements (codeToTokens code)


typeDecKeywords :: [Token]
typeDecKeywords = ["class", "struct", "typedef", "enum", "enum class"]


isTypeDec :: [Token] -> Bool
isTypeDec ("class":ss) = length ss > 2 || last ss == "{"
isTypeDec (s:_)        = s `elem` typeDecKeywords -- && (length ss > 2 || last ss == "{")
isTypeDec  _           = False



getTypeName :: [Token] -> Token
getTypeName ("class":name:_)        = name
getTypeName ("struct":name:_)       = name
getTypeName ("typedef":statement)   = head(tail(reverse statement))
getTypeName ("enum":"class":name:_) = name
getTypeName ("enum":"{":_)          = "constants in enum"
getTypeName ("enum":name:_)         = name
getTypeName  _                      = ""

