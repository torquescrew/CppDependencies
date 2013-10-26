module Main where

import RemoveComments
import Data.List.Utils
import CodeToLines
import StatementsToTokens
import Strings
import Statement

type Code = String

main :: IO ()
main = do
       inpStr <- readFile chartDrawer
       putStr (parseFile inpStr)


parseFile :: String -> String
-- parseFile code = join "\n" (map toT (toS code))
-- parseFile code = join "\n" (map show2 (codeToStatements code))
parseFile code = join "\n" (ft code)


toS ::  Code -> [String]
toS = (toLines . removeComments)

-- to token string
toT ::  String -> String
toT = (join " ") . (toTokens)



ft :: Code  -> [Token]
ft code = map getTypeName (filter isTypeDec (codeToStatements code))


-- typeDecStatements :: String -> [[String]]
-- typeDecStatements ss = filter isTypeDec' (codeToTokens ss)


codeToTokens :: Code -> [[Token]]
codeToTokens code = map toTokens ((toLines . removeComments) code)


codeToStatements :: Code -> [Statement]
codeToStatements code = toStatements (codeToTokens code)


typeDecKeywords :: [Token]
typeDecKeywords = ["class", "struct", "typedef", "enum", "enum class"]


-- isTypeDec' :: [Token] -> Bool
-- isTypeDec' ("class":ss) = length ss > 2 || last ss == "{"
-- isTypeDec' (s:_)        = s `elem` typeDecKeywords -- && (length ss > 2 || last ss == "{")
-- isTypeDec'  _           = False


isTypeDec :: Statement -> Bool
isTypeDec (Statement ("class":ss) _ ) = length ss > 2 || last ss == "{"
isTypeDec (Statement (s:_) _)         = s `elem` typeDecKeywords
isTypeDec  _                          = False



getTypeName :: Statement -> Token
getTypeName s = scopeStr s ++ "::" ++  getTypeName' (tokens s)


--Should this be in Statement?
getTypeName' :: [Token] -> Token
getTypeName' ("class":name:_)        = name
getTypeName' ("struct":name:_)       = name
getTypeName' ("typedef":statement)   = head(tail(reverse statement))
getTypeName' ("enum":"class":name:_) = name
getTypeName' ("enum":"{":_)          = "constants in enum"
getTypeName' ("enum":name:_)         = name
getTypeName'  _                      = ""

