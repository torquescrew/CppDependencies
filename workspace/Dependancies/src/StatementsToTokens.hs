module StatementsToTokens where

import Data.Char
import Data.List.Utils
import CodeToLines
import RemoveComments

operators :: [Char]
operators = ":;!<>{}*()&,-+= .?"

doubleOps :: [String]
doubleOps = ["--","++","->","==","<=",">=","::","&&","||","**","/=","!=","+=","-=","*=","()"]

type Token = String

takeString :: String -> (String, String)
takeString code = extrStr' "" code

extrStr' :: String -> String -> (String, String)
extrStr' ""  ('"':code) = extrStr' "\"" code
extrStr' str ('"':code) = (reverse('"':str), code)
extrStr' str (c  :code) = extrStr' (c:str) code
extrStr' str  code      = (str, code)


doubleOp :: String -> Bool
doubleOp code = hasAny [take 2 code] doubleOps

singleOp :: String -> Bool
singleOp (c:_) = c `elem` operators
singleOp []    = False


toTokens :: String -> [Token]
toTokens code = filter (not . null) (tt "" code)

tt :: String -> String -> [Token]
tt token code 
    | null code           = rToken:[]
    | isQuote code        = rToken:(fst string_code):tt "" (snd string_code)
    | isSpace (head code) = rToken:tt "" (tail code)
    | doubleOp code       = rToken:(take 2 code):tt "" (drop 2 code)
    | singleOp code       = rToken:[head code]:tt "" (tail code)
    | otherwise           = tt ((head code):token) (tail code)
      where string_code = takeString code
            rToken      = reverse token


isQuote :: String -> Bool
isQuote ('"':_) = True
isQuote  _      = False


tokenStatements ::  String -> [[Token]]
tokenStatements code = map toTokens ((toLines . removeComments) code)
