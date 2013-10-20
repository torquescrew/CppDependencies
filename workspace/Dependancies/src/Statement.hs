module Statement where

import Data.String.Utils
import StatementsToTokens

data Visibility = Public | Protected | Private
                  deriving (Show, Eq)

data Context = Namespace { contextName :: Token }
             | Class { contextName :: Token, visibility :: Visibility }
	     | Struct { contextName :: Token, visibility :: Visibility }
	     | Enum { contextName :: Token } 
             | OpenParen
               deriving (Show, Eq)


data Statement = Statement { tokens :: [Token], context :: [Context] }
                 deriving (Show, Eq)


setVisibility :: [Context] -> Visibility -> [Context]
setVisibility ((Class n _):cs) v = (Class n v):cs
setVisibility  cs              v = error ("Called setVisibility of " ++ (show v) ++ " " ++ " with context:" ++ (show cs))


tokensToStatement :: [Token] -> [Context] -> Statement
tokensToStatement ts@("class":n:_) c | last ts == "{"     = Statement ts ((Class n Private):c)
tokensToStatement ts@("namespace":n:_) c | last ts == "{" = Statement ts ((Namespace n):c)
tokensToStatement ts@("struct":n:_) c | last ts == "{"    = Statement ts ((Struct n Public):c)
tokensToStatement ts@("public":":":_) c                   = Statement ts (setVisibility c Public)
tokensToStatement ts@("protected":":":_) c                = Statement ts (setVisibility c Protected)
tokensToStatement ts@("private":":":_) c                  = Statement ts (setVisibility c Private)
tokensToStatement ts c | last ts == "{"                   = Statement ts (OpenParen:c)
                       | head ts == "}"                   = Statement ts (tail c)
tokensToStatement ts c                                    = Statement ts c


-- TODO: Check it works, then try remove recursion.
toStatements' :: [[Token]] -> [Context] -> [Statement]
toStatements' (t:ts) c = newS:toStatements' ts (context newS)
                        where newS = tokensToStatement t c
toStatements' []     _ = []


toStatements :: [[Token]] -> [Statement]
toStatements ts = toStatements' ts []

sToString :: Statement -> String
sToString s = (show s) ++ "\n"



{-
How to go from [[Token]] -> [Statement]?


-}
