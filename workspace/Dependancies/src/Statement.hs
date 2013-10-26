module Statement where

import Data.String.Utils
import StatementsToTokens

data Visibility = Public | Protected | Private
                  deriving (Show, Eq)

data Context = Namespace { contextName :: Token }
             | Class { contextName :: Token, visibility :: Visibility }
	     | Struct { contextName :: Token, visibility :: Visibility }
	     | Enum { contextName :: Token } 
             | OpenParen { contextName :: Token }
               deriving (Show, Eq)


data Statement = Statement { tokens :: [Token], context :: [Context] }
                 deriving (Show, Eq)


showContext :: Context -> String
showContext (Namespace n) = "namespace_" ++ n ++ "_{"
showContext (Class n v)   = "class_" ++ n ++ "_(" ++ show v ++ ")_{"
showContext (Struct n v)  = "struct_" ++ n ++ "_(" ++ show v ++ ")_{"
showContext (Enum n)      = "enum_" ++ n ++ "_{"
showContext (OpenParen _) = "{"


showTokens :: [Token] -> String
showTokens ts = join " " ts

scs :: [Context] -> String
scs cs = join ", " (map showContext cs)


setVisibility :: [Context] -> Visibility -> [Context]
setVisibility ((Class n _):cs) v = (Class n v):cs
setVisibility  cs              v = error ("Called setVisibility of " ++ (show v) ++ " " ++ " with context:" ++ (show cs))


-- contextChange :: [Token]  -> Bool
-- contextChange ts@("class":_)         = last ts == "{"
-- contextChange ts@("namespace":_)     = last ts == "{"
-- contextChange ts@("struct":_)        = last ts == "{"
-- contextChange    ("public":":":_)    = True
-- contextChange    ("protected":":":_) = True
-- contextChange    ("private":":":_)   = True
-- contextChange ts | last ts == "{"    = True
--                  | last ts == "}"    = True
-- contextChange _                      = False


tokensToStatement :: [Token] -> [Context] -> Statement
tokensToStatement ts@("class":n:_) c | last ts == "{"     = Statement ts ((Class n Private):c)
tokensToStatement ts@("namespace":n:_) c | last ts == "{" = Statement ts ((Namespace n):c)
tokensToStatement ts@("struct":n:_) c | last ts == "{"    = Statement ts ((Struct n Public):c)
tokensToStatement ts@("public":":":_) c                   = Statement ts (setVisibility c Public)
tokensToStatement ts@("protected":":":_) c                = Statement ts (setVisibility c Protected)
tokensToStatement ts@("private":":":_) c                  = Statement ts (setVisibility c Private)
tokensToStatement ts c | last ts == "{"                   = Statement ts (OpenParen "":c)
                       | "}" `elem` ts                    = Statement ts (tail c)
tokensToStatement ts c                                    = Statement ts c


-- TODO: Try remove recursion.
toStatements' :: [[Token]] -> [Context] -> [Statement]
toStatements' (t:ts) c = newS:toStatements' ts (context newS)
                         where newS = tokensToStatement t c
toStatements' []     _ = []


toStatements :: [[Token]] -> [Statement]
-- toStatements ts = toStatements' (filter contextChange ts) []
toStatements ts = toStatements' ts []


show2 :: Statement -> String
show2 s = showTokens (tokens s) ++ " // " ++ scs (context s) ++ "\n"

scopeStr :: Statement -> String
-- scopeStr s = join "::" (map contextName (reverse $ context s))
scopeStr s = join "::" (map contextName (reverse (filter notParen (context s))))


notParen :: Context -> Bool
notParen (OpenParen {}) = False
notParen  _             = True

--goos = [ x | x@(Goo {}) <- foos]


{-




-}
