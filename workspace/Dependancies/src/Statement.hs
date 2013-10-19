module Statement where

import StatementsToTokens

data Visibility = Public | Protected | Private
                  deriving (Show, Eq)

data Context = Namespace { name :: Token }
             | Class { name :: Token, visibility :: Visibility }
	     | Struct { name :: Token, visibility :: Visibility }
	     | Enum { name :: Token } 
             | OpenParen
             | CloseParen
               deriving (Show, Eq)


data Statement = Statement { tokens :: [Token], context :: [Context] }
                 deriving (Show, Eq)


setVisibility :: [Context] -> Visibility -> [Context]
setVisibility ((Class n _):cs) v = (Class n v):cs
setVisibility  _               _ = error "Called setVisibility without class context"


tokensToStatement :: [Token] -> [Context] -> Statement
tokensToStatement ts@("class":n:_) c | last ts == "{" = Statement ts ((Class n Private):c)
tokensToStatement ts@("namespace":n:_) c | last ts == "{" = Statement ts ((Namespace n):c)
tokensToStatement ts@("struct":n:_) c | last ts == "{" = Statement ts ((Struct n Public):c)
tokensToStatement ts@("public":":":_) c = Statement ts (setVisibility c Public)
tokensToStatement ts@("protected":":":_) c = Statement ts (setVisibility c Protected)
tokensToStatement ts@("private":":":_) c = Statement ts (setVisibility c Private)
tokensToStatement ts c | last ts == "{" = Statement ts (OpenParen:c)
                       | head ts == "}" = Statement ts (tail c)

