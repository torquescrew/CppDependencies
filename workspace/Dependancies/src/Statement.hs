module Statement where

import StatementsToTokens

data Visibility = Public | Protected | Private
                  deriving (Show, Eq)

data Context = Namespace { name :: Token }
             | Class { name :: Token, visibility :: Visibility }
	     | Struct { name :: Token, visibility :: Visibility }
	     | Enum { name :: Token } deriving (Show, Eq)


data Statement = Statement { tokens :: [Token], context :: [Context] }
                 deriving (Show, Eq)

-- TODO: should take "class" and then pick Visibility on its own.
setVisibility :: [Context] -> Visibility -> [Context]
setVisibility (c@(Class n _):cs) v = (Class n v):cs
setVisibility  _                 _ = error "Called setVisibility on non class"


tokensToStatement :: [Token] -> [Context] -> Statement
tokensToStatement ts@("class":n:_) c | last ts == "{" = Statement ts ((Class n Private):c)
tokensToStatement ts@("namespace":n:_) c | last ts == "{" = Statement ts ((Namespace n):c)
tokensToStatement ts@("struct":n:_) c | last ts == "{" = Statement ts ((Struct n Public):c)
--tokensToStatement ts@("public":":":_) c = Statement ts (
