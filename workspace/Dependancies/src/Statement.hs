module Statement where

import StatementsToTokens

data Visibility = Public | Protected | Private
                  deriving (Show, Eq)

data Context = Namespace { name :: Token }
             | Class { name :: Token, visibility :: Visibility }
	     | Struct { name :: Token, visibility :: Visibility }
	     | Enum { name :: Token } deriving (Show, Eq)
             | NotSet

data Statement = Statement { tokens :: [Token], context :: [Context] }
                 deriving (Show, Eq)


--statement :: 
tokensToStatement :: [Token] -> Statement
tokensToStatement 
