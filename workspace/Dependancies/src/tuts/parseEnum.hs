module ParseEnum where

-- import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec
-- import Text.Parsec

-- import Control.Applicative
-- import Control.Monad
import Control.Applicative hiding ((<|>), many, optional)

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

junk = optional . many $ oneOf "\n\r\t\\/"

data Enum = Enum { eName::String, eValues::[[String]] } deriving Show

tokenTail = alphaNum <|> char '_'
tokenHead = letter <|> char '_'

identifier = tokenHead <:> manyTill tokenTail (char ' ')


parseE = do spaces
            string "enum"
            spaces
            name <- identifier
            spaces
            char '{'
            val <- sepBy (many (noneOf ",}")) (char ',')
            char '}'
            spaces
            char ';'
            junk
            return $ Enum name (filter (not . null) (map eBlock val))

cToken = many1 (noneOf " ,{}")

enumBlock = do
            spaces
            n <- sepBy cToken spaces
            spaces
            junk
            return n


eBlock :: String -> [String]
eBlock s = getRight (myParse enumBlock s)

myParse p s = parse p "(unknown)" s

-- parseEnum :: String -> Either ParseError [[String]]
-- parseEnum = parse parseE "(unknown)" "enum DisplayMode { kDispModeChart, kDispModeScope, };"
parseEnum s = myParse parseE s

getRight :: Either t [[Char]] -> [[Char]]
getRight (Right b) = b
getRight (Left  _) = ["ruh roh"]

{-

want to parse:

{ kSomething, kSomething2 }

and also:

{ kShowScrollEnableButton = 0x80000000, kHideChartViewButton = 0x40000000, }


endBy (sepBy (many (noneOf ",\n")) (char ',') eol

-}
-- enum = endBy eList eoe
-- eList = sepBy vals (char ',')
-- vals = many (noneOf ",}")

-- eoe = char '}'


-- eol =   try (string "\n\r")
--     <|> try (string "\r\n")
--     <|> string "\n"
--     <|> string "\r"


-- parseFile fname = parseFromFile (manyTill (try tag) (try readEnd)) fname

-- tag = do manyTill anyChar . try $ lookAhead tagStart
--          char '<'
--          name <- many $ noneOf " "
--          props <- tagContents
--          char '>'
--          junk
--          return (name, props)

-- tagContents = do props <- manyTill property . try . lookAhead $ char '>'
--                  junk
--                  return props

-- property = do spaces
--               name <- many1 $ noneOf "="
--               string "=\""
--               val <- manyTill anyChar $ char '"'
--               junk
--               return (name, val)


-- readEnd = do optional $ string "</svg>" 
--              junk
--              eof
-- tagStart = do char '<'
--               tagName

-- tagName = string "rect" 
          -- <|> string "polygon" 
          -- <|> string "polyline" 
          -- <|> string "circle" 
          -- <|> string "path" 
          -- <|> string "g" 
          -- <|> string "svg"
