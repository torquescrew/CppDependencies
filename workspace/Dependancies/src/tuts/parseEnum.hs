module ParseEnum where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (haskellDef)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char
import System.Environment


parseFile fname = parseFromFile (manyTill (try tag) (try readEnd)) fname

tag = do manyTill anyChar . try $ lookAhead tagStart
         char '<'
         name <- many $ noneOf " "
         props <- tagContents
         char '>'
         junk
         return (name, props)

tagContents = do props <- manyTill property . try . lookAhead $ char '>'
                 junk
                 return props

property = do spaces
              name <- many1 $ noneOf "="
              string "=\""
              val <- manyTill anyChar $ char '"'
              junk
              return (name, val)

junk = optional . many $ oneOf "\n\r\t\\/"

readEnd = do optional $ string "</svg>" 
             junk
             eof
tagStart = do char '<'
              tagName

tagName = string "rect" 
          <|> string "polygon" 
          <|> string "polyline" 
          <|> string "circle" 
          <|> string "path" 
          <|> string "g" 
          <|> string "svg"



valEnd = try (char '}')
         <|> (char ',')

-- Word = many (noneOf " ")

data Enum = Enum { eName::String, eValues::[String] } deriving Show


nextWord = manyTill anyChar (char ' ')

parseE = do spaces
            string "enum"
            spaces
            name <- nextWord
            spaces
            char '{'
            val <- sepBy (many (noneOf ",}")) (char ',')
            char '}'
            spaces
            char ';'
            junk
            return $ Enum name val


enum = endBy eList eoe
eList = sepBy vals (char ',')
vals = many (noneOf ",}")

eoe = char '}'


-- eol =   try (string "\n\r")
--     <|> try (string "\r\n")
--     <|> string "\n"
--     <|> string "\r"

parseEnum :: String -> Either ParseError [[String]]
parseEnum input = parse enum "(unknown)" input


{-

want to parse:

{ kSomething, kSomething2 }

and also:

{ kShowScrollEnableButton = 0x80000000, kHideChartViewButton = 0x40000000, }


endBy (sepBy (many (noneOf ",\n")) (char ',') eol

-}
