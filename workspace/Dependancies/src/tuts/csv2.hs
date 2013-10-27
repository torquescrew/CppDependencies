module Csv2 where

-- file: ch16/csv2.hs
import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")


eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

-- eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
