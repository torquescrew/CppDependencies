{-# LANGUAGE OverloadedStrings #-}

-- import Data.Attoparsec.Char
import Data.Attoparsec
import Control.Applicative
import Data.Word

data Product = Mouse | Keyboard | Monitor | Speakers deriving Show

productParser :: Parser Product
productParser =
     (string "mouse"    >> return Mouse)
 <|> (string "keyboard" >> return Keyboard)
 <|> (string "monitor"  >> return Monitor)
 <|> (string "speakers" >> return Speakers)


data E = E Word8 deriving Show


spaceSkip = many $ satisfy $ inClass [ ' ' , '\t' ]
      -- sepParser = spaceSkip >> char sepChar >> spaceSkip

enums = do
   spaceSkip
   string "enum"
   spaceSkip
   name <- anyWord8
   spaceSkip
   return (E name)
   


main :: IO ()
main = do
  print $ parseOnly enums " enum cheese {}"
