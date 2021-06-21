{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse (parse) where


import Ast (Program (..), Function (..))

import Data.Text (Text)
import qualified Data.Text as Text

import Text.Parsec (char, many, spaces, try, (<?>), (<|>))
import qualified Text.Parsec as Parsec


-- attempt to parse a string as a program, returning an error message on failure
parse :: Text -> Either String Program
parse t =
  case
    let initState = () in
    let filename = "" in
    Parsec.runParser (spaces *> program <* Parsec.eof) initState filename t
  of
    Left e -> Left $ show e
    Right pgm -> Right pgm

type Parser s = Parsec.Parsec Text s

-- parse a program
program :: Parser () Program
program = do
  functions <- many function
  return Program { _functions = functions }

-- parse a function
function :: Parser () Function
function = do
  name <- ident
  token "("
  args <- ident `Parsec.sepEndBy` token "," -- allow a trailing comma in parameter lists
  token ")"
  token ";"
  return Function { _name = name, _args = args }

-- expect the string `s` followed by spaces
token :: String -> Parser a ()
token s = Parsec.string s >> spaces

-- parse an identifier
ident :: Parser a Text
ident =
  try (do
    c  <-       Parsec.letter   <|> char '_' <|> char '.'
    cs <- many (Parsec.alphaNum <|> char '_' <|> char '.')
    let i = Text.pack $ c:cs
    if i `elem` ["auto", "extrn", "if", "else", "for", "while", "repeat", "switch", "do"]
      then Parsec.unexpected $ "keyword " ++ c:cs
      else return i
  ) <?> "identifier"
