module Parse (parse) where


import Ast (Program (..), Function (..), Statement (..), Expr (..))

import Data.Text (Text)
import qualified Data.Text as Text

import Text.Parsec (char, many, sepEndBy, spaces, try, (<?>), (<|>))
import qualified Text.Parsec as Parsec


-- attempt to parse a string as a program, returning an error message on failure
parse :: Text -> Either String Program
parse t =
  case
    let filename = "" in
    Parsec.parse (spaces *> program <* Parsec.eof) filename t
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
function =
  do
    name <- ident
    token "("
    args <- ident `sepEndBy` token "," -- allow a trailing comma in parameter lists
    token ")"
    -- TODO: allow single-statement functions to be defined without braces
    token "{"
    body <- many statement
    token "}"
    return Function { _name = name, _args = args, _body = body }
  <?> "function declaration"

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

-- parse a statement
statement :: Parser a Statement
statement =
  do
    name <- ident
    token "("
    args <- expr `sepEndBy` token "," -- allow a trailing comma in parameter lists
    token ")"
    token ";"
    return $ SCall name args
  <?> "statement"

-- parse an expression
expr :: Parser a Expr
expr = EVar <$> ident <?> "expression"
