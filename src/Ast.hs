module Ast (Program (..), Function (..), Statement (..), Expr (..)) where


import Data.Text (Text)


data Program = Program {
  _functions :: [Function]
} deriving (Show)

data Function = Function {
  _name :: Text,
  _args :: [Text],
  _body :: [Statement]
} deriving (Show)

data Statement = SCall Text [Expr] deriving (Show)

data Expr = EVar Text deriving (Show)
