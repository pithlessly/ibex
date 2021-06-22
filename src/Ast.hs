module Ast (Program (..), Function (..), Statement (..)) where


import Data.Text (Text)


data Program = Program {
  _functions :: [Function]
} deriving (Show)

data Function = Function {
  _name :: Text,
  _args :: [Text],
  _body :: [Statement]
} deriving (Show)

data Statement = SCall Text deriving (Show)
