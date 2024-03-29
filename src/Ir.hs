module Ir (Program (..), Function (..), Statement (..), Expr (..)) where


import Data.Text (Text)

import Data.Vector (Vector)


data Program = Program {
  _functions :: Vector Function
} deriving (Show)

data Function = Function {
  _name :: Text,
  _numArgs :: Int,
  _body :: Vector Statement
} deriving (Show)

data Statement = SCall Int (Vector Expr) deriving (Show)

data Expr = EVar Int deriving (Show)
