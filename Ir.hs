{-# OPTIONS_GHC -Wall #-}

module Ir (Program (..), Function (..), Statement (..)) where


import Data.Text (Text)


data Program = Program {
  _functions :: [Function]
} deriving (Show)

data Function = Function {
  _name :: Text,
  _numArgs :: Int,
  _body :: [Statement]
} deriving (Show)

data Statement = SCall Int deriving (Show)
