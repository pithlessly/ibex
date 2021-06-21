{-# OPTIONS_GHC -Wall #-}

module Ast (Program (..), Function (..)) where


import Data.Text (Text)


data Program = Program {
  _functions :: [Function]
} deriving (Show)

data Function = Function {
  _name :: Text,
  _args :: [Text]
} deriving (Show)
