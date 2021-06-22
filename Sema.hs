{-# OPTIONS_GHC -Wall #-}

module Sema (sema) where


import qualified Ast
import qualified Ir

import qualified Data.Text as Text

import qualified Data.Set as Set


sema :: Ast.Program -> Either String Ir.Program
sema pgm = do
  let processFns _    [] = Right []
      processFns seen (f:fs)
        | name `Set.member` seen = Left $ "duplicate definitions of symbol '" ++ Text.unpack name ++ "'"
        | otherwise = (f' :) <$> processFns (name `Set.insert` seen) fs
        where name = Ast._name f
              f' = Ir.Function {
                Ir._name = name,
                Ir._numArgs = length $ Ast._args f
              }
  fns' <- processFns Set.empty (Ast._functions pgm)
  return Ir.Program {
    Ir._functions = fns'
  }
