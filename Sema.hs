{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module Sema (sema) where


import qualified Ast
import qualified Ir

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.Map as Map


sema :: Ast.Program -> Either String Ir.Program
sema = walkPgm

type Walk = Either String

walkPgm :: Ast.Program -> Walk Ir.Program
walkPgm Ast.Program { Ast._functions = fs } = do
  fs' <- walkFns fs
  return Ir.Program { Ir._functions = fs' }

walkFns :: [Ast.Function] -> Walk [Ir.Function]
walkFns = go 0 Map.empty where
  go idx bindings = \case
    [] -> Right []
    f:fs -> do
      let name = Ast._name f
      if name `Map.member` bindings then
        Left $ "duplicate definitions of symbol '" ++ Text.unpack name ++ "'"
      else do
      let bindings' = Map.insert name idx bindings
      f' <- walkFn bindings' f
      fs' <- go (idx + 1) bindings' fs
      return $ f':fs'

walkFn :: Map.Map Text Int -> Ast.Function -> Walk Ir.Function
walkFn bindings f = do
  body' <- mapM (walkStatement bindings) $ Ast._body f
  return Ir.Function {
    Ir._name    = Ast._name f,
    Ir._numArgs = length $ Ast._args f,
    Ir._body    = body'
  }

walkStatement :: Map.Map Text Int -> Ast.Statement -> Walk Ir.Statement
walkStatement bindings = \case
  Ast.SCall fname ->
    case Map.lookup fname bindings of
      Just idx -> Right $ Ir.SCall idx
      Nothing -> Left $ "undefined symbol '" ++ Text.unpack fname ++ "'"
