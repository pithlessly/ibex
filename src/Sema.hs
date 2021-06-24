module Sema (sema) where


import qualified Ast
import qualified Ir

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.List as List

import qualified Data.Map as Map

import qualified Data.Vector as Vector


sema :: Ast.Program -> Either String Ir.Program
sema = walkPgm

type Walk = Either String

walkPgm :: Ast.Program -> Walk Ir.Program
walkPgm Ast.Program { Ast._functions = fs } = do
  fs' <- walkFns fs
  return Ir.Program { Ir._functions = Vector.fromList fs' }

walkFns :: [Ast.Function] -> Walk [Ir.Function]
walkFns = go 0 Map.empty where
  go idx bindings = \case
    [] -> Right []
    f:fs ->
      let name = Ast._name f in
      if name `Map.member` bindings then
        Left $ "duplicate definitions of symbol '" ++ Text.unpack name ++ "'"
      else do
        let bindings' = Map.insert name idx bindings
        f' <- walkFn bindings' f
        fs' <- go (idx + 1) bindings' fs
        return $ f':fs'

walkFn :: Map.Map Text Int -> Ast.Function -> Walk Ir.Function
walkFn bindings f = do
  let walk = walkStatement bindings (Ast._args f)
  body' <- mapM walk $ Ast._body f
  return Ir.Function {
    Ir._name    = Ast._name f,
    Ir._numArgs = length $ Ast._args f,
    Ir._body    = Vector.fromList body'
  }

walkStatement :: Map.Map Text Int -> [Text] -> Ast.Statement -> Walk Ir.Statement
walkStatement bindings argNames = \case
  Ast.SCall fname args ->
    case Map.lookup fname bindings of
      Just idx -> do
        args' <- mapM (walkExpr argNames) args
        return $ Ir.SCall idx (Vector.fromList args')
      Nothing -> Left $ "undefined symbol '" ++ Text.unpack fname ++ "'"

walkExpr :: [Text] -> Ast.Expr -> Walk Ir.Expr
walkExpr argNames = \case
  Ast.EVar v ->
    case List.findIndex (== v) argNames of
      Just idx -> Right $ Ir.EVar idx
      Nothing  -> Left $ "undefined variable '" ++ Text.unpack v ++ "'"
