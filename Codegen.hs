{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Codegen (codegen) where


import Ir (Program (..), Function (..), Statement (..))

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Control.Monad.RWS.Strict as RWS


codegen :: Program -> Text
codegen pgm = Text.concat $ reverse $ fst $ RWS.execRWS (emitProgram pgm) () []

-- The code generation monad, relying on context data `ctx`. There is no written data;
-- instead text fragments are accumulated in the state as a list in reverse order for
-- better performance.
type Codegen ctx = RWS.RWS ctx () [Text]

-- like `RWS.local`, but allows the function to modify the context type
localCtx :: (y -> x) -> Codegen x a -> Codegen y a
localCtx f x = RWS.rws $ RWS.runRWS x . f

emit :: Text -> Codegen ctx ()
emit t = RWS.modify (t :)

emits :: [Text] -> Codegen ctx ()
emits ts = RWS.modify (reverse ts ++)

emitProgram :: Program -> Codegen ctx ()
emitProgram pgm =
  let fns = _functions pgm in
  RWS.forM_ fns (localCtx (const fns) . emitFunction)

type FnsCtx = [Function]

emitFunction :: Function -> Codegen FnsCtx ()
emitFunction f = do
  let name = convertName $ _name f
  -- all functions are public by default; B has no equivalent of C's 'static' keyword.
  -- maybe we should add it?
  emits ["global ", name, "\n"]
  emits [name, ":\n"]
  mapM_ emitStatement $ _body f
  emit "  ret\n\n"

emitStatement :: Statement -> Codegen FnsCtx ()
emitStatement = \case
  SCall idx -> do
    fns <- RWS.ask
    let f = fns !! idx
    emits ["  call ", _name f, "\n"]

-- If the name starts with a dot or an underscore, prepend an underscore to it.
convertName :: Text -> Text
convertName name
  | Text.head name `elem` ['.', '_'] = Text.cons '_' name
  | otherwise = name
