{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen (codegen) where


import Ir (Program (..), Function (..))

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Control.Monad.State.Strict as State


codegen :: Program -> Text
codegen pgm = Text.concat $ reverse $ State.execState (emitProgram pgm) []

type Codegen = State.State [Text]

emit :: Text -> Codegen ()
emit t = State.modify (t :)

emits :: [Text] -> Codegen ()
emits ts = State.modify (reverse ts ++)

emitProgram :: Program -> Codegen ()
emitProgram pgm =
  mapM_ emitFunction $ _functions pgm

emitFunction :: Function -> Codegen ()
emitFunction f = do
  let name = convertName $ _name f
  -- all functions are public by default; B has no equivalent of C's 'static' keyword.
  -- maybe we should add it?
  emits ["global ", name, "\n"]
  emits [name, ":\n"]
  emit "  ret\n\n"

-- If the name starts with a dot or an underscore, prepend an underscore to it.
convertName :: Text -> Text
convertName name
  | Text.head name `elem` ['.', '_'] = Text.cons '_' name
  | otherwise = name
