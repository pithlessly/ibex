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

emitProgram :: Program -> Codegen ()
emitProgram pgm =
  mapM_ emitFunction $ _functions pgm

emitFunction :: Function -> Codegen ()
emitFunction f = do
  emit (_name f)
  emit ":\n  ret\n"
