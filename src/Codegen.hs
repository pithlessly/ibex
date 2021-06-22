module Codegen (codegen) where


import Ir (Program (..), Function (..), Statement (..))

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import qualified Control.Monad.RWS.Strict as RWS


codegen :: Program -> Text
codegen pgm = render $ snd $ RWS.execRWS (emitProgram pgm) () []

data Emitter = ELeaf Text | EBranch Emitter Emitter

render :: Emitter -> Text
render = Text.concat . go [] where
  go acc (ELeaf t) = t : acc
  go acc (EBranch e1 e2) = go (go acc e2) e1

instance Semigroup Emitter where (<>) = EBranch
instance Monoid    Emitter where mempty = ELeaf ""

-- The code generation monad, relying on context data `ctx` and mutating state data `st`.
-- Generated fragments are accumulated in the writer as a difference list.
type Codegen ctx st = RWS.RWS ctx Emitter st

-- like `RWS.local`, but allows the function to modify the context type
localCtx :: (cty -> ctx) -> Codegen ctx st a -> Codegen cty st a
localCtx f x = RWS.rws $ RWS.runRWS x . f

emit :: Text -> Codegen ctx st ()
emit t = RWS.tell $ ELeaf t

emits :: [Text] -> Codegen ctx st ()
emits = mapM_ emit

emitProgram :: Program -> Codegen ctx st ()
emitProgram pgm =
  let fns = _functions pgm in
  RWS.forM_ fns (localCtx (const fns) . emitFunction)

type FnsCtx = Vector Function

emitFunction :: Function -> Codegen FnsCtx st ()
emitFunction f = do
  let name = convertName $ _name f
  -- all functions are public by default; B has no equivalent of C's 'static' keyword.
  -- maybe we should add it?
  emits ["global ", name, "\n"]
  emits [name, ":\n"]
  mapM_ emitStatement $ _body f
  emit "  ret\n\n"

emitStatement :: Statement -> Codegen FnsCtx st ()
emitStatement = \case
  SCall idx -> do
    fns <- RWS.ask
    let f = fns Vector.! idx
    emits ["  call ", _name f, "\n"]

-- If the name starts with a dot or an underscore, prepend an underscore to it.
convertName :: Text -> Text
convertName name
  | Text.head name `elem` ['.', '_'] = Text.cons '_' name
  | otherwise = name
