module Codegen (codegen) where


import Ir (Program (..), Function (..), Statement (..), Expr (..))

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Control.Monad (when)
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
localCtx view f = RWS.rws $ RWS.runRWS f . view

-- run with a custom state type
localSt :: st' -> Codegen ctx st' a -> Codegen ctx st a
localSt initial f =
  RWS.rws $ \ctx st ->
    let (ctx', _, o) = RWS.runRWS f ctx initial in (ctx', st, o)

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
  let numArgs = _numArgs f
  -- all functions are public by default; B has no equivalent of C's 'static' keyword.
  -- maybe we should add it?
  emits ["global ", name, "\n"]
  emits [name, ":\n"]
  localCtx ((,) numArgs) $
    mapM_ emitStatement $ _body f
  when (numArgs > 0) $
    emits ["  add rsp, ", Text.pack $ show $ 8 * numArgs, "\n"]
  emit "  ret\n\n"

type StackHeightDelta = Int

emitStatement :: Statement -> Codegen (Int, FnsCtx) st ()
emitStatement = \case
  SCall idx args -> do
    -- push the arguments onto the stack by copying them from deeper in the stack
    localCtx fst $
      localSt (0 :: StackHeightDelta) $
        mapM_ emitPushExpr args

    -- call the function
    fns <- RWS.asks snd
    let f = fns Vector.! idx
    emits ["  call ", _name f, "\n"]

emitPushExpr :: Expr -> Codegen Int StackHeightDelta ()
emitPushExpr = \case
  EVar idx -> do
    numArgs <- RWS.ask
    stackDelta <- RWS.get
    RWS.modify (+ 8)
    let delta = (numArgs - 1 - idx) * 8 + stackDelta
    if delta == 0 then
      emit "  push [rsp]\n"
    else
      emits ["  push [rsp + ", Text.pack $ show delta, "]\n"]

-- If the name starts with a dot or an underscore, prepend an underscore to it.
convertName :: Text -> Text
convertName name
  | Text.head name `elem` ['.', '_'] = Text.cons '_' name
  | otherwise = name
