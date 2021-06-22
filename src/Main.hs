module Main (compile, main) where


import qualified Parse
import qualified Sema
import qualified Codegen

import System.IO (hPutStrLn, stderr)

import qualified Data.Text as Text


compile :: String -> IO ()
compile s =
  case Parse.parse (Text.pack s) of
    Left msg -> err "parse" msg
    Right ast ->
      case Sema.sema ast of
        Left msg -> err "semantic analysis" msg
        Right ir ->
          putStr $ Text.unpack $ Codegen.codegen ir
  where
    err src msg =
      let formatted =
            case lines msg of
              [] -> "\ESC[m"
              [_] -> ":\ESC[m " ++ msg
              ls -> ":\ESC[m" ++ concatMap ("\n  " ++) ls
      in hPutStrLn stderr $ concat ["\ESC[1;31m", src, " error", formatted]

main :: IO ()
main = return ()
