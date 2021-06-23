module Main (compile, main) where


import qualified Parse
import qualified Sema
import qualified Codegen

import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError, ioeGetErrorString)
import System.Environment (getArgs)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text


err :: String -> String -> IO a
err src msg = do
  let formatted =
        case lines msg of
          [] -> "\ESC[m"
          [_] -> ":\ESC[m " ++ msg
          ls -> ":\ESC[m" ++ concatMap ("\n  " ++) ls

  hPutStrLn stderr $ concat ["\ESC[1;31merror (", src, ")", formatted]
  exitWith $ ExitFailure 1

-- helper to test compilation in the REPL
compile :: String -> IO ()
compile s =
  case Parse.parse (Text.pack s) of
    Left msg -> err "parse" msg
    Right ast ->
      case Sema.sema ast of
        Left msg -> err "sema" msg
        Right ir ->
          putStr $ Text.unpack $ Codegen.codegen ir

-- main compilation driver
main :: IO ()
main =
  try readCode >>= \(_, code) ->
    case Parse.parse code of
      Left msg -> err "parse" msg
      Right ast ->
        case Sema.sema ast of
          Left msg -> err "sema" msg
          Right ir -> do
            let asm = Codegen.codegen ir
            try $ Text.writeFile "out.asm" asm
            hPutStrLn stderr "finished, all ok."
  where
    try :: IO a -> IO a
    try f = tryIOError f >>= either (err "IO" . ioeGetErrorString) return

    readCode :: IO (FilePath, Text)
    readCode = getArgs >>= \case
      [path] -> do
        code <- Text.readFile path
        return (path, code)
      _ -> err "driver" "invalid arguments"
