module Main where
import Lexmich
import Parmich
import Absmich
import Mymich
import ErrM
import System.Environment

main = do
  (path:_) <- getArgs
  prog <- readFile path
  let e = pProgram (myLexer prog) in
    case e of
      Ok p -> do
        transProgram p
      Bad err -> do
        putStrLn err