module Main where

import Control.Monad.State
import Env (Env)
import Eval (evalSE)
import Parser (parse)
import System.IO (hFlush, stdout)

-- program = "(begin (define r (+ 5 5)) (* pi (* r r)))"

-- program = "(<= (+ 1 (* 4 4)) 17)"
-- repl :: State Env (IO ())
-- repl = do
--   -- putStr ">"
--   -- hFlush stdout
--   program <- getLine
--   case parse program of
--     Left err -> print err
--     Right expr -> print (evalSE expr) >> repl

main :: IO ()
main = do
  putStr ">"
  hFlush stdout
  program <- getLine
  case parse program of
    Left err -> print err
    Right expr -> print (evalSE expr) >> main
