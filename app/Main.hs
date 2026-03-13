module Main where

import Control.Monad.State
import Env (Env, standardEnv)
import Eval (eval)
import RuntimeError(RuntimeError(..))
import Parser (parse)
import System.IO (hFlush, stdout)
import Control.Monad.Except (ExceptT, runExceptT)

-- program = "(begin (define r (+ 5 5)) (* pi (* r r)))"

-- program = "(<= (+ 1 (* 4 4)) 17)"

processInput :: String -> ExceptT RuntimeError (State Env) String
processInput input = case parse input of
  Left err -> return ("Parsing Error: " ++ show err)
  Right expr -> show <$> eval expr

repl :: Env -> IO ()
repl env = do
  program <- putStr "> " >> hFlush stdout >> getLine
  let (output, newEnv) = runState (runExceptT (processInput program)) env
  case output of
    Left (RuntimeError err) -> putStrLn ("Runtime error: " ++ err)
    Right newExp -> putStrLn newExp
  repl newEnv

main :: IO ()
main = repl standardEnv
