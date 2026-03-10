module Main where

import Control.Monad.State
import Env (Env, standardEnv)
import Eval (eval)
import Parser (parse)
import System.IO (hFlush, stdout)

-- program = "(begin (define r (+ 5 5)) (* pi (* r r)))"

-- program = "(<= (+ 1 (* 4 4)) 17)"

processInput :: String -> State Env String
processInput input = case parse input of
  Left err -> return ("Parsing Error: " ++ show err)
  Right expr -> show <$> eval expr

repl :: Env -> IO ()
repl env = do
  program <- putStr "> " >> hFlush stdout >> getLine
  let (output, newEnv) = runState (processInput program) env
  putStrLn output
  repl newEnv

main :: IO ()
main = repl standardEnv
