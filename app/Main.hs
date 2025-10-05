module Main where

import Eval (evalSE)
import Parser (parse)

-- program = "(begin (define r (+ 5 5)) (* pi (* r r)))"

program = "(<= (+ 1 (* 4 4)) 17)"

main :: IO ()
main = case parse program of
  Left err -> print err
  Right expr -> print (evalSE expr)
