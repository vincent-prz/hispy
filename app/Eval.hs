module Eval (eval, evalSE) where

import Control.Monad.State
import qualified Data.Map as Map
import Env (Env, standardEnv)
import Parser (Atom (..), Exp (..))

-- eval with standard Env

evalSE :: Exp -> Exp
evalSE expr = evalState (eval expr) standardEnv

eval :: Exp -> State Env Exp
eval (Atom (ASymbol s)) = do
  env <- get
  case env Map.!? s of
    Just x -> return x
    Nothing -> error ("unknown variable " ++ s)
eval num@(Atom (ANumber _)) = return num
eval (List ((Atom (ASymbol "if")) : rest)) = evalIf rest
eval (List ((Atom (ASymbol "define")) : rest)) = evalDefine rest
eval (List (func : args)) = do
  evaluatedFunc <- eval func
  evaluatedArgs <- mapM eval args
  case evaluatedFunc of
    Atom (AFunc f) -> return (f evaluatedArgs)
    _ -> error "callee is not a function"
eval _ = error "unhandled case if eval"

evalIf :: [Exp] -> State Env Exp
evalIf [test, conseq, alt] = do
  testResult <- eval test
  case testResult of
    Atom (ABool True) -> return conseq
    Atom (ABool False) -> return alt
    _ -> error "condition evaluated on non boolean"
evalIf _ = error "malformed conditional"

evalDefine :: [Exp] -> State Env Exp
evalDefine [Atom (ASymbol symbol), expr] = do
  evaluatedExpr <- eval expr
  modify (Map.insert symbol evaluatedExpr)
  return evaluatedExpr
evalDefine _ = error "malformed definition"