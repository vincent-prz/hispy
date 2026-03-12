module Eval (eval, standardEnv, RuntimeError(..)) where

import Control.Monad.State
import qualified Data.Map as Map
import Env (Env, standardEnv)
import Parser (Atom (..), Exp (..))
import Control.Monad.Except (ExceptT, MonadError (throwError))


newtype RuntimeError = RuntimeError String

eval :: Exp -> ExceptT RuntimeError (State Env) Exp
eval (Atom (ASymbol s)) = do
  env <- get
  case env Map.!? s of
    Just x -> return x
    Nothing -> throwError (RuntimeError ("unknown variable " ++ s))
eval num@(Atom (ANumber _)) = return num
eval (List ((Atom (ASymbol "if")) : rest)) = evalIf rest
eval (List ((Atom (ASymbol "define")) : rest)) = evalDefine rest
eval (List (func : args)) = do
  evaluatedFunc <- eval func
  evaluatedArgs <- mapM eval args
  case evaluatedFunc of
    Atom (AFunc f) -> return (f evaluatedArgs)
    _ -> throwError (RuntimeError "callee is not a function")
eval _ = error "unhandled case if eval"

evalIf :: [Exp] -> ExceptT RuntimeError (State Env) Exp
evalIf [test, conseq, alt] = do
  testResult <- eval test
  case testResult of
    Atom (ABool True) -> return conseq
    Atom (ABool False) -> return alt
    _ -> throwError (RuntimeError "condition evaluated on non boolean")
evalIf _ = throwError (RuntimeError "malformed conditional")

evalDefine :: [Exp] -> ExceptT RuntimeError (State Env) Exp
evalDefine [Atom (ASymbol symbol), expr] = do
  evaluatedExpr <- eval expr
  modify (Map.insert symbol evaluatedExpr)
  return evaluatedExpr
evalDefine _ = throwError (RuntimeError "malformed definition")