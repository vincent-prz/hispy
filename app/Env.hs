module Env (Env, standardEnv) where

import qualified Data.Map as Map
import Exp (ANumber (..), Atom (..), Exp (..))
import RuntimeError(RuntimeError(..))

type Env = Map.Map String Exp

standardEnv :: Env
standardEnv =
  Map.fromList
    [ ("+", Atom (AFunc add)),
      ("-", Atom (AFunc substract)),
      ("*", Atom (AFunc multiply)),
      ("/", Atom (AFunc divide)),
      (">", Atom (AFunc gt)),
      ("<", Atom (AFunc lt)),
      (">=", Atom (AFunc ge)),
      ("<=", Atom (AFunc le)),
      ("=", Atom (AFunc (pure . equals))),
      ("begin", Atom (AFunc (pure. last))),
      ("pi", Atom (ANumber (AFloat pi)))
    ]

add :: [Exp] -> Either RuntimeError Exp
add = arithOp (+) "add"

substract :: [Exp] -> Either RuntimeError Exp
substract = arithOp (-) "substract"

multiply :: [Exp] -> Either RuntimeError Exp
multiply = arithOp (*) "multiply"

divide :: [Exp] -> Either RuntimeError Exp
divide = arithOp (/) "divide"

arithOp :: (ANumber -> ANumber -> ANumber) -> String -> [Exp] -> Either RuntimeError Exp
arithOp _ opName [] = error ("apply " ++ opName ++ " on empty list")
arithOp _ _ [Atom (ANumber n)] = Right (Atom (ANumber n))
arithOp op opName (Atom (ANumber n) : rest) = case arithOp op opName rest of
  Right (Atom (ANumber m)) -> Right (Atom (ANumber (op n m)))
  Right _ -> Left (RuntimeError ("try to apply " ++ opName ++ " on something which is not a number"))
  Left err -> Left err
arithOp _ opName _ = Left (RuntimeError ("try to apply " ++ opName ++ " on something which is not a number"))

equals :: [Exp] -> Exp
equals [] = error "= applied on empty list"
equals [_] = Atom (ABool True)
equals (x : xs) = fromBool (all (equalsBinary x) xs)
  where
    equalsBinary :: Exp -> Exp -> Bool
    equalsBinary (Atom (ABool a)) (Atom (ABool b)) = a == b
    equalsBinary (Atom (ANumber a)) (Atom (ANumber b)) = a == b
    equalsBinary (List []) (List (_ : _)) = False
    equalsBinary (List (_ : _)) (List []) = False
    equalsBinary (List (a : as)) (List (b : bs)) =
      equalsBinary a b && equalsBinary (List as) (List bs)
    equalsBinary _ _ = False

fromBool :: Bool -> Exp
fromBool True = Atom (ABool True)
fromBool False = Atom (ABool False)

gt :: [Exp] -> Either RuntimeError Exp
gt = booleanArithOp (>) ">"

lt :: [Exp] -> Either RuntimeError Exp
lt = booleanArithOp (<) "<"

ge :: [Exp] -> Either RuntimeError Exp
ge = booleanArithOp (>=) ">="

le :: [Exp] -> Either RuntimeError Exp
le = booleanArithOp (<=) "<="

booleanArithOp :: (ANumber -> ANumber -> Bool) -> String -> [Exp] -> Either RuntimeError Exp
booleanArithOp _ opName [] = error ("apply " ++ opName ++ " on empty list")
booleanArithOp op _ [Atom (ANumber x), Atom (ANumber y)] = Right (fromBool (op x y))
booleanArithOp _ opName exprs
  | length exprs /= 2 =
      Left (RuntimeError ("applied " ++ opName ++ " on " ++ show (length exprs) ++ "arg, expected 2"))
  | otherwise = error ("try to apply " ++ opName ++ " on something which is not a number")
