module Env (Env, standardEnv) where

import qualified Data.Map as Map
import Parser (ANumber (..), Atom (..), Exp (..))

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
      ("=", Atom (AFunc equals)),
      ("begin", Atom (AFunc last)),
      ("pi", Atom (ANumber (AFloat pi)))
    ]

add :: [Exp] -> Exp
add = arithOp (+) "add"

substract :: [Exp] -> Exp
substract = arithOp (-) "substract"

multiply :: [Exp] -> Exp
multiply = arithOp (*) "multiply"

divide :: [Exp] -> Exp
divide = arithOp (/) "divide"

arithOp :: (ANumber -> ANumber -> ANumber) -> String -> [Exp] -> Exp
arithOp _ opName [] = error ("apply " ++ opName ++ " on empty list")
arithOp _ _ [Atom (ANumber n)] = Atom (ANumber n)
arithOp op opName (Atom (ANumber n) : rest) = case arithOp op opName rest of
  Atom (ANumber m) -> Atom (ANumber (op n m))
  _ -> error ("try to apply " ++ opName ++ " on something which is not a number")
arithOp _ opName _ = error ("try to apply " ++ opName ++ " on something which is not a number")

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

gt :: [Exp] -> Exp
gt = booleanArithOp (>) ">"

lt :: [Exp] -> Exp
lt = booleanArithOp (<) "<"

ge :: [Exp] -> Exp
ge = booleanArithOp (>=) ">="

le :: [Exp] -> Exp
le = booleanArithOp (<=) "<="

booleanArithOp :: (ANumber -> ANumber -> Bool) -> String -> [Exp] -> Exp
booleanArithOp _ opName [] = error ("apply " ++ opName ++ " on empty list")
booleanArithOp op _ [Atom (ANumber x), Atom (ANumber y)] = fromBool (op x y)
booleanArithOp _ opName exprs
  | length exprs /= 2 =
      error ("applied " ++ opName ++ " on " ++ show (length exprs) ++ "arg, expected 2")
  | otherwise = error ("try to apply " ++ opName ++ " on something which is not a number")
