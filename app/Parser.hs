{-# LANGUAGE InstanceSigs #-}

module Parser (Exp (..), Atom (..), ANumber (..), parse) where

import Data.Char (isDigit)
import Data.List (intercalate)

type Token = String

data Exp = Atom Atom | List [Exp]

instance Show Exp where
  show :: Exp -> String
  show (Atom a) = show a
  show (List e) = "(" ++ intercalate "," (map show e) ++ ")"

data Atom
  = ASymbol String
  | ANumber ANumber
  | ABool Bool
  | AFunc ([Exp] -> Exp)

instance Show Atom where
  show :: Atom -> String
  show (ASymbol s) = s
  show (ANumber n) = show n
  show (ABool b) = show b
  show (AFunc _) = "<func>"

data ANumber = AInt Int | AFloat Float
  deriving (Eq)

instance Show ANumber where
  show :: ANumber -> String
  show (AInt n) = show n
  show (AFloat f) = show f

instance Num ANumber where
  (*) :: ANumber -> ANumber -> ANumber
  (*) (AInt n) (AInt m) = AInt (n * m)
  (*) (AFloat n) (AFloat m) = AFloat (n * m)
  (*) (AInt n) (AFloat m) = AFloat (fromIntegral n * m)
  (*) (AFloat n) (AInt m) = AFloat (n * fromIntegral m)
  (+) :: ANumber -> ANumber -> ANumber
  (+) (AInt n) (AInt m) = AInt (n + m)
  (+) (AFloat n) (AFloat m) = AFloat (n + m)
  (+) (AInt n) (AFloat m) = AFloat (fromIntegral n + m)
  (+) (AFloat n) (AInt m) = AFloat (n + fromIntegral m)
  (-) :: ANumber -> ANumber -> ANumber
  (-) (AInt n) (AInt m) = AInt (n - m)
  (-) (AFloat n) (AFloat m) = AFloat (n - m)
  (-) (AInt n) (AFloat m) = AFloat (fromIntegral n - m)
  (-) (AFloat n) (AInt m) = AFloat (n - fromIntegral m)
  abs :: ANumber -> ANumber
  abs (AInt n) = AInt (abs n)
  abs (AFloat n) = AFloat (abs n)
  signum :: ANumber -> ANumber
  signum (AInt n) = AInt (signum n)
  signum (AFloat n) = AFloat (signum n)
  fromInteger :: Integer -> ANumber
  fromInteger integer = AInt (fromInteger integer)
  negate :: ANumber -> ANumber
  negate (AInt n) = AInt (negate n)
  negate (AFloat n) = AFloat (negate n)

instance Fractional ANumber where
  fromRational :: Rational -> ANumber
  fromRational r = AFloat (fromRational r)
  (/) :: ANumber -> ANumber -> ANumber
  (/) (AInt n) (AInt m) = AFloat (fromIntegral n / fromIntegral m)
  (/) (AFloat n) (AFloat m) = AFloat (n / m)
  (/) (AInt n) (AFloat m) = AFloat (fromIntegral n / m)
  (/) (AFloat n) (AInt m) = AFloat (n / fromIntegral m)

instance Ord ANumber where
  (<=) :: ANumber -> ANumber -> Bool
  (<=) (AInt n) (AInt m) = n <= m
  (<=) (AFloat n) (AFloat m) = n <= m
  (<=) (AInt n) (AFloat m) = fromIntegral n <= m
  (<=) (AFloat n) (AInt m) = n <= fromIntegral m

newtype SyntaxError = SyntaxError String
  deriving (Show)

-- tokenizer
tokenize :: String -> [Token]
tokenize = words . spaceParens
  where
    spaceParens [] = []
    spaceParens ('(' : xs) = " ( " ++ spaceParens xs
    spaceParens (')' : xs) = " ) " ++ spaceParens xs
    spaceParens (x : xs) = x : spaceParens xs

-- parser
parse :: String -> Either SyntaxError Exp
parse = readFromTokens . tokenize

readFromTokens :: [Token] -> Either SyntaxError Exp
readFromTokens toks = case parseExp toks of
  (Nothing, []) -> Left (SyntaxError "unexpected EOF")
  (Nothing, t : _) -> Left (SyntaxError ("unexpected token : " ++ t))
  (Just expr, _) -> Right expr

atom :: Token -> Atom
atom t
  | isIntAtom t = ANumber (AInt (read t))
  | isFloatAtom t = ANumber (AFloat (read t))
  | t == "True" = ABool True
  | t == "False" = ABool False
  | otherwise = ASymbol t
  where
    isIntAtom = all isDigit
    isFloatAtom tok =
      let splittedTok = split '.' tok
       in length splittedTok == 2 && all isIntAtom splittedTok

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = [[]]
split x (y : ys) =
  let recSplitted = split x ys
   in if x == y
        then [] : recSplitted
        else case recSplitted of
          [] -> error "Error splitting array"
          (frontArray : rest) -> (y : frontArray) : rest

-- parsing lib

type Parser a = [Token] -> (Maybe a, [Token])

parseExp :: Parser Exp
parseExp toks =
  let (listResult, listRest) = parseList toks
      (atomResult, atomRest) = parseAtom toks
   in case listResult of
        Just _ -> (listResult, listRest)
        Nothing -> (atomResult, atomRest)

parseAtom :: Parser Exp
parseAtom [] = (Nothing, [])
parseAtom ("(" : ts) = (Nothing, ts)
parseAtom (")" : ts) = (Nothing, ts)
parseAtom (t : ts) = (Just (Atom (atom t)), ts)

parseList :: Parser Exp
parseList [] = (Nothing, [])
parseList ("(" : ts) =
  let (listResult, listRest) = parseMany parseExp ts
   in case listRest of
        [] -> (Nothing, [])
        ")" : rest' -> (List <$> listResult, rest')
        _ -> (Nothing, listRest)
parseList toks = (Nothing, toks)

parseMany :: Parser Exp -> Parser [Exp]
parseMany p toks =
  let (singleParse, rest) = p toks
   in case singleParse of
        Nothing -> (Just [], toks)
        Just sp ->
          let (otherParses, rest') = parseMany p rest
           in ((sp :) <$> otherParses, rest')
