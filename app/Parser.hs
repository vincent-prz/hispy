module Parser where

import Data.Char (isDigit)

type Token = String

data Exp = Atom Atom | List [Exp]
  deriving (Show)

data Atom = Symbol String | ANumber ANumber
  deriving (Show)

data ANumber = AInt Int | AFloat Float
  deriving (Show)

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
  | otherwise = Symbol t
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
