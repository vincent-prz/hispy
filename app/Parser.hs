module Parser where

import Control.Applicative
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
  Nothing -> Left (SyntaxError "Could not parse")
  (Just (expr, [])) -> Right expr
  (Just (_, t : _)) -> Left (SyntaxError ("Unexpected token : " ++ show t))

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

type Parser a = [Token] -> Maybe (a, [Token])

parseExp :: Parser Exp
parseExp toks = parseList toks <|> parseAtom toks

parseAtom :: Parser Exp
parseAtom [] = Nothing
parseAtom ("(" : _) = Nothing
parseAtom (")" : _) = Nothing
parseAtom (t : ts) = Just (Atom (atom t), ts)

parseList :: Parser Exp
parseList [] = Nothing
parseList ("(" : ts) = do
  (list, rest) <- parseMany parseExp ts
  case rest of
    [] -> Nothing
    ")" : rest' -> return (List list, rest')
    _ -> Nothing
parseList _ = Nothing

parseMany :: Parser Exp -> Parser [Exp]
parseMany p toks = case p toks of
  Nothing -> Just ([], toks)
  Just (singleParse, rest) -> do
    (otherParses, rest') <- parseMany p rest
    return (singleParse : otherParses, rest')
