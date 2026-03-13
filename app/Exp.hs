{-# LANGUAGE InstanceSigs #-}
module Exp(ANumber(..), Atom(..), Exp(..)) where

import Data.List (intercalate)
import RuntimeError(RuntimeError)

data Exp = Atom Atom | List [Exp]

instance Show Exp where
  show :: Exp -> String
  show (Atom a) = show a
  show (List e) = "(" ++ intercalate "," (map show e) ++ ")"

data Atom
  = ASymbol String
  | ANumber ANumber
  | ABool Bool
  | AFunc ([Exp] -> Either RuntimeError Exp)

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
