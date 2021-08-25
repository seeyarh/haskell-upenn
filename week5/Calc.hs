{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified ExprT
import Parser
import StackVM (Program)
import qualified StackVM

-- Ex 1
eval :: ExprT.ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add exp1 exp2) = eval exp1 + eval exp2
eval (ExprT.Mul exp1 exp2) = eval exp1 * eval exp2

-- Ex 2
evalStr :: String -> Maybe Integer
evalStr s =
  case parseExp ExprT.Lit ExprT.Add ExprT.Mul s of
    Just exp -> Just (eval exp)
    Nothing -> Nothing

-- Ex 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT.ExprT where
  lit e = ExprT.Lit e
  add e1 e2 = ExprT.Add e1 e2
  mul e1 e2 = ExprT.Mul e1 e2

-- Ex 4
instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x = x > 0
  add x y = x || y
  mul x y = x && y

newtype MinMax =
  MinMax Integer
  deriving newtype (Eq, Show, Num, Ord)

instance Expr MinMax where
  lit x = MinMax x
  add x y = max x y
  mul x y = min x y

newtype Mod7 =
  Mod7 Integer
  deriving newtype (Show, Num, Enum, Eq, Ord, Real, Integral)

instance Expr Mod7 where
  lit x = Mod7 x
  add x y = (x + y) `mod` 7
  mul x y = (x * y) `mod` 7

-- Ex 5
instance Expr Program where
  lit x = [StackVM.PushI x]
  add x y = (x ++ y) ++ [StackVM.Add]
  mul x y = (x ++ y) ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Ex 6
class HasVars a where
  var :: String -> a

data VarExprT
  = Lit Integer
  | Add VarExprT VarExprT
  | Mul VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance HasVars VarExprT where
  var s = Var s

instance Expr VarExprT where
  lit x = Lit x
  add x y = Add x y
  mul x y = Mul x y

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var s = M.lookup s

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = \_ -> Just x
  add x y =
    \m ->
      case (x m, y m) of
        (Just a, Just b) -> Just (a + b)
        _ -> Nothing
  mul x y =
    \m ->
      case (x m, y m) of
        (Just a, Just b) -> Just (a * b)
        _ -> Nothing

withVars ::
     [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
