module Compiler
  ( module Compiler
  ) where

import Data.Fixed (mod')
import qualified Data.Map as Map
import Syntax

type EvalErr = String

type ValType = Double

type Env = Map.Map String Double

defaultEnv :: Env
defaultEnv = Map.empty

getBinding :: String -> Env -> Maybe Double
getBinding = Map.lookup

setBinding :: String -> Double -> Env -> Env
setBinding = Map.insert

evalAll :: [Expr] -> Env -> Either EvalErr (ValType, Env)
evalAll [] _ = Left "There needs to be at least one expression"
evalAll [expr] env = eval expr env
evalAll (expr:remaining) env =
  case eval expr env of
    Right (_, newEnv) -> evalAll remaining newEnv
    _ -> Left "error eval all"

-- TODO: Clean this up by using StateMonad
-- TODO: Propogate errors, maybe helper function to show either or both errors
eval :: Expr -> Env -> Either EvalErr (ValType, Env)
eval (Float f) env = Right (f, env)
eval (Var v) env =
  case getBinding v env of
    Just i -> Right (i, env)
    Nothing -> Left ("Undefined variable " ++ v)
eval (BinOp Plus left right) env =
  case (eval left env, eval right env) of
    (Right (lRes, _), Right (rRes, _)) -> Right (lRes + rRes, env)
    (_, _) -> Left "Error"
eval (BinOp Minus left right) env =
  case (eval left env, eval right env) of
    (Right (lRes, _), Right (rRes, _)) -> Right (lRes - rRes, env)
    (_, _) -> Left "Error"
eval (BinOp Times left right) env =
  case (eval left env, eval right env) of
    (Right (lRes, _), Right (rRes, _)) -> Right (lRes * rRes, env)
    (_, _) -> Left "Error"
eval (BinOp Divide left right) env =
  case (eval left env, eval right env) of
    (Right (lRes, _), Right (rRes, _)) -> Right (lRes / rRes, env)
    (_, _) -> Left "Error"
eval (BinOp Mod left right) env =
  case (eval left env, eval right env) of
    (Right (lRes, _), Right (rRes, _)) -> Right (mod' lRes rRes, env)
    (_, _) -> Left "Error"
eval (Assign var expr) env =
  case eval expr env of
    Right (res, newEnv) -> Right (res, setBinding var res newEnv)
    _ -> Left "Error evaluating assign"
