{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn off unused import warning off in stub
{-# OPTIONS_GHC -Wno-unused-matches #-} -- Turn off unused binding warning off in stub

-- | Interpreter for lambda-calculus with if, +, -, <.
--
--   Strategy can be either call-by-value or call-by-name.

module Interpreter (interpret, Strategy(..)) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import Fun.Abs
import Fun.Print

-- | Evaluation strategy.

data Strategy
  = CallByName
  | CallByValue
  deriving (Eq, Show)

-- | Error monad.

type Err = Except String

-- | Entry point: Program computes a number.

interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs (DMain mainExp)) = do

  -- Run the interpreter
  v <- eval cxt mainExp
  -- Return the result


  case v of
    VInt i -> return i
    VAbs {} -> throwError "main should return an integer"

  where
    cxt = Cxt
      { cxtStrategy = strategy
      , cxtSig      = Map.fromList $ map (\ (DDef f xs e) -> (f, mkDef xs e)) defs
      , cxtEnv      = Map.empty
      }

    -- Turn f x1 --- xn = e into f = \ x1 -> ... \ xn -> e
    mkDef :: [Ident] -> Exp -> Exp
    mkDef xs e = foldr EAbs e xs

-- Data structures for the interpreter --

-- Context
data Cxt = Cxt { cxtStrategy  :: Strategy -- Evaluation strategy (fixed)
               , cxtSig       :: Signature
               , cxtEnv       :: Env
               }

type Signature = Map Ident Exp

type Env = Map Ident ValClos

data ValClos = V Value | C Env Exp

-- Values
data Value = VInt Integer
           | VAbs Env Ident Exp  -- Function closure


-- Interpreter --
eval :: Cxt -> Exp -> Err Value
eval cxt exp = case exp of
-- evaluation rules
  EInt i -> return $ VInt i

-- Variable
  EVar x -> do
    case Map.lookup x (cxtEnv cxt) of
      Just v -> case v of
        V va    -> return va
        C en ex -> eval (cxt { cxtEnv = en }) ex
        
      Nothing -> case Map.lookup x (cxtSig cxt) of
        Just e -> eval (cxt { cxtEnv = Map.empty }) e
        Nothing -> throwError $ unwords ["unbound variable", printTree x]


-- Lambda
  EAbs x e -> return $ VAbs (cxtEnv cxt) x e

-- Application
  EApp e1 e2 -> do
    f  <- eval cxt e1

    case cxtStrategy cxt of
-- Call-by-Value
      CallByValue -> do
        v1  <- eval cxt e2
        case f of
          VInt{} -> throwError $ unwords [ "can not apply an integer" ]
          VAbs delta x e -> eval (cxt { cxtEnv = Map.insert x (V v1) delta}) e -- use the value V

-- Call-by-Name  
      CallByName -> do
        case f of
          VInt{} -> throwError $ unwords [ "can not apply an integer" ]
          VAbs delta x e -> do 
            eval (cxt { cxtEnv = Map.insert x (C (cxtEnv cxt) e2) delta}) e -- use the "C" closure instead of actual value

--Addition
  EAdd e1 e2 -> do
    ne1 <- eval cxt e1
    ne2 <- eval cxt e2
    case (ne1, ne2) of
      (VInt i1, VInt i2) -> return $ VInt (i1 + i2)
      _ -> error $ unwords [ "Addition is not with integers" ]

--Subtraction
  ESub e1 e2 -> do
    ne1 <- eval cxt e1
    ne2 <- eval cxt e2
    case (ne1, ne2) of
      (VInt i1, VInt i2) -> return $ VInt (i1 - i2)
      _ -> error $ unwords [ "Addition is not with integers" ]

--Less than comparison
  ELt e1 e2 -> do
    ne1 <- eval cxt e1
    ne2 <- eval cxt e2
    case (ne1, ne2) of
      (VInt i1, VInt i2) -> if i1 < i2
                            then return $ VInt 1
                            else return $ VInt 0
      _ -> error $ unwords [ "Comparison is not with integers" ]

--IfElse condition
  EIf con e1 e2 -> do
    bool <- eval cxt con
    case bool of
      VInt i -> if i == 1
                then eval cxt e1
                else eval cxt e2
      _      -> error $ unwords [ "If-condition did not return a bool(VInt)" ] 
