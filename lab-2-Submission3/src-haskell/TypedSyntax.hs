{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The typed abstract syntax of language CMM.

module TypedSyntax (module TypedSyntax, module X) where

import CMM.Abs as X (Arg(..), IncDecOp(..), CmpOp(..), Type(..), Id(..))

data Program = PDefs [Def]
  deriving (Eq, Show)

data Def = DFun Type Id [Arg] [Stm]
  deriving (Eq, Show)

data Stm
    = SExp Type Exp
    | SDecls Type [Id]
    | SInit Type Id Exp
    | SReturn Type Exp
    | SWhile Exp Stm
    | SBlock [Stm]
    | SIfElse Exp Stm Stm
  deriving (Eq, Show)

data Exp
    = EBool Bool
    | EInt Integer
    | EDouble Double
    | EId Id
    | EApp Id [Exp]
    | EPost Type Id IncDecOp
    | EPre Type IncDecOp Id
    | EArith Type Exp ArithOp Exp
    | ECmp Type Exp CmpOp Exp
    | ELogic LogicOp Exp Exp
    | EAss Id Exp
    | EIntDouble Exp
  deriving (Eq, Show)

data ArithOp = OTimes | ODiv | OPlus | OMinus
  deriving (Eq, Show)

data LogicOp = OAnd | OOr
  deriving (Eq, Show)
