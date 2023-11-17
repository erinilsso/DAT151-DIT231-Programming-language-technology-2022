module FunType where

import CMM.Abs


data FunType = FunType Type [Type]
    deriving Show

fromType :: Type -> String
fromType t = case t of 
    Type_int    -> "I"
    Type_double -> "D"
    _           -> ""