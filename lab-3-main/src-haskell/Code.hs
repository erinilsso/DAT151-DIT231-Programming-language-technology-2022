{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE PatternSynonyms #-}

module Code where

import Annotated (ArithOp(..))
import CMM.Abs (Id(..), Type(..), CmpOp(..))
import FunType (FunType(..))


-- | Idealized JVM instructions.
data Fun = Fun {funId :: Id, funFunType :: FunType}
           deriving Show
--   Some idealized instructions (e.g., @Inc Type_double@)
--   decompose into several real JVM instructions.

type Label = Int

type Addr = Int

data Code
    = Store Type Addr    -- ^ Store stack content of type @Type@ in local variable @Addr@.
    | Load Type Addr     -- ^ Push stack content of type @Type@ from local variable @Addr@.
    | IConst Integer     -- ^ Put integer constant on the stack.
    | DConst Double      -- ^ Put floating point constant on the stack.
    | Dup Type           -- ^ Duplicate top of stack.
    | Pop Type           -- ^ Pop something of type @Type@ from the stack.
    | Return Type        -- ^ Return from method of type @Types.
    | Call Fun           -- ^ Call function.
    
    | Label Label        -- ^ Define label.
    | Goto Label          -- ^ Jump to label.
    | If CmpOp Label      -- ^ If top of stack is @`op`0@, jump to label.
    | IfCmp Type CmpOp Label
                         -- ^ If prev `ор' top, jump.
    | DCmp               -- ^ If prev > top, then 1, if prev == top, 0, if prev < top, -1.
                         -- In case we do not care about NaN, this can be printed to either
                         -- @dcmpg@ or @dcmpl@.
    | Inc Type Addr Int  -- ^ In/decrease variable by small number
    | Arith Type ArithOp -- ^ Add/multiply 2 top values of stack.
                         -- Subtract/divide top of stack from previous of stack.
    | I2D                -- ^ Convert top of the stack int to double.

    | Comment String     -- ^ comment (not including "::"). @null@ for blank line.
    
    deriving (Show)

pattern IfZ l = If OEq l
pattern IfNZ l = If ONEq l

negateCmp :: CmpOp -> CmpOp
negateCmp op = case op of
    OEq -> ONEq

class ToJVM a where
  toJVM :: a -> String

instance ToJVM Type where
  toJVM t = case t of
    Type_int    -> "I"
    Type_double -> "D"
    Type_void   -> "V"
    Type_bool   -> "Z"

instance ToJVM ArithOp where
  toJVM op = case op of
    OPlus -> "add"
    OMinus -> "sub"
    OTimes -> "mul"
    ODiv -> "div"

instance ToJVM CmpOp where
  toJVM op = case op of
    OEq -> "eq" 
    ONEq -> "ne"
    OLt -> "lt"
    OLtEq -> "le"
    OGt -> "gt"
    OGtEq -> "ge"

instance ToJVM FunType where
  toJVM (FunType t ts) = "(" ++ (ts >>= toJVM)++ ")" ++ toJVM t

instance ToJVM Fun where
  toJVM (Fun (Id f) t) = f ++ toJVM t

instance ToJVM Label where
  toJVM l = "L" ++ show l

instance ToJVM Code where
  toJVM c = case c of 
    Store t n -> prefixSmall t ++ "store " ++ show n
    Load t n  -> prefixSmall t ++ "load " ++ show n
    Return t  -> prefixSmall t ++ "return"
    Call f    -> "invokestatic " ++ toJVM f
    DConst d  -> "ldc2_w " ++ show d 
    
    IConst i
            | i == -1          -> "iconst_m1"
            | i >= 0 && i <= 5 -> "iconst_" ++ show i
            | isByte i         -> "bipush " ++ show i
            | otherwise        -> "ldc " ++ show i
    Dup Type_double -> "dup2"
    Dup _           -> "dup"
    Pop Type_double -> "pop2"
    Pop _           -> "pop"

    Label l -> toJVM l ++ ":"
    Goto l  -> "goto " ++ toJVM l
    If op l -> "if" ++ toJVM op ++ " " ++ toJVM l
    
    c@(IfCmp Type_double _ _) -> impossible c
    c@(IfCmp Type_void _ _) -> impossible c

    IfCmp _ op l     -> "if_icmp" ++ toJVM op ++ " " ++ toJVM l

    DCmp             -> "dcmpg" -- Note: dcmpg and dcmpl only differ in the treatment of NaN

    Inc Type_int a k -> "iinc " ++ show a ++ " " ++ show k
    c@Inc{}          -> impossible c

    Arith t op       -> prefixSmall t ++ toJVM op

    I2D              -> "i2d"

    Comment ""       -> ""
    Comment s        -> ";; " ++ s

    where impossible c = error $ "Impossible toJVM " ++ show c

prefix :: Type -> String
prefix t = case t of
  Type_double -> "D"
  Type_int    -> "I"
  Type_bool   -> "Z"
  Type_void   -> ""

prefixSmall :: Type -> String
prefixSmall t = case t of
  Type_double -> "d"
  Type_int    -> "i"
  Type_bool   -> "i"
  Type_void   -> ""


isByte :: Integer -> Bool
isByte i =  i >= (-128) && i<= 127

class Size a where
  size :: a -> Int

instance Size Type where
  size t = case t of
    Type_int    -> 1
    Type_double -> 2
    Type_bool   -> 1
    Type_void   -> 0

instance Size Id where
     size _ = 0
 
instance (Size a, Size b) => Size(a,b) where
     size (x,y) = size x + size y
 
instance Size a => Size [a] where
     size = sum . map size
 
instance Size FunType where 
     size (FunType t ts) = size ts - size t 
instance Size Fun where
  size (Fun _ ft) = size ft