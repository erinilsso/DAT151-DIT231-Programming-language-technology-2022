{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn off unused import warning off in stub

-- | Compiler for C--, producing symbolic JVM assembler.

module Compiler where

import Debug.Trace
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import CMM.Print (printTree)

import Annotated
import Code
import FunType (FunType(..))









-- | Type signatures and JVM names of functions

type Sig = Map Id Fun

-- | Local variables

type Cxt = [CxtBlock]
--type CxtBlock = Map Id Type
type CxtBlock = [(Id, Type)]

-- | State for compiling a function definition

data St = St
  {  sig          :: Sig    -- Function signature (read-only)
  ,  cxt          :: Cxt    -- Context
  ,  limitLocals  :: Int    -- Maximal size for locals encountered
  ,  currentStack :: Int    -- Current stack size
  ,  limitStack   :: Int    -- Maximal stack size encountered
  ,  nextLabel    :: Label  -- Next jump label (persistent part of state)
  ,  output       :: Output -- Reversed code list (last instruction at front)
  }

initSt :: Sig -> St
initSt s = St
  { sig          = s
  , cxt          = [[]]
  , limitLocals  = 0
  , currentStack = 0
  , limitStack   = 0
  , nextLabel    = 0
  , output       = []
  }
-- Compiling a definition produces a (reversed) sequence of instructions
type Output = [Code]

-- Compilation monad
type Compile = State St

-- Built-in functions
builtin :: [(Id, Fun)]
-- builtin = [ (Id "printInt", Fun (Id "Runtime/printInt") $ FunType Type_void [Type_int])]

builtin = [ (Id "printInt", Fun (Id "Runtime/printInt")       $ FunType Type_void [Type_int])
          , (Id "printDouble", Fun (Id "Runtime/printDouble") $ FunType Type_void [Type_double])
          , (Id "readInt", Fun (Id "Runtime/readInt")         $ FunType Type_int  [])
          , (Id "readDouble", Fun (Id "Runtime/readDouble")   $ FunType Type_double [])]







-- | Entry point.

-- ^ Class name. -- ^ Type-annotated program.
-- ^ Generated jasmin source file content.
compile :: String -> Program -> String
compile name (PDefs defs) =
  unlines ([header] ++ (compileDefs sig0 defs)) -- (map ((compileDef sig0)) defs))
  
  where
    -- Signature maps function names to their Jasmin name and their function type
    sig0 = Map.fromList $ builtin ++ map sigEntry defs
    sigEntry def@(DFun _ f@(Id x) _ _) = (f,) $ Fun (Id $ name ++ "/" ++ x) $ funType def
    
    -- Header is fixed except for class @name@
    header :: String
    header = unlines $ concat
      [ [ ";; BEGIN HEADER"
        , ""
        , ".class public " ++ name
        , ".super java/lang/Object"
        , ""
        , ".method public <init>()V"
        , ".limit locals 1"
        , ""
        ]
      , map indent
        [ "aload_0"
        , "invokespecial java/lang/Object/<init>()V"
        , "return"
        ]
      , [ ""
        , ".end method"
        , ""
        , ".method public static main([Ljava/lang/String;)V"
        , ".limit locals 1"
        , ".limit stack  1"
        , ""
        ]
      , map indent
        [ "invokestatic " ++ name ++ "/main()I"
        , "pop"
        , "return"
        ]
      , [ ""
        , ".end method"
        , ""
        , ";; END HEADER"
        ]
      ]

-- | Indent non-empty lines.

indent :: String -> String
indent s = if null s then s else "\t" ++ s


compileDefs :: Sig -> [Def] -> [String]
compileDefs _ [] = []
compileDefs sig (def:defs) = ((compileDef sig def) ++ (compileDefs sig defs))

compileDef :: Sig -> Def -> [String]
compileDef sig0 def@(DFun t f args ss) = concat
  -- Output function header
  [ [ ""
    , ".method public static " ++ toJVM (Fun f $ funType def)
    ]
  -- Output limits
  , [ ".limit locals " ++ show (limitLocals st)
    , ".limit stack "  ++ show (limitStack  st)
    ]
  -- Output code
  , map (indent . toJVM) $ reverse (output st)
  -- Output function footer
  ,  [ "" 
    , ".end method"
    ]
  ]
  where
    st = execState (compileFun t args ss) $ initSt sig0


compileFun :: Type -> [Arg] -> [Stm] -> Compile ()
compileFun t args ss = do
  mapM_ (\ (ADecl t' x) -> newVar x t') args
  mapM_ compileStm ss
  when (not (foldr (||) (False) (map checkRetrunStm ss))) (emit $ Return Type_void)



checkRetrunStm :: Stm -> Bool
checkRetrunStm (SReturn _ _) = True
checkRetrunStm _ = False

-- Compile statement

compileStm :: Stm -> Compile ()
compileStm s0 = do

  -- Output a comment with the statement to compile
  let top = stmTop s0
  unless (null top) $ do
    blank
    mapM_ comment (lines top)

  -- Compile the statement
    case s0 of
      SInit t x e -> do
          newVar x t
          compileExp e
          (a, _) <- lookupVar x
          emit $ Store t a
      SDecls t x -> do
        newVarMult x t
      SExp t e -> do
        compileExp e
        emit $ Pop t
      SReturn t e -> do
        compileExp e
        emit $ Return t

      SBlock stms -> do
        newBlock
        mapM_ compileStm stms
        exitBlock
      SWhile exp stm -> do
        testWhile <- newLabel
        endWhile <- newLabel

        emit $ Label testWhile
        compileExp exp
        newBlock
        emit $ (If OEq endWhile)
        compileStm stm
        exitBlock
        emit $ Goto testWhile
        emit $ Label endWhile

      SIfElse exp stm1 stm2 -> do
        ifLabel <- newLabel 
        elseLabel <- newLabel
        compileExp exp
        emit (If OEq elseLabel)
        newBlock
        compileStm stm1
        exitBlock
        emit (Goto ifLabel)
        emit (Label elseLabel)
        newBlock
        compileStm stm2
        exitBlock
        emit (Label ifLabel)
      -- s -> error $ "Not yet implemented: compileStm " ++ show s--printTree s


-- Compile a boolean so that the jump is taken if the condition is @cond@,
-- and fall through otherwise

compileCond :: Bool -> Label -> Exp -> Compile ()
compileCond cond l exp = case exp of
  e -> do
    compileExp e
    emit $ (if cond then IfNZ l else IfZ l)


--Compile an expression to leave its value on the stack

compileExp :: Exp -> Compile ()
compileExp exp = case exp of
  EBool bool -> case bool of
    True -> emit $ IConst  1
    False -> emit $ IConst 0

  EInt i -> do 
    emit $ IConst i
  
  EDouble d -> do
    emit $ DConst d

  EId x  -> do
      (a, t) <- lookupVar x
      emit $ Load t a

  EApp id ex -> do
      mapM_ compileExp ex
      f <- lookupFun id
      emit $ Call f
  EPost t id op -> case op of
    OInc -> do
      (a,t) <- lookupVar id
      emit $ Load t a
      emit $ Dup t
      emit $ Inc t a 1
    ODec -> do
      (a,t) <- lookupVar id
      emit $ Load t a
      emit $ Dup t
      emit $ Inc t a (-1)
  EPre t op id -> case op of 
    OInc -> do
      (a, t) <- lookupVar id
      emit $ Load t a
      emit $ Inc t a 1
      emit $ Load t a
    ODec -> do
      (a, t) <- lookupVar id
      emit $ Load t a
      emit $ Inc t a (-1)
      emit $ Load t a

  EArith t e1 op e2 -> case op of
    OTimes -> do
      compileExp e1 
      compileExp e2 
      emit $ Arith t OTimes
    ODiv -> do
      compileExp e1 
      compileExp e2 
      emit $ Arith t ODiv
    OPlus -> do
      compileExp e1 
      compileExp e2 
      emit $ Arith t OPlus
    OMinus -> do
      compileExp e1 
      compileExp e2 
      emit $ Arith t OMinus

  ECmp t e1 op e2 -> do
    truee <- newLabel
    emit $ IConst 1
    compileExp e1
    compileExp e2
    emit $ IfCmp t op truee
    emit $ Pop Type_int
    emit $ IConst 0
    emit $ Label truee
  EIntDouble e -> do
    compileExp e
    emit I2D
    -- emit $ Goto end
    -- emit $ Label truee
    -- emit $ IConst 1
    -- emit $ Label end


  ELogic op e1 e2 -> do
    compileExp e1
    falsee <- newLabel
    fin  <- newLabel
    emit (If OEq falsee)
    case op of
      OAnd -> do
        compileExp e2
        emit (Goto fin)
        emit (Label falsee)
        emit (IConst 0)
        emit (Label fin)

      OOr -> do
        emit (IConst 1)
        emit (Goto fin)
        emit (Label falsee)
        compileExp e2
        emit (Label fin)

          

  EAss x e -> do
      compileExp e
      (a, t) <- lookupVar x
      emit $ Dup t
      emit $ Store t a
      -- emit $ Load  t a


  -- e -> error $ "Not yet implemented: compileExp " ++ show e --printTree e

-- subtype :: Exp -> Exp ->








  -- Labels

newLabel :: Compile Label
newLabel = do
  l <- gets nextLabel
  modify $ \ st -> st { nextLabel = succ l }
  return $ l

-- Variable handling

inNewBlock :: Compile a -> Compile a
inNewBlock cont = do
  modify $ \ st -> st { cxt = [] : cxt st }
  a <- cont
  modify $ \ st -> st { cxt = tail $ cxt st }
  return a

newBlock :: Compile ()
newBlock = do
  modify $ \ st -> st  {  cxt = [] : cxt st }
exitBlock :: Compile ()
exitBlock = do
  modify $ \ st -> st { cxt = tail $ (traceShowId (cxt st)) }


-- newVarMult :: [Id] -> Type -> Compile ()
-- newVarMult [] _ = pure ()
-- newVarMult (id:ids) t = do 
--   newVar id t
--   newVarMult ids t

newVarMult :: [Id] -> Type -> Compile ()
newVarMult ids t = mapM_ (flip newVar t) ids

newVar :: Id -> Type -> Compile ()
newVar x t = do
  modify $ \ st@St{ cxt = (b:bs) } -> st { cxt = ((x,t):b):bs}
  updateLimitLocals

-- Return adresses and type of variable
lookupVar :: Id -> Compile (Addr, Type)
lookupVar x = loop . concat <$> gets cxt
  where
    loop [] = error $ "unbound variable " ++ printTree x
    loop ((y,t) :bs)
      | x == y    = (size bs, t)
      | otherwise = loop bs

updateLimitLocals :: Compile ()
updateLimitLocals = do
  old <- gets limitLocals
  new <- size <$> gets cxt
  when (new > old) $ modify $ \ st -> st { limitLocals = new }







incStack :: Size t => t -> Compile ()
incStack t = modStack (size t)

-- Note size t can be negative thus decStack can also increase the stack limit

decStack :: Size t => t -> Compile ()
decStack t = modStack (0 - size t)

decStack2 :: Fun -> Compile ()
decStack2 (Fun (Id f) (FunType t1 ts)) = modStack (0 - (size t1 + length ts))

modStack :: Int -> Compile ()
modStack n = do
  new <- (n +) <$> gets currentStack
  modify $ \ st -> st { currentStack = new }
  old <- gets limitStack
  when (new > old) $ modify $ \ st -> st { limitStack = new }



-- Emitting statements
-- Print a single instruction and update the stack limits

emit :: Code -> Compile ()
-- Handling of void

emit (Store Type_void _) = return ()
emit (Load Type_void _) = return ()
emit (Dup Type_void) = return ()
emit (Pop Type_void) = return ()


emit (Inc t@Type_int a k) = do
  emit $ IConst $ fromIntegral k
  emit $ Arith t OPlus
  emit $ Store t a

-- Implementation of some double operations

emit (Inc t@Type_double a k) = do
  emit $ DConst $ fromIntegral k
  emit $ Arith t OPlus
  emit $ Store t a

emit (IfCmp Type_double o l) = do
  emit $ DCmp
  emit $ If o l

emit c = do
  modify $ \ st@St{ output = cs } -> st{ output = (c:cs)}
  adjustStack c


adjustStack :: Code -> Compile ()
adjustStack code = case code of
  Store t _    -> decStack t
  Load t _     -> incStack t
  IConst _     -> incStack Type_int
  DConst _     -> incStack Type_double
  Dup t        -> incStack t
  Pop t        -> decStack t
  Return t     -> decStack t
  Call f       -> decStack f
  Label{}      -> return ()
  Goto {}      -> return ()
  If _ _       -> decStack Type_int
  IfCmp t _ _  -> decStack t >> decStack t
  DCmp         -> decStack Type_double >> decStack Type_double >> incStack Type_int
  Inc{}        -> return ()
  Arith t _    -> decStack t
  I2D          -> decStack Type_int >> incStack Type_double
  Comment _    -> return ()

-- Comments
-- Print a comment

comment :: String -> Compile ()
comment = emit . Comment

-- Print an empty line

blank :: Compile ()
blank = comment ""


-- Get top part of statement (for comments)

stmTop :: Stm -> String
stmTop st = case st of
  SWhile e _      -> "while (" ++ show e ++ ")" --"while (" ++ printTree e ++ ")"
  SIfElse e _ _   -> "if (" ++ show e ++ ")" --"if (" ++ printTree e ++ ")"
  SBlock _      -> "Block"
  s             -> show s --printTree s

-- Auxilirary functions

funType :: Def -> FunType
funType (DFun re _ args _) = FunType re $ map (\ (ADecl t _) -> t) args











lookupFun :: Id -> Compile Fun
lookupFun x = do
  m <- gets sig
  return $ Map.findWithDefault (error $ "unknown function" ++ printTree x) x m