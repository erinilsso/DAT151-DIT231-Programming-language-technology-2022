{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn off unused import warning off in stub

module TypeChecker where

import Debug.Trace
import Control.Monad
import Control.Monad.Except
import Data.List (nub)
import Data.Maybe
import Data.Either
import Data.Map (Map, lookup)
import qualified Data.Map as Map
import CMM.Abs
import CMM.Print (printTree)

import qualified TypedSyntax as T

type AnnotatedProgram = T.Program
type TypeError = String
type M = Either TypeError
type Env = (Sig, Context)
type Sig = Map Id ([Type], Type)
type Context = [Block]
type Block = Map Id Type

typecheck :: Program -> M T.Program
typecheck (PDefs defs) = do
  -- Pass 1: create signature
  -- Pass 2: check definitions
  nenv <- initSig defs
  defs' <- mapM (checkDef nenv) defs
  -- Check for main
  let main = listToMaybe $ filter (\(DFun _ f _ _) -> f == Id "main") defs
  when (isNothing main) (Left "Expected a main function")
  let (DFun t _ _ _) = fromJust main
  unless (t == Type_int) (Left "Expected main to be of type `int`")
  return $ T.PDefs defs'

-- check the arguments of a function 
checkDef :: Env -> Def -> M T.Def
checkDef env (DFun t f args ss) = do
  -- check arguments, error out on duplicates
  tupList <- checkArgs args
  nenv <- foldM updateVarHelp (emptyonenv env) tupList
  (neonenv, typstmts) <- recursestm nenv [] ss
  return $ T.DFun t f args typstmts
  where
    updateVarHelp envvv (id1, typ1) = updateVar envvv id1 typ1
    emptyonenv (sigus, contextus) = (sigus, Map.empty : contextus)
    recursestm env tstm [] = return (env, tstm)
    recursestm env tstm (stmt:stmts) = case checkStm t env stmt of
      Right (nenvus, tstatm) -> recursestm nenvus (tstm ++ [tstatm]) stmts
      Left e -> Left e

--used in "checkDef", returns a list of pair of an argument and its type
checkArgs :: [Arg] -> M [(Id, Type)]
checkArgs args = do
  -- check for duplicates
  let argList = map (\(ADecl t x) -> (x, t)) args
  unless (null (filter (\(_, t) -> t == Type_void) argList)) (Left "Void argument found in function")
  let idList = map (\(x, t) -> x) argList
  if(idList == (nub idList)) then
    return $ map (\ (ADecl t x) -> (x, t)) args
    else Left "Duplicate arguments in function"

--initialize the functions into the environment
initSig :: [Def] -> M Env
initSig [] = Right initEnv
initSig ((DFun typ (Id "main") args stms):ds) = 
      if (typ == Type_int && (null args)) then
         do
            e <- initSig ds
            updatedEnv <- updateFun e (Id "main") ((map (\(ADecl t id) -> t) args), typ)
            return $ updatedEnv

         else Left "No valid main function"

initSig ((DFun typ id args stms):ds) = 
        do
            e <- initSig ds
            updatedEnv <- updateFun e id ((map (\(ADecl t id) -> t) args), typ)
            return $ updatedEnv

--check different types of statements and their type correctness
checkStm :: Type -> Env -> Stm -> M (Env, T.Stm)
checkStm typee envir@(sig, cxts) stam = case stam of
  SReturn e -> do
    e' <- checkExp envir e
    etyp <- tExpToType e' envir
    unless (subtype etyp typee) (Left "Incompatible return types")
    let retExp = castIntDouble (etyp == typee) e'
    return $ (envir, (T.SReturn typee retExp))
  SExp e -> do
    e' <- checkExp envir e
    t' <- tExpToType e' envir
    return $ (envir, (T.SExp t' e'))
  SDecls t ids -> do
    when (t == Type_void) (Left $ "Void variable declared")
    ncxt <- foldM addVarOfType envir ids
    return $ (ncxt, (T.SDecls t ids))
    where
      addVarOfType cxtt id = updateVar cxtt id t
  SInit t id exp -> do
    newEnv <- updateVar envir id t
    exptyp <- checkExp newEnv exp
    expressiontyp <- tExpToType exptyp newEnv
    if (subtype expressiontyp t) then do
      let retExp = castIntDouble (t == expressiontyp) exptyp
      return $ (newEnv, (T.SInit t id retExp))
    else Left "The init's expression's type is not the same as the variable's"
  SWhile e s -> do
    echeck <- checkExp envir e
    techeck <- tExpToType echeck envir
    unless (techeck == Type_bool) (Left "While expression is not a bool")
    (ncxt, scheck) <- checkStm typee (newBlock envir) s
    return $ (envir, (T.SWhile echeck scheck))
  SBlock ss -> do
    (ncxt, scheck) <- foldM bcheckstm (newBlock envir, []) ss
    return $ (envir, T.SBlock (reverse scheck))
    where
      bcheckstm (cxtt, stt) stmm = do
        (nc, ns) <- (checkStm typee cxtt stmm)
        return $ (nc, (ns:stt))
  SIfElse exp stm1 stm2 -> case checkExp envir exp of
    Right texp -> do
      expressiontyp <- tExpToType texp envir
      unless (expressiontyp == Type_bool) (Left "The conditions are not booleans")
      (cc, st) <- checkStm typee (newBlock envir) stm1
      (cc2, st2) <- checkStm typee (newBlock envir) stm2
      return $ (envir, T.SIfElse texp st st2)
    Left e -> Left e

--check different types of expression and their type correctness
checkExp :: Env -> Exp -> M T.Exp
checkExp env expr = case expr of
  EInt i -> return $ T.EInt i
  EBool b -> if b == LTrue then return (T.EBool True ) else return (T.EBool False)
  EDouble d -> return $ T.EDouble d
  EId id ->  do
                lookedTyp <- lookupVar env id
                case lookupVar env id of
                  Right _ -> return $ T.EId id
                  Left e -> Left e
                
  EApp id exps -> do 
                 retexps <- mapM (checkExp env) exps
                 (artyps, funtyp) <- lookupFun env id
                 unless (length retexps == length artyps) (Left $ "Expected " ++ show (length artyps) 
                                                  ++ " arguments for function `" ++ show id ++ "`, found " ++ show (length retexps))
                 let foundtyps = map (fromRight Type_void . (flip tExpToType env)) retexps
                 unless (and $ zipWith subtype foundtyps artyps) (Left $ "Invalid argument types for function `" ++ show id ++ "`")
                 let retExps = map (uncurry castIntDouble) $ zip (zipWith (==) foundtyps artyps) retexps
                 return $ T.EApp id retExps
  EPost id op -> do
                 rettype <- lookupVar env id
                 if isnum env rettype then
                  return $ T.EPost rettype id op
                  else Left $ "Post-operation expected a numeric variable but got `" ++ show id ++ "` of type `" ++ show rettype ++ "`"
  EPre op id -> do
                 rettype <- lookupVar env id
                 if isnum env rettype then
                  return $ T.EPre rettype op id
                 else Left $ "Pre-operation expected a numeric variable but got `" ++ show id ++ "` of type `" ++ show rettype ++ "`"
  EAdd ex1 op ex2 -> do
                 typ1 <- checkExp env ex1
                 typ2 <- checkExp env ex2
                 t1 <- tExpToType typ1 env
                 t2 <- tExpToType typ2 env
                 unless (all (isnum env) [t1, t2]) (Left $ "bad expression, incompatible types, for operator `" ++ show op ++ "`")
                 listus <- sequence $ map (flip tExpToType env) [typ1, typ2]
                 let endtyp = maximum listus
                 let leftExp = castIntDouble (t1 == endtyp) typ1
                 let rightExp = castIntDouble (t2 == endtyp) typ2
                 return $ T.EArith endtyp leftExp (addopToArithOp op) rightExp
  
  EMul ex1 op ex2 -> do
                 typ1 <- checkExp env ex1
                 typ2 <- checkExp env ex2
                 t1 <- tExpToType typ1 env
                 t2 <- tExpToType typ2 env
                 unless (all (isnum env) [t1, t2]) (Left $ "bad expression, incompatible types, for operator `" ++ show op ++ "`")
                 listus <- sequence $ map (flip tExpToType env) [typ1, typ2]
                 let endtyp = maximum listus
                 let leftExp = castIntDouble (t1 == endtyp) typ1
                 let rightExp = castIntDouble (t2 == endtyp) typ2
                 return $ T.EArith endtyp leftExp (mulopToArithOp op) rightExp
  ECmp ex1 op ex2 -> do
                 typ1 <- checkExp env ex1
                 typ2 <- checkExp env ex2
                 t1 <- tExpToType typ1 env
                 t2 <- tExpToType typ2 env
                 let isEq = if elem op [OEq, ONEq] then (t1 == t2) else False
                 let endtyp = maximum [t1, t2]
                 let leftExp = castIntDouble (t1 == endtyp) typ1
                 let rightExp = castIntDouble (t2 == endtyp) typ2
                 if isEq || all (isnum env) [t1, t2] then
                    return $ T.ECmp endtyp leftExp op rightExp
                 else Left $ "bad expression, incompatible types, for operator `" ++ show op ++ "`"
  EAnd ex1 ex2 -> do
                 typ1 <- checkExp env ex1
                 typ2 <- checkExp env ex2
                 if all (isbool env) [typ1, typ2] then
                    return $ T.ELogic T.OAnd typ1 typ2
                 else Left "bad expression, incompatible types, for operator `&&`"
  EOr ex1 ex2 -> do
                 typ1 <- checkExp env ex1
                 typ2 <- checkExp env ex2
                 if all (isbool env) [typ1, typ2] then
                    return $ T.ELogic T.OOr typ1 typ2
                 else Left "bad expression, incompatible types, for operator `||`"
  EAss id ex -> do
                 idtype <- lookupVar env id
                 extype <- checkExp env ex
                 endtyp <- tExpToType extype env
                 let retExp = castIntDouble (idtype == endtyp) extype
                 if(subtype endtyp idtype) then
                    return $ T.EAss id retExp
                 else Left $ "bad expression, RHS of assignment cannot be converted to `" ++ show id ++ "`'s type, `" ++ show idtype ++ "`"

--check if two give types are int and double to allow implicit type casting
subtype :: Type -> Type -> Bool
subtype t1 t2 = (t1 == Type_int && t2 == Type_double) || t1 == t2

castIntDouble :: Bool -> T.Exp -> T.Exp
castIntDouble True e = e
castIntDouble False e = T.EIntDouble e

--check if two typed expressions are the same
ofsametyp :: Env -> T.Exp -> T.Exp -> Bool
ofsametyp env exp1 exp2 = tExpToType exp1 env == tExpToType exp2 env

--check if a given type is numeric i.e. int or double
isnum :: Env -> Type -> Bool
isnum env texp = texp == Type_int || texp == Type_double

--check if a given typed expression is a bool
isbool :: Env -> T.Exp -> Bool
isbool env exp = tExpToType exp env == Right Type_bool

nyi :: String -> M a
nyi msg = throwError $ unwords [ "not yet implemented:", msg ]

--check if a given variable is in the environment
lookupVar :: Env -> Id -> M Type
lookupVar (sig,[]) i     = Left "variable does not exist1"
lookupVar (sig, (c:cs)) i = case Map.lookup i c of
  Just t -> return t
  Nothing -> lookupVar (sig, cs) i

--given a typed expression return the type
tExpToType :: T.Exp -> Env -> M Type
tExpToType texp env = case texp of
  T.EArith t ex1 op ex2 -> return t
  T.EPre t op id -> return t
  T.EPost t id op -> return t
  T.EBool b -> return Type_bool
  T.EInt i -> return Type_int
  T.EDouble d -> return Type_double
  T.ECmp t e op ex -> return Type_bool
  T.EId id -> lookupVar env id
  T.EApp id exps -> snd <$> lookupFun env id
  T.EAss id exp -> lookupVar env id
  T.ELogic op exp1 exp2 -> return Type_bool
  T.EIntDouble e -> return Type_double

--convert an AddOp in CMM.Abs. to ArithOp in typed syntax
addopToArithOp :: AddOp -> T.ArithOp
addopToArithOp op = case op of
                OPlus -> T.OPlus
                OMinus -> T.OMinus

--convert an MulOp in CMM.Abs.hs to ArithOp in typed syntax
mulopToArithOp :: MulOp -> T.ArithOp
mulopToArithOp op = case op of
                OTimes -> T.OTimes
                ODiv -> T.ODiv

--check if a variable is already defined or not and update the environment if it is not
updateVar :: Env -> Id -> Type -> M Env
updateVar (sig, []) id typ = Right $ (sig,[(Map.insert id typ Map.empty)])
updateVar env@(sig, (c:cs)) id typ = case Map.lookup id c of
  Just t -> Left $ "Variable `" ++ show id ++ "` already defined"
  Nothing -> Right $ (sig, (Map.insert id typ c):cs)

--lookup a function in the environment
lookupFun :: Env -> Id -> M ([Type], Type)
lookupFun (sig , cntxts) idFunc  = case Map.lookup idFunc sig of
    Just typ -> Right typ
    Nothing -> Left "Function not found"  

--check if a function is already defined if not update the environment
updateFun :: Env -> Id -> ([Type], Type) -> M Env
updateFun (sig, cxt) id (ts, t) = case Map.lookup id sig of
                      Just a -> Left "Function already defined"
                      Nothing -> Right $ (Map.insert id (ts, t) sig, cxt)

--create a new empty block and add it to the environment 
newBlock :: Env -> Env
newBlock (sig, cs) = (sig, (Map.empty:cs))

--create an empty environment
emptyEnv :: Env
emptyEnv = (Map.empty,[])
 
--initialize the empty environment with the built in functions
initEnv :: Env
initEnv =  ((Map.insert (Id "readDouble") ([], Type_double)
          (Map.insert (Id "readInt") ([], Type_int)
          (Map.insert (Id "printDouble") ([Type_double], Type_void)
          (Map.insert (Id "printInt") ([Type_int], Type_void) (fst emptyEnv)))))
          , (snd emptyEnv))
