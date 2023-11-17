{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn off unused import warning off in stub

module Interpreter where
import Debug.Trace
import Control.Monad
import Data.Maybe
import Data.Map (Map, lookup, fromList)
import qualified Data.Map as Map
import TypedSyntax

data Value
  = VInt Integer
  | VDouble Double
  | VBool Bool
  | VNull
  deriving (Eq, Show)

printV :: Value -> IO ()
printV (VInt i) = putStrLn $ show i
printV (VDouble d) = putStrLn $ show d
printV (VBool b) = putStrLn $ show b

type M = Either TypeError
type TypeError = String
type Env = (Sig, [Context])
type Sig = Map Id Def
type Context = Block
type Block = Map Id Value

printValue :: Value -> String
printValue = \case
  VInt i    -> show i
  VDouble d -> show d
  VBool b   -> show b
  VNull     -> "undefined value"

interpret :: Program -> IO ()
interpret (PDefs defs) = do
  -- Preparation: create the signature which maps functions names to their definition
  -- Find the main function.  We can assume it exists.
  (sigg, cxts) <- initSig defs
  let main = listToMaybe $ filter (\(DFun _ f _ ss) -> f == Id "main") defs
  let (Just (DFun t _ _ ss)) = main
  -- Execute its statements
  execMultiple t ((sigg, Map.empty:cxts), VNull) ss
  return ()

initSig :: [Def] -> IO Env
initSig [] = return initEnv
initSig ((DFun typ id args stms):ds) = 
        do
          e <- initSig ds
          updatedEnv <- updateFun e (DFun typ id args stms)
          return $ updatedEnv

execMultiple :: Type -> (Env, Value) -> [Stm] -> IO (Env, Value)
execMultiple t (env, VNull) (stm:stms) = do
  (ee, vv) <- exec t env stm
  case stm of
    SReturn _ _-> return (ee, (castIfNeeded t vv))
    _ ->         execMultiple t (ee, vv) stms
execMultiple _ envVal _  = return envVal

exec :: Type -> Env -> Stm -> IO (Env, Value)
exec t env s = case s of
  SReturn t e -> do
    (nenv, v) <- eval env e
    return (nenv, v)
  SExp t e -> do
    (nenv, v) <- eval env e
    return (nenv, VNull)
  SDecls t ids -> do
    nenv <- foldM addVarOfType env ids
    return $ (nenv, VNull) 
    where
      addVarOfType envv id = initVar envv id VNull
  SInit t id exp -> do
    (nenv, v) <- eval env exp
    updatedEnv <- initVar nenv id (castIfNeeded t v)
    return (updatedEnv, VNull)
  SWhile exp stm -> do 
    (nenv, v) <- eval env exp
    case v of
      VBool True -> do
        (updatedEnv, rval) <- exec t (newBlock nenv) stm
        case rval of
          VNull -> exec t (remBlock updatedEnv) (SWhile exp stm)
          value -> return (remBlock updatedEnv, value)
      _ -> return (nenv, VNull)
  SBlock stms -> do
    (rnenv, rval) <- execMultiple t ((newBlock env), VNull) stms
    return $ ((remBlock rnenv), rval)
  SIfElse exp stm1 stm2 -> do
    (nenv, v) <- eval env exp
    case v of
      VBool True -> do
        (updatedEnv, rval) <- exec t (newBlock nenv) stm1
        return $ ((remBlock updatedEnv), rval)
      VBool False -> do
        (updatedEnv, rval) <- exec t (newBlock nenv) stm2
        return $ ((remBlock updatedEnv), rval)

evalMult :: Env -> [Exp] -> IO (Env, [Value])
evalMult e [] = return (e, [])
evalMult e (exp:exps) = do
              (ee, v) <- eval e exp
              (eee, vs) <- evalMult ee exps
              return (eee, v:vs)

eval :: Env -> Exp -> IO (Env, Value)
eval env@(siggs, blcks) exp = case exp of
  EInt i -> return $ (env, VInt i)
  EBool b -> return $ (env, VBool b)
  EDouble d -> return $ (env, VDouble d)
  EId id -> do
    vl <- lookupVar env id
    when (vl == VNull) (fail $ "Variable `" ++ show id ++ "` used before initialization")
    return $ (env,vl)
  EApp id exps ->
    case id of
      Id "printInt" -> do
        (ee, (v:vs)) <- evalMult env exps
        printV v
        return (ee, VNull)
      
      Id "printDouble" -> do
        (ee, (v:vs)) <- evalMult env exps
        printV (castIfNeeded Type_double v)
        return (ee, VNull)

      Id "readInt" -> do
        intt <- getLine
        return (env, VInt (read intt))

      Id "readDouble" -> do
        doublee <- getLine
        return (env, VDouble (read doublee))
      
      _ ->
        case Map.lookup id (fst env) of
          Just (DFun tpp iden args stms) -> do
                        let idents = [iis | (ADecl t iis) <- args]
                        let types = [t | (ADecl t iis) <- args]
                        (ee, vvs) <- evalMult env exps
                        let zippedVals = zip idents (zipWith castIfNeeded types vvs)
                        let newblocktoadd = [Data.Map.fromList zippedVals]
                        (_, finVal) <- execMultiple tpp ((siggs, newblocktoadd), VNull) stms
                        return (ee, finVal)
          Nothing -> fail "Function not found"
        where
          evalApp (envv, vv) expss = do 
            (nc, ns) <- (eval envv expss)
            return $ (nc, (ns))

  EPost t id op -> do
    lookedVar <- lookupVar env id
    case lookedVar of
      VInt i -> do 
        updatedEnv <- updateVar env id (VInt (functiongiver i))
        return (updatedEnv, (VInt i))
      VDouble d -> do
        updatedEnv <- updateVar env id (VDouble (functiongiver d))
        return (updatedEnv, (VDouble d))
      _ -> fail "variable is not int nor double"
    where
      functiongiver :: (Num a) => a -> a 
      functiongiver = case op of
                            OInc -> (+) 1
                            ODec -> flip (-) 1
  EPre t op id -> do
    lookedVar <- lookupVar env id
    case lookedVar of
      VInt i -> do
        case op of
          OInc -> do
            updatedEnv <- updateVar env id (VInt (i + 1))
            return (updatedEnv, (VInt (i + 1)))
          ODec -> do
            updatedEnv <- updateVar env id (VInt (i - 1))
            return (updatedEnv, (VInt (i - 1)))

      VDouble d -> do
        case op of
          OInc -> do
            updatedEnv <- updateVar env id (VDouble (d + 1))
            return (updatedEnv, (VDouble (d + 1)))
          ODec -> do
            updatedEnv <- updateVar env id (VDouble (d - 1))
            return (updatedEnv, (VDouble (d - 1)))
      _ -> fail "variable is not int nor double"

  EArith t exp1 op exp2 -> do
    (nenv1, v1) <- eval env exp1
    (nenv2, v2) <- eval nenv1 exp2
    case (v1,v2,op) of
      (VInt i1, VInt i2, OPlus) -> return (nenv2, VInt (i1 + i2))
      (VInt i1, VInt i2, OMinus) -> return (nenv2, VInt (i1 - i2))
      (VDouble d1, VDouble d2, OPlus) -> return (nenv2, VDouble (d1 + d2))
      (VDouble d1, VDouble d2, OMinus) -> return (nenv2, VDouble (d1 - d2))
      (VInt i1, VDouble d2, OPlus) -> return (nenv2, VDouble ((fromInteger i1) + d2))
      (VDouble d1, VInt i2, OPlus) -> return (nenv2, VDouble (d1 + (fromInteger i2)))
      (VInt i1, VDouble d2, OMinus) -> return (nenv2, VDouble ((fromInteger i1) - d2))
      (VDouble d1, VInt i2, OMinus) -> return (nenv2, VDouble (d1 - (fromInteger i2)))

      (VInt i1, VInt i2, OTimes) -> return (nenv2, VInt (i1 * i2))
      (VInt i1, VInt i2, ODiv) -> if i2 == 0 then fail "Division by zero is not allowed"
                                    else return (nenv2, VInt (i1 `div` i2))
      (VDouble d1, VDouble d2, OTimes) -> return (nenv2, VDouble (d1 * d2))
      (VDouble d1, VDouble d2, ODiv) -> if d2 == 0 then fail "Division by zero is not allowed"
                                          else return (nenv2, VDouble (d1 / d2))
      (VInt i1, VDouble d2, OTimes) -> return (nenv2, VDouble ((fromInteger i1) * d2))
      (VDouble d1, VInt i2, OTimes) -> return (nenv2, VDouble (d1 * (fromInteger i2)))
      (VInt i1, VDouble d2, ODiv) -> if d2 == 0 then fail "Division by zero is not allowed"
                                          else return (nenv2, VDouble ((fromInteger i1) / d2))
      (VDouble d1, VInt i2, ODiv) -> if i2 == 0 then fail "Division by zero is not allowed"
                                          else return (nenv2, VDouble (d1 / (fromInteger i2)))

  ECmp t exp1 op exp2 -> do
    (nenv1, v1) <- eval env exp1
    (nenv2, v2) <- eval nenv1 exp2
    case (v1,v2,op) of
      (VInt i1, VInt i2, OLt) -> return (nenv2, VBool (i1 < i2))
      (VInt i1, VInt i2, OGt) -> return (nenv2, VBool (i1 > i2))
      (VDouble d1, VDouble d2, OLt) -> return (nenv2, VBool (d1 < d2))
      (VDouble d1, VDouble d2, OGt) -> return (nenv2, VBool (d1 > d2))
      (VInt i1, VDouble d2, OGt) -> return (nenv2, VBool ((fromInteger i1) > d2))
      (VDouble d1, VInt i2, OGt) -> return (nenv2, VBool (d1 > (fromInteger i2)))
      (VInt i1, VDouble d2, OLt) -> return (nenv2, VBool ((fromInteger i1) < d2))
      (VDouble d1, VInt i2, OLt) -> return (nenv2, VBool (d1 < (fromInteger i2)))

      (VInt i1, VInt i2, OEq) -> return (nenv2, VBool (i1 == i2))
      (VInt i1, VInt i2, ONEq) -> return (nenv2, VBool (i1 /= i2))
      (VDouble d1, VDouble d2, OEq) -> return (nenv2, VBool (d1 == d2))
      (VDouble d1, VDouble d2, ONEq) -> return (nenv2, VBool (d1 /= d2))
      (VInt i1, VDouble d2, OEq) -> return (nenv2, VBool ((fromInteger i1) == d2))
      (VDouble d1, VInt i2, OEq) -> return (nenv2, VBool (d1 == (fromInteger i2)))
      (VInt i1, VDouble d2, ONEq) -> return (nenv2, VBool ((fromInteger i1) /= d2))
      (VDouble d1, VInt i2, ONEq) -> return (nenv2, VBool (d1 /= (fromInteger i2)))

      (VBool b1, VBool b2, OEq) -> return (nenv2, VBool (b1 == b2))
      (VBool b1, VBool b2, ONEq) -> return (nenv2, VBool (b1 /= b2))


      (VInt i1, VInt i2, OGtEq) -> return (nenv2, VBool (i1 >= i2))
      (VInt i1, VInt i2, OLtEq) -> return (nenv2, VBool (i1 <= i2))
      (VDouble d1, VDouble d2, OGtEq) -> return (nenv2, VBool (d1 >= d2))
      (VDouble d1, VDouble d2, OLtEq) -> return (nenv2, VBool (d1 <= d2))
      (VInt i1, VDouble d2, OGtEq) -> return (nenv2, VBool ((fromInteger i1) >= d2))
      (VDouble d1, VInt i2, OGtEq) -> return (nenv2, VBool (d1 >= (fromInteger i2)))
      (VInt i1, VDouble d2, OLtEq) -> return (nenv2, VBool ((fromInteger i1) <= d2))
      (VDouble d1, VInt i2, OLtEq) -> return (nenv2, VBool (d1 <= (fromInteger i2)))
      _ -> fail "Not a valid artithmetic operation"

  ELogic op exp1 exp2 -> do
    (nenv1, v1) <- eval env exp1
    case (v1,op) of
      (VBool False, OAnd) -> return (nenv1, VBool False)
      (VBool True, OOr) ->   return (nenv1, VBool True)
      (vv,opp) -> eval nenv1 exp2
  
  EAss id exp -> do
    (nenv, v) <- eval env exp
    case v of
      VNull -> fail "Variable uninitialized in assignment. Variable is "
      otherwise -> do
        updatedEnv <- updateVar nenv id v
        return (updatedEnv, v)
  EIntDouble exp -> do
    (nenv, VInt v) <- eval env exp
    return (nenv, VDouble $ fromInteger v)

nyi msg = error $ unwords [ "not yet implemented", msg ]

castIfNeeded :: Type -> Value -> Value
castIfNeeded Type_double (VInt i) = (VDouble . fromInteger) i
castIfNeeded _ v = v

lookupVar :: Env -> Id -> IO Value
lookupVar (sig,[]) i     = fail "variable does not exist"
lookupVar (sig,(c:cs)) i = case Data.Map.lookup i c of
    Just val -> return val
    Nothing -> lookupVar (sig,cs) i

updateVar :: Env -> Id -> Value -> IO Env
updateVar (sig,[]) id v = fail "Variable not defined yet"
updateVar (sig,(b:bs)) id v = case Map.lookup id b of
                      Just a -> return (sig, (Map.insert id v b):bs)
                      Nothing -> do
                        (sigg, bbs) <- updateVar (sig, bs) id v
                        return (sig, (b:bbs))

initVar :: Env -> Id -> Value -> IO Env
initVar (sigg, []) id val = return (sigg, (Map.insert id val Map.empty):[])
initVar (sigg, cx:cxt) id val = return (sigg, (Map.insert id val cx):cxt)

emptyEnv :: Env
emptyEnv = (Map.empty,[])

initEnv :: Env
initEnv =  emptyEnv

updateFun :: Env -> Def -> IO Env
updateFun (sig, cxt) (DFun t id a s) = case Map.lookup id sig of
                      Just a -> fail "Function already defined"
                      Nothing -> return (Map.insert id (DFun t id a s) sig, cxt)

lookupFun :: Env -> Id -> IO Def
lookupFun (sig , cntxts) idFunc  = case Map.lookup idFunc sig of
    Just typ -> return $ typ
    Nothing -> fail "Function not found" 

newBlock :: Env -> Env
newBlock (sig, cs) = (sig, (Map.empty:cs))

remBlock :: Env -> Env
remBlock (sig, c:cs) = (sig, cs)
