module Mymich where
import qualified Data.List as List
import qualified Data.Map  as Map
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import System.IO

import Absmich

data Val = I Integer | B Bool deriving (Show)
type Var = String
type Loc = Integer
type Store = Map.Map Loc Val
type VEnv = Map.Map Var Loc
type FEnv = Map.Map Var Fun
type Eval a = ReaderT (VEnv, FEnv) (ErrorT String (StateT Store IO)) a
data Fun = Fun { runFun :: ([Loc], [Fun]) -> (ErrorT String (StateT Store IO)) Val
               , params_var :: Integer -- number of variable parameters
               , params_fun :: Integer -- number of function parameters
               }





-- Alloc -----------------------------------------------------------------------

alloc :: Eval Loc

alloc = do
  store <- get
  let loc = (if Map.size store /= 0 then (fst $ Map.findMax store) else 0) + 1
  put $ Map.insert loc (I 0) store
  return loc

--------------------------------------------------------------------------------




-- Identifiers -----------------------------------------------------------------

transIdent :: Ident -> Eval Var

transIdent (Ident str) = return str

--------------------------------------------------------------------------------




-- Program ---------------------------------------------------------------------

transProgram :: Program -> IO ()

transProgram (Progr ext_decs) = do
  let ds = transExternal_declarations ext_decs
  (env, store) <- runStateT (runErrorT (runReaderT ds (Map.empty, Map.empty))) Map.empty  
  case env of  
    Right v  -> do
      let main = transExp (Efunk (Evar (Ident "main")))
      (result, store) <- runStateT (runErrorT (runReaderT main v)) store
      case result of
        Left err  -> hPutStrLn stderr err
        otherwise -> return ()
    Left err -> hPutStrLn stderr err
    
  return ()
  
--------------------------------------------------------------------------------


 

-- Declarations ----------------------------------------------------------------

transExternal_declarations :: [External_declaration] -> Eval (VEnv, FEnv)

transExternal_declarations [] = ask

transExternal_declarations (d:ds) = do
  newEnv <- transExternal_declaration d
  local (const newEnv) $ transExternal_declarations ds



transExternal_declaration :: External_declaration -> Eval (VEnv, FEnv)

transExternal_declaration (Afunc fun_def) = transFunction_def fun_def

transExternal_declaration (Global dec) = transDec dec



transFunction_def :: Function_def -> Eval (VEnv, FEnv)

transFunction_def (Func type_s (ParenDecl decl) c_stm) = 
  transFunction_def (Func type_s decl c_stm)

transFunction_def (Func type_s (FunDec id) c_stm) = 
  transFunDeclarator id ([], []) c_stm

transFunction_def (Func type_s (ParamFunDec id par_decs) c_stm) = do
  params <- transParameter_declarations par_decs
  transFunDeclarator id params c_stm

transFunction_def _ = throwError "Invalid function declaration"  



transDec :: Dec -> Eval (VEnv, FEnv)

transDec (Declarators type_s init_dec) = transInit_declarator init_dec


transInit_declarator :: Init_declarator -> Eval (VEnv, FEnv)  
transInit_declarator (OnlyDecl var) = transVarDeclarator var $ I 0
transInit_declarator (InitDecl var init) = do
  val <- transInitializer init
  transVarDeclarator var val
  
  

transVarDeclarator :: Declarator -> Val -> Eval (VEnv, FEnv)

transVarDeclarator (ParenDecl decl) val = transVarDeclarator decl val

transVarDeclarator (Name id) val = do
  (venv, fenv) <- ask
  loc <- alloc
  modify (\store -> Map.insert loc val store)
  var <- transIdent id
  if (Map.member var venv)
    then throwError "Redeclaration" 
    else return (Map.insert var loc venv, fenv)

transVarDeclarator _ _ = throwError "Invalid variable declaration"


-- [Var] - list of variable parameters
-- [Loc] - list of locations for variable parameters
-- Update environment, before function call
substituteVEnv :: [Var] -> [Loc] -> VEnv -> VEnv

substituteVEnv params locs venv = Map.union (Map.fromList $ zip params locs) venv

-- [Var] - list of function parameters
-- [Loc] - list of functions for function parameters
-- Update environment, before function call
substituteFEnv :: [Var] -> [Fun] -> FEnv -> FEnv

substituteFEnv params funs fenv = Map.union (Map.fromList $ zip params funs) fenv


transFunDeclarator :: Ident -> ([Var], [Var]) -> Compound_stm -> Eval (VEnv, FEnv)
transFunDeclarator id (var_params, fun_params) body = do
  (venv, fenv) <- ask
  fun_id <- transIdent id
  let fun_body = transCompound_stm body
  let var_length = toInteger $ length var_params
  let fun_length = toInteger $ length fun_params
  let fun =  Fun { runFun = \(locs, funs) -> runReaderT fun_body
                     (substituteVEnv var_params locs venv, Map.insert fun_id fun
                     (substituteFEnv fun_params funs fenv)),
                   params_var = var_length,
                   params_fun = fun_length
                 }
  return (venv, Map.insert fun_id fun fenv)



transParameter_declarations :: Parameter_declarations -> Eval ([Var], [Var])

transParameter_declarations (OnlyVar declarations) = do
  d <- transVar_declarations declarations
  return (d, [])

transParameter_declarations (OnlyFun declarations) = do
  d <- transFun_declarations declarations
  return ([], d)

transParameter_declarations (VarAndFun decs_var decs_fun) = do
  (d_var, _) <- transParameter_declarations (OnlyVar decs_var)
  (_, d_fun) <- transParameter_declarations (OnlyFun decs_fun)
  return (d_var, d_fun)


transVar_declarations :: Declarations -> Eval [Var]

transVar_declarations (ParDec declaration) = do
  d <- transVar_declaration declaration
  return (d:[])

transVar_declarations (MoreParDec dec decs) = do
  ds <- transVar_declarations decs
  d <-  transVar_declaration dec
  return (d:ds)

transVar_declaration :: Declaration -> Eval Var
  
transVar_declaration (TypeAndParam type_s (Name id)) = transIdent id

transVar_declaration _ = throwError "Invalid variable parameter"


transFun_declarations :: Declarations -> Eval [Var]

transFun_declarations (ParDec declaration) = do
  d <- transFun_declaration declaration
  return (d:[])

transFun_declarations (MoreParDec dec decs) = do
  ds <- transFun_declarations decs
  d <-  transFun_declaration dec
  return (d:ds)

transFun_declaration :: Declaration -> Eval Var
  
transFun_declaration (TypeAndParam type_s (FunDec id)) = transIdent id

transFun_declaration (TypeAndParam type_s (ParamFunDec id (OnlyTypes ts))) =
  transIdent id

transFun_declaration _ = throwError "Invalid functional parameter"


    
transInitializer :: Initializer -> Eval Val

transInitializer (InitExpr exp) = transExp exp

--------------------------------------------------------------------------------




-- Statements ------------------------------------------------------------------

transStms :: Stms -> Eval Val

transStms (StmsRetOnly return_stm) = transReturn_stm return_stm

transStms (StmsRet stms_only return_stm) = do
  transStmsNoRet stms_only
  transReturn_stm return_stm
  
transStms (StmsNoRet stms_only) = do
  transStmsNoRet stms_only
  return $ I 0



transStmsNoRet :: [Stm] -> Eval Val

transStmsNoRet [] = return $ I 0

transStmsNoRet (stm:stms) = do
  transStm stm
  transStmsNoRet stms



transStm :: Stm -> Eval Val

transStm stm = do
  case stm of
    CompS compound_stm    -> transCompound_stm compound_stm
    ExprS expression_stm  -> transExpression_stm expression_stm
    SelS selection_stm    -> transSelection_stm selection_stm
    IterS iter_stm        -> transIter_stm iter_stm
    PrintS print_stm      -> transPrint_stm print_stm



transCompound_stm :: Compound_stm -> Eval Val

transCompound_stm ScompOne = return $ I 0

transCompound_stm (ScompTwo stms) = transStms stms

transCompound_stm (ScompThree ext_decs) = do
    transExternal_declarations ext_decs
    return $ I 0
    
transCompound_stm (ScompFour external_declarations stms) = do
    env <- transExternal_declarations external_declarations
    local (const env) $ transStms stms



transExpression_stm :: Expression_stm -> Eval Val

transExpression_stm SexprOne = return $ I 0

transExpression_stm (SexprTwo exp) = transExp exp
    


transSelection_stm :: Selection_stm -> Eval Val

transSelection_stm (SselIf exp compound_stm) = do
    b <- transExp exp
    case b of
      B False -> return $ I 0
      B True  -> transCompound_stm compound_stm
        
transSelection_stm (SselIfElse exp compound_stm1 compound_stm2) = do
    b <- transExp exp
    case b of
      B False -> transCompound_stm compound_stm2
      B True  -> transCompound_stm compound_stm1



transIter_stm :: Iter_stm -> Eval Val

transIter_stm w@(SiterWhile exp stm) = do
    b <- transExp exp
    case b of
      B False -> return $ I 0
      B True  -> do
        transStm stm
        transIter_stm w
        
transIter_stm (SiterFor exp_stm1@(SexprTwo (Eassign var op e))
               exp_stm2 exp stm) = do
                          
  transExpression_stm exp_stm1
  transFor_stm exp_stm2 exp stm



transFor_stm :: Expression_stm -> Exp -> Stm -> Eval Val

transFor_stm exp_stm exp stm = do
  b <- transExpression_stm exp_stm
  case b of
      B False -> return $ I 0
      B True -> do
        transStm stm
        transExp exp
        transFor_stm exp_stm exp stm



transPrint_stm :: Print_stm -> Eval Val

transPrint_stm (Sprint exp) = do
  val <- transExp exp
  liftIO $ putStrLn $ show val
  return $ I 0


  
transReturn_stm :: Return_stm -> Eval Val

transReturn_stm (Sret exp) = transExp exp

--------------------------------------------------------------------------------




-- Expressions -----------------------------------------------------------------

-- Gets variable locations and functions from function call
-- var_length is the last variable paramter before following function parameters
getParams :: [Exp] -> Integer -> Eval ([Loc], [Fun])

getParams exps var_length = do
  let (vars, funs) = List.genericSplitAt var_length exps
  locs <- getLocs vars
  funs <- getFuns funs
  return (locs, funs)


getFuns :: [Exp] -> Eval [Fun]

getFuns [] = return []

getFuns (e:es) = do
  (venv, fenv) <- ask
  fun <- getFun e
  funs <- getFuns es
  return (fun:funs)

getFun :: Exp -> Eval Fun

getFun (Evar id) = do
  (venv, fenv) <- ask
  fun_id <- transIdent id
  case Map.lookup fun_id fenv of
    Nothing -> throwError "Unbound function variable"
    Just f  -> return f

getFun (Elambda params@(OnlyVar decs) stm) = do
  (venv, fenv) <- ask
  (var_params, _) <- transParameter_declarations params
  let fun_body = transStm stm
  let var_length = toInteger $ length var_params
  let fun_length = 0
  let fun =  Fun { runFun = \(locs, funs) -> runReaderT fun_body
                     (substituteVEnv var_params locs venv, fenv),
                   params_var = var_length,
                   params_fun = fun_length
                 }
  return $ fun

getFun _ = throwError "Not a function"


getLocs :: [Exp] -> Eval [Loc]

getLocs [] = return []

getLocs ((Evar id):es) = do
  (venv, fenv) <- ask
  var <- transIdent id
  case Map.lookup var venv of
    Nothing -> throwError "Unbound variable1"
    Just loc  -> do
      locs <- getLocs es
      return (loc:locs)


  
transExp :: Exp -> Eval Val

transExp x = case x of

  Eassign exp1@(Evar id) op exp2  -> do
    (venv, fenv) <- ask
    var <- transIdent id
    val <- case op of
      Assign    -> transExp exp2
      AssignAdd -> transExp (Eplus   exp1 exp2)
      AssignDiv -> transExp (Ediv    exp1 exp2) 
      AssignMul -> transExp (Etimes  exp1 exp2)
      AssignSub -> transExp (Eminus  exp1 exp2)
    case Map.lookup var venv of
      Nothing  -> throwError "Unbound variable"
      Just loc -> do
        modify (\store -> Map.insert loc val store)
        return val
  Eassign _ _ _ -> throwError "Invalid assignment"
    
  Elor exp1 exp2  -> do
    B b1 <- transExp exp1
    B b2 <- transExp exp2
    return $ B $ b1 || b2

  Eland exp1 exp2  -> do
    B b1 <- transExp exp1
    B b2 <- transExp exp2
    return $ B $ b1 && b2

  Eeq exp1 exp2  -> do
    B b1 <- transExp (Ele exp1 exp2)
    B b2 <- transExp (Ege exp1 exp2)
    return $ B $ b1 && b2

  Eneq exp1 exp2  -> do
    B b <- transExp (Eeq exp1 exp2)
    return $ B $ not b 

  Elthen exp1 exp2  -> do
    B b1 <- transExp (Ele exp1 exp2)
    B b2 <- transExp (Eneq exp1 exp2)
    return $ B $ b1 && b2

  Egrthen exp1 exp2  -> do
    B b1 <- transExp (Ege exp1 exp2)
    B b2 <- transExp (Eneq exp1 exp2)
    return $ B $ b1 && b2

  Ele exp1 exp2  -> do
    I i1 <- transExp exp1
    I i2 <- transExp exp2
    return $ B $ i1 <= i2

  Ege exp1 exp2  -> do
    I i1 <- transExp exp1
    I i2 <- transExp exp2
    return $ B $ i1 >= i2

  Eplus exp1 exp2  -> do
    I e1 <- transExp exp1
    I e2 <- transExp exp2
    return $ I (e1 + e2)

  Eminus exp1 exp2  -> do
    I e1 <- transExp exp1
    I e2 <- transExp exp2
    return $ I $ e1 - e2  

  Etimes exp1 exp2  -> do
    I e1 <- transExp exp1
    I e2 <- transExp exp2
    return $ I $ e1 * e2  

  Ediv exp1 exp2  -> do
    I e1 <- transExp exp1
    I e2 <- transExp exp2
    case e2 of
      0 -> throwError "divide by 0"
      otherwise -> return $ I $ e1 `div` e2 

  Epreinc (Evar id)  -> do
    (venv, fenv) <- ask
    store <- get
    var <- transIdent id
    case Map.lookup var venv of
      Nothing  -> throwError "Unbound variable"
      Just loc -> do
        let I val = (store Map.! loc)
        modify (\store -> Map.insert loc (I (val + 1)) store)
        return $ I $ val + 1
  Epreinc _ -> throwError "Not variable"

  Epredec (Evar id)  -> do
    (venv, fenv) <- ask
    store <- get
    var <- transIdent id
    case Map.lookup var venv of
      Nothing  -> throwError "Unbound variable"
      Just loc -> do
        let I val = (store Map.! loc)
        modify (\store -> Map.insert loc (I (val - 1)) store)
        return $ I $ val - 1
  Epredec _ -> throwError "Not variable"  

  Epostinc (Evar id)  -> do
    (venv, fenv) <- ask
    store <- get
    var <- transIdent id
    case Map.lookup var venv of
      Nothing  -> throwError "Unbound variable"
      Just loc -> do
        let I val = (store Map.! loc)
        modify (\store -> Map.insert loc (I (val + 1)) store)
        return $ I $ val
  Epostinc _ -> throwError "Not variable"  

  Epostdec (Evar id)  -> do
    (venv, fenv) <- ask
    store <- get
    var <- transIdent id
    case Map.lookup var venv of
      Nothing  -> throwError "Unbound variable"
      Just loc -> do
        let I val = (store Map.! loc)
        modify (\store -> Map.insert loc (I (val - 1)) store)
        return $ I $ val
  Epostdec _ -> throwError "Not variable"

  Elambda parameter_declarations stm  ->
    throwError "Invalid anonymous function usage"

  Efunk (Evar id) -> do
    (venv, fenv) <- ask
    store <- get
    ident <- transIdent id
    case Map.lookup ident fenv of
      Nothing  -> throwError "Unbound function variable"
      Just fun -> do
        (env, newStore) <- liftIO $ runStateT (runErrorT (runFun fun ([],[]))) store
        case env of
          Left err -> throwError err
          Right v  -> do
            modify (const newStore)
            return v
  Efunk _ -> throwError "Invalid function call"
  
  Efunkpar (Evar id) exps  -> do
    (venv, fenv) <- ask
    store <- get
    ident <- transIdent id
    case Map.lookup ident fenv of
      Nothing  -> throwError "Unbound function variable1"
      Just fun -> do
        fun_params <- getParams exps (params_var fun)
        (val, newStore) <- liftIO $ runStateT (runErrorT (runFun fun fun_params)) store
        case val of
          Left err -> throwError err
          Right v  -> do
            modify (const newStore)
            return v
  Efunkpar _ _ -> throwError "Invalid function call"       

  Evar id  -> do
    (venv, fenv) <- ask
    var <- transIdent id
    store <- get
    case Map.lookup var venv of
      Nothing  -> throwError "Unbound variable"
      Just loc -> do
        return $ store Map.! loc
    
  Econst exp  -> transConstant exp



transConstant :: Constant -> Eval Val

transConstant x = case x of
  Eint n  -> return $ I n
  Etrue  -> return $ B True
  Efalse  -> return $ B False

--------------------------------------------------------------------------------
 