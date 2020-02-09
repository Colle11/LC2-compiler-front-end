{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module TypeChecker (
  typeCheckProgram,
  printErrors,
  LcErrorType
  ) where

import Annotations
import Types
import SourceLocation hiding (getLoc)
import AST
import Compiler.SymbolTable
import Compiler.Errors

import Prelude hiding (lookup,log)
import Data.Monoid
import Data.Functor
import Data.List
import Data.Maybe
import Data.Bool (bool)
import Data.Char (ord)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Hash
import Control.Monad.Except
import Control.Monad.State
import Data.Hashable
import Data.Bits
import Lens.Micro.Platform


type ErrorLog = [LcErrorType] -> [LcErrorType]

type FuncInfo = (TypeSpec,         -- current function return type
                [FormalParameter], -- current function by res formal parameters
                Ident)             -- function name

-- State of the TypeChecker Monad
data TCState = TCState {
  _env    :: Env,          -- environment
  _log    :: ErrorLog,     -- log of errors
  _funcInfo :: [FuncInfo], -- stack of FuncInfo
  _loopid :: Int           -- a value greater than zero means we are inside a loop
  }

-- TemplateHaskell: generates the lenses (getters+setters) for TCState
makeLenses ''TCState

-- Gets the head of [FuncInfo]
currentFunc :: Traversal' [FuncInfo] FuncInfo
currentFunc = ix 0

-- From TCState, retrieves the Funcinfo of the current function
currentFuncInfo :: Traversal' TCState FuncInfo
currentFuncInfo = funcInfo . currentFunc

-- Initial state of TypeChecker Monad
initialState :: TCState
initialState = TCState [initialEnv] id [] 0


-- THE CHOSEN MONAD STACK:
-- ExceptT e (StateT s m) a
-- runExceptT -> (StateT s m (Either e a))
-- runStateT -> m ((Either e a), s)

newtype TypeCheckerT m a = TypeCheckerT {
  runTCT :: ExceptT LcErrorType (StateT TCState m) a
  }
  deriving (
    Functor,
    Applicative,
    Monad,
    MonadState TCState,
    MonadError LcErrorType
    )

-- The monad inside which we perform the type-checking
type TypeChecker a = TypeCheckerT IO a

-- Executes the monadic action TypeCheckerT from the initial state TCState
runTypeCheckerT :: TCState -> TypeCheckerT m a -> m (Either LcErrorType a, TCState)
runTypeCheckerT s t = flip runStateT s . runExceptT . runTCT $ t

-- Entry point of the type-checker
typeCheckProgram :: Program -> IO (Either [LcErrorType] Program)
typeCheckProgram l = do
  (res, state) <- runTypeCheckerT initialState (checkProgram l)
  case res of
    Left e -> return $ Left [e]
    Right a -> do
      case _log state $ [] of
        [] -> return $ Right a
        es -> return $ Left es

-- Lookup Ident in the Variables Environment
lookupVar :: Ident -> [LocalEnv] -> Maybe (TypeSpec, SrcSpan, Modality, [Width], IsInit)
lookupVar k (x:xs) = case Hash.lookup k . _vars $ x of
                       Nothing -> lookupVar k xs
                       v -> v
lookupVar k [] = Nothing

-- Lookup Ident in the Functions Environment
lookupFun :: Ident -> [LocalEnv] -> Maybe (TypeSpec, SrcSpan, [Addr])
lookupFun k (x:xs) = case Hash.lookup k . _funs $ x of
                       Nothing -> lookupFun k xs
                       v -> v
lookupFun k [] = Nothing

-- Inserts Ident in the Variables environment
insertVar :: Ident -> (TypeSpec, SrcSpan, Modality, [Width], IsInit) -> TypeChecker ()
insertVar k v = do
  modify addVar
  where
    addVar :: TCState -> TCState
    addVar s = s {_env=new_env}
      where
        outer_env = tail . _env $ s
        local_env = head . _env $ s
        updated_env = local_env {_vars=Hash.insert k v . _vars $ local_env}
        new_env = updated_env:outer_env

-- Lookup Ident only on Variables with modality Result
lookupResParam :: Ident -> [LocalEnv] -> Maybe (TypeSpec, SrcSpan, Modality, [Width], IsInit)
lookupResParam id (x:xs) = case Hash.lookup id . _vars $ x of
                             v@(Just (_, _, Result, _, _)) -> v
                             _ -> lookupResParam id xs
lookupResParam id [] = Nothing

-- Inserts Ident in the Functions environment
insertFun :: Ident -> (TypeSpec, SrcSpan, [Addr]) -> TypeChecker ()
insertFun k v = do
  modify addFun
  where
    addFun :: TCState -> TCState
    addFun s = s {_env=new_env}
      where
        outer_env = tail . _env $ s
        local_env = head . _env $ s
        updated_env = local_env {_funs=Hash.insert k v . _funs $ local_env}
        new_env = updated_env:outer_env

-- Creates the local environment of a Function and updates the current FuncInfo
newFunLocalEnv :: TypeSpec -> [AnFormalParameter] -> Ident -> TypeChecker ()
newFunLocalEnv t fps fname = do
  modify addEnv
    where
      isByRes (FormalParameter Result (ArrayType _ _) _) = False
      isByRes (FormalParameter Result _ _) = True
      isByRes _ = False
      byResParams = filter isByRes $ map unAnn fps
      addEnv :: TCState -> TCState
      addEnv s = s{_env=emptyEnv:_env s, _funcInfo=(t, byResParams, fname):_funcInfo s}

-- Deletes the most recent local environment and restores the current FuncInfo
exitFunLocalEnv :: TypeChecker ()
exitFunLocalEnv = do
  modify removeEnv
    where
      removeEnv :: TCState -> TCState
      removeEnv s = s{_env= tail . _env $ s, _funcInfo= tail . _funcInfo $ s}

-- Creates a new local environment
newBlockLocalEnv :: TypeChecker ()
newBlockLocalEnv = do
  modify addEnv
    where
      addEnv :: TCState -> TCState
      addEnv s = s{_env=emptyEnv:_env s}

-- Deletes the most recent local environment
exitBlockLocalEnv :: TypeChecker ()
exitBlockLocalEnv = do
  modify removeEnv
    where
      removeEnv :: TCState -> TCState
      removeEnv s = s{_env= tail . _env $ s}

-- Creates a new local environment and updates loopid
newLoopLocalEnv :: TypeChecker ()
newLoopLocalEnv = do
  newBlockLocalEnv
  incLoopId

-- Deletes the most recent local environment and restores loopid
exitLoopLocalEnv :: TypeChecker ()
exitLoopLocalEnv = do
  decLoopId
  exitBlockLocalEnv

-- Increases the loopid
incLoopId :: TypeChecker ()
incLoopId = do
  modify addId
    where
      addId :: TCState -> TCState
      addId s = s{_loopid=_loopid s + 1}

-- Decreases the loopid
decLoopId :: TypeChecker ()
decLoopId = do
  modify decId
    where
      decId :: TCState -> TCState
      decId s = s{_loopid=_loopid s - 1}

-- Adds the error to the Log of Errors
logError :: LcErrorType -> TypeChecker ()
logError lcError = do
  log %= flip (.) (lcError :)

-- Logs the error and returns the default value a
logErrorDefault :: a -> LcErrorType -> TypeChecker a
logErrorDefault a lcError = do
  logError lcError
  return a

-- Infers the type of a RExpr
inferTypeRExpr :: AnRExpr -> TypeChecker (AnRExpr)
inferTypeRExpr anRExpr =
  let old_a = getAnn anRExpr
      rexpr = unAnn anRExpr in
  case rexpr of
    InfixOp op rexpr1 rexpr2 -> do
      node1 <- inferTypeRExpr rexpr1
      node2 <- inferTypeRExpr rexpr2
      let t1 = getType node1
          t2 = getType node2
          constness = (getConst node1) <> (getConst node2)
      if isArrayLit node1 || isArrayLit node2
      then throwError . L (getLocInfix op) . StaticError $ UnexpectedArrayLit
      else
        case op of
        (ArithOp o) -> do
          t <- inferTypeArithOp o t1 t2
          node1 <- possiblyCoerce node1 t
          node2 <- possiblyCoerce node2 t
          return . A old_a{_type=t, _const=constness, _valcat=RValue} $ InfixOp op node1 node2
        (BoolOp o) -> do
          t <- inferTypeBoolOp o t1 t2
          node1 <- possiblyCoerce node1 t
          node2 <- possiblyCoerce node2 t
          return . A old_a{_type=t, _const=constness, _valcat=RValue} $ InfixOp op node1 node2
        (RelOp o) -> do
          t <- inferTypeRelOp o t1 t2
          let b = BasicType BasicTypeBool
          node1 <- possiblyCoerce node1 t
          node2 <- possiblyCoerce node2 t
          return . A old_a{_type=b, _const=constness, _valcat=RValue} $ InfixOp op node1 node2
        (BitOp o) -> do
          t <- inferTypeBitOp o t1 t2
          node1 <- possiblyCoerce node1 t
          node2 <- possiblyCoerce node2 t
          return . A old_a{_type=t, _const=constness, _valcat=RValue} $ InfixOp op node1 node2
    UnaryOp o rexpr -> do
      node <- inferTypeRExpr rexpr
      let tn = getType node
          constness = getConst node
      if isArrayLit node
      then throwError . L (getLoc o) . StaticError $ UnexpectedArrayLit
      else do
        t <- inferTypeUnaryOp o tn
        return . A old_a{_type=t, _const=constness, _valcat=RValue} $ UnaryOp o node
    Const t (Array l) -> do
      l' <- mapM inferTypeRExpr l
      return . A old_a{_type=t, _valcat=RValue} . Const t . Array $ l'
    Const t c -> return $ A old_a{_type=t, _const=ConstExpr, _valcat=RValue} rexpr
    RBrackets r -> do
      node <- inferTypeRExpr r
      let t = getType node
          constness = getConst node
          vc = getVC node
      if isArrayLit node
      then throwError . L (getLoc node) . StaticError $ UnexpectedArrayLit
      else do
        return $ A old_a{_type = t, _const=constness, _valcat=vc} (RBrackets node)
    LExpr lexpr -> do
      node <- inferTypeLExpr lexpr
      checkLExprInit node
      let t = getType node
          constness = getConst node
          vc = getVC node
      return $ A old_a{_type=t,_const=constness, _valcat=vc} $ LExpr node
    Ref lexpr -> do
      node <- inferTypeLExpr lexpr
      checkLExprInit node
      checkLExprConst node
      let t  = getType node
          vc = getVC node
      case vc of
        LValue -> return $ A old_a{_type=RefType t, _const=VarExpr, _valcat=RValue} $ Ref node
        RValue -> throwError . L (getLoc old_a) . StaticError $ InvalidRefVC
    FCall anIdent l -> do
      env <- use env
      case flip lookupFun env . unAnn $ anIdent of
        Nothing -> throwError . L (getLoc anIdent) . StaticError . UndeclaredFunIdent . unAnn $ anIdent
        Just ((FunType mt r), loc, _) -> do
          l' <- zipWithM checkFunctionArg mt l
          let ll  = length l
              lmt = length mt
          case ll == lmt of
            True  -> return $ A old_a {_type=r, _const=VarExpr, _valcat=RValue} $ FCall anIdent l'
            False -> throwError . L (getLoc old_a) . StaticError $ WrongArgNum ll lmt
    Coercion _ _ -> return anRExpr
    TernaryOp gRexpr tRexpr eRexpr -> do
      gNode <- inferTypeRExpr gRexpr
      tNode <- inferTypeRExpr tRexpr
      eNode <- inferTypeRExpr eRexpr
      let gT = getType gNode
          tT = getType tNode
          eT = getType eNode
      when (isArrayLit gNode) $
        throwError . L (getLoc gNode) . StaticError $ UnexpectedArrayLit
      when (isArrayLit tNode) $
        throwError . L (getLoc tNode) . StaticError $ UnexpectedArrayLit
      when (isArrayLit eNode) $
        throwError . L (getLoc eNode) . StaticError $ UnexpectedArrayLit
      case gT of
        BasicType BasicTypeBool -> case partialOrdEq tT eT && not (isUnit tT) of
          False -> throwError . L (getLoc old_a) . StaticError $ InvalidTernaryOps tT eT
          True -> do
            let tVC = getVC tNode
                eVC = getVC eNode
                ternaryVC = bool RValue LValue (isLValue tVC && isLValue eVC)
            return . A old_a{_type=tT, _const=VarExpr, _valcat=ternaryVC} $ TernaryOp gNode tNode eNode
        _ -> throwError . L (getLoc gNode) . StaticError $ (InvalidGuard gT)

-- Checks if a RExpr is an Array literal
isArrayLit :: AnRExpr -> Bool
isArrayLit anRExpr = isArray && isLit
                     where
                       isArray = isArrayType $ getType anRExpr
                       isLit = isLiteral $ unAnn anRExpr

-- Gets location of an infix operator
getLocInfix :: InfixOp -> SrcSpan
getLocInfix (ArithOp anArithOp) = getLoc anArithOp
getLocInfix (RelOp anRelOp) = getLoc anRelOp
getLocInfix (BoolOp anBoolOp) = getLoc anBoolOp
getLocInfix (BitOp anBitOp) = getLoc anBitOp

-- If necessary, coerces a RExpr to the given type
possiblyCoerce :: AnRExpr -> TypeSpec -> TypeChecker AnRExpr
possiblyCoerce r t | lt (getType r) t = return $ A (flip setVC RValue . flip setType t . getAnn $ r) (Coercion t r)
                   | otherwise = return r

-- The function inferType*Op returns the type (inferred from the operands) on which the operation is done.
-- In other words, it infers the domain of the operator and not the codomain.

inferTypeArithOp :: AnArithOp -> TypeSpec -> TypeSpec -> TypeChecker TypeSpec
inferTypeArithOp o l r = let res = l \/ r in
  case unAnn o of
    Add -> do
      case res of
        BasicType BasicTypeInt -> return res
        BasicType BasicTypeFloat -> return res
        BasicType BasicTypeChar -> return $ BasicType BasicTypeInt
        UnTypeable -> throwError . L (getLoc o) . StaticError $ IncompatibleTypes l r
        _ -> throwError . L (getLoc o) . StaticError $ InvalidInfixOp (ArithOp o) l r
    Sub -> do
      case res of
        BasicType BasicTypeInt -> return res
        BasicType BasicTypeFloat -> return res
        BasicType BasicTypeChar -> return $ BasicType BasicTypeInt
        UnTypeable -> throwError . L (getLoc o) . StaticError $ IncompatibleTypes l r
        _ -> throwError . L (getLoc o) . StaticError $ InvalidInfixOp (ArithOp o) l r
    Mul -> do
      case res of
        BasicType BasicTypeInt -> return res
        BasicType BasicTypeFloat -> return res
        BasicType BasicTypeChar -> return $ BasicType BasicTypeInt
        UnTypeable -> throwError . L (getLoc o) . StaticError $ IncompatibleTypes l r
        _ -> throwError . L (getLoc o) . StaticError $ InvalidInfixOp (ArithOp o) l r
    Div -> do
      case res of
        BasicType BasicTypeInt -> return res
        BasicType BasicTypeFloat -> return res
        BasicType BasicTypeChar -> return $ BasicType BasicTypeInt
        UnTypeable -> throwError . L (getLoc o) . StaticError $ IncompatibleTypes l r
        _ -> throwError . L (getLoc o) . StaticError $ InvalidInfixOp (ArithOp o) l r
    Mod -> do
      case res of
        BasicType BasicTypeInt -> return res
        BasicType BasicTypeChar -> return $ BasicType BasicTypeInt
        UnTypeable -> throwError . L (getLoc o) . StaticError $ IncompatibleTypes l r
        _ -> throwError . L (getLoc o) . StaticError $ InvalidInfixOp (ArithOp o) l r
    Exp -> do
      case res of
        BasicType BasicTypeInt -> return $ BasicType BasicTypeFloat
        BasicType BasicTypeFloat -> return res
        BasicType BasicTypeChar -> return $ BasicType BasicTypeFloat
        UnTypeable -> throwError . L (getLoc o) . StaticError $ IncompatibleTypes l r
        _ -> throwError . L (getLoc o) . StaticError $ InvalidInfixOp (ArithOp o) l r

inferTypeBoolOp :: AnBoolOp -> TypeSpec -> TypeSpec -> TypeChecker TypeSpec
inferTypeBoolOp o l r = let res = l \/ r in
  case res of
    BasicType BasicTypeBool -> return res
    UnTypeable -> throwError . L (getLoc o) . StaticError $ IncompatibleTypes l r
    _ -> throwError . L (getLoc o) . StaticError $ InvalidInfixOp (BoolOp o) l r

inferTypeRelOp :: AnRelOp -> TypeSpec -> TypeSpec -> TypeChecker TypeSpec
inferTypeRelOp o l r = do
  let res = l \/ r
  case unAnn o of
    Eq -> case res of
      BasicType BasicTypeInt -> return res
      BasicType BasicTypeFloat -> return res
      BasicType BasicTypeBool -> return res
      BasicType BasicTypeChar -> return res
      BasicType BasicTypeString -> return res
      UnTypeable -> throwError . L (getLoc o) . StaticError $ IncompatibleTypes l r
      _ -> throwError . L (getLoc o) . StaticError $ InvalidInfixOp (RelOp o) l r
    Neq -> case res of
      BasicType BasicTypeInt -> return res
      BasicType BasicTypeFloat -> return res
      BasicType BasicTypeBool -> return res
      BasicType BasicTypeChar -> return res
      BasicType BasicTypeString -> return res
      UnTypeable -> throwError . L (getLoc o) . StaticError $ IncompatibleTypes l r
      _ -> throwError . L (getLoc o) . StaticError $ InvalidInfixOp (RelOp o) l r
    _ -> case res of
      BasicType BasicTypeInt -> return res
      BasicType BasicTypeFloat -> return res
      BasicType BasicTypeChar -> return $ BasicType BasicTypeInt
      UnTypeable -> throwError . L (getLoc o) . StaticError $ IncompatibleTypes l r
      _ -> throwError . L (getLoc o) . StaticError $ InvalidInfixOp (RelOp o) l r

inferTypeBitOp :: AnBitOp -> TypeSpec -> TypeSpec -> TypeChecker TypeSpec
inferTypeBitOp o l r = do
  let res = l \/ r
  case res of
    BasicType BasicTypeInt -> return res
    BasicType BasicTypeChar -> return $ BasicType BasicTypeInt
    UnTypeable -> throwError . L (getLoc o) . StaticError $ IncompatibleTypes l r
    _ -> throwError . L (getLoc o) . StaticError $ InvalidInfixOp (BitOp o) l r

inferTypeUnaryOp :: AnUnaryOp -> TypeSpec -> TypeChecker TypeSpec
inferTypeUnaryOp o e =
  case unAnn o of
   Not -> do
    case e of
     BasicType BasicTypeBool -> return e
     _ -> throwError . L (getLoc o) . StaticError $ InvalidUnaryOp (unAnn o) e
   Neg -> do
    case e of
     BasicType BasicTypeInt -> return e
     BasicType BasicTypeChar -> return $ BasicType BasicTypeInt
     BasicType BasicTypeFloat -> return e
     _ -> throwError . L (getLoc o) . StaticError $ InvalidUnaryOp (unAnn o) e
   Plus -> do
    case e of
     BasicType BasicTypeInt -> return e
     BasicType BasicTypeChar -> return $ BasicType BasicTypeInt
     BasicType BasicTypeFloat -> return e
     _ -> throwError . L (getLoc o) . StaticError $ InvalidUnaryOp (unAnn o) e
   BComp -> do
    case e of
     BasicType BasicTypeInt -> return e
     BasicType BasicTypeChar -> return $ BasicType BasicTypeInt
     _ -> throwError . L (getLoc o) . StaticError $ InvalidUnaryOp (unAnn o) e

-- Infers the type of a LExpr
inferTypeLExpr :: AnLExpr -> TypeChecker AnLExpr
inferTypeLExpr anLExpr = do
  let old_a = getAnn anLExpr
      lexpr = unAnn anLExpr
  case lexpr of
    LBrackets l -> do
      node <- inferTypeLExpr l
      let t = getType node
          constness = getConst node
          vc = getVC node
      return $ A old_a{_type = t, _const=constness, _valcat=vc} $ LBrackets node
    Deref r -> do
      node <- inferTypeRExpr r
      let t = getType node
      case t of
        (RefType tr) -> return . A old_a{_type=tr, _valcat=LValue} . Deref $ node
        _ -> throwError . L (getLoc old_a) . StaticError $ InvalidDerefOp t
    VarIdent (A e id) -> do
      theEnv <- use env
      case flip lookupVar theEnv $ id of
        Nothing -> throwError . L (getLoc e) . StaticError $ UndeclaredVarIdent id
        Just v@(t, _, m, _, _) -> case m of
            Result -> do
              (_, byResParams, fname) <- head <$> use funcInfo
              when (not . elem id $ map getFormalParameterId byResParams) $ do
                logError . L (getLoc e) . StaticError $ CaptureOfResParam id fname
                insertVar id (t, getLoc e, Value, [], True) -- fake variable, just to silence similar errors
              return $ A old_a{_type=t, _valcat=LValue} lexpr
            _ -> return $ A old_a{_type=t, _valcat=LValue} lexpr
    ArrayElem l r -> do
      nodel <- inferTypeLExpr l
      noder <- inferTypeRExpr r
      let tl = getType nodel
          tr = getType noder
      case tl of
        ArrayType _ t -> case tr of
          BasicType BasicTypeInt ->
             return $ A old_a {_type=t, _valcat=LValue} $ ArrayElem nodel noder
          _ -> throwError . L (getLoc r) . StaticError $ UnexpectedType tr (BasicType BasicTypeInt)
        _ -> throwError . L (getLoc l) . StaticError $ ExpectedArrayType tl
    PrePostIncDecr pp id l -> do
      node <- inferTypeLExpr l
      let t = getType node
          vc = getVC node
      if isInt t || isFloat t
      then if isLValue vc
           then return $ A old_a {_type=t, _valcat=RValue} $ PrePostIncDecr pp id node
           else throwError . L (getLoc l) . StaticError $ InvalidIncDecrVC id
      else throwError . L (getLoc l) . StaticError $ InvalidIncDecrOp id t

-- Checks if a variable has been initialized
-- Call only after inferTypeLExpr
checkLExprInit :: AnLExpr -> TypeChecker ()
checkLExprInit anLExpr = case unAnn anLExpr of
  VarIdent anIdent -> do
    env <- use env
    let (_, _, _, _, isInit) = fromJust . flip lookupVar env . unAnn $ anIdent
    when (not isInit) $ do
      logError . L (getLoc anIdent) . StaticError . UnInitVarIdent . unAnn $ anIdent
  LBrackets l -> checkLExprInit l
  Deref r -> return ()
  ArrayElem l _ -> return ()
  PrePostIncDecr _ _ l -> checkLExprInit l

-- Marks a variable as initialized
-- Call only after inferTypeLExpr
markLExprAsInit :: AnLExpr -> TypeChecker ()
markLExprAsInit anLExpr = case unAnn anLExpr of
  VarIdent (A _ id) -> markIdentAsInit id
  LBrackets l -> markLExprAsInit l
  Deref r -> return ()
  ArrayElem l _ -> return ()
  PrePostIncDecr _ _ l -> return ()

-- Marks an Ident as initialized
-- Call only after inferTypeLExpr
markIdentAsInit :: Ident -> TypeChecker ()
markIdentAsInit id = do
    theEnv <- use env
    let tuple = fromJust . flip lookupVar theEnv $ id
        isInit = view _5 tuple
    when (not isInit) $ do
      insertVar id (set _5 True tuple)

-- Checks if a variable is declared with Constant modality
-- Call where a variable with Constant modality is not allowed
checkLExprConst :: AnLExpr -> TypeChecker ()
checkLExprConst anLExpr = case unAnn anLExpr of
  VarIdent anIdent -> do
    env <- use env
    let  isConst Constant = True
         isConst _ = False
         (_, _, m, _, _) = fromJust . flip lookupVar env . unAnn $ anIdent
    when (isConst m) $ do
      logError . L (getLoc anIdent) . StaticError . VarIdentNotModif . unAnn $ anIdent
  LBrackets l -> checkLExprConst l
  Deref r -> return ()
  ArrayElem l _ -> checkLExprConst l
  PrePostIncDecr _ _ l -> checkLExprConst l

-- Checks compatibility between formal parameter and actual parameter
checkFunctionArg :: ModTypeSpec -> AnRExpr -> TypeChecker AnRExpr
checkFunctionArg (ModTypeSpec m t) rexpr = do
  node <- inferTypeRExpr rexpr `catchError` logErrorDefault rexpr
  let tn  = getType node -- type of actual parameter
      vc  = getVC node
      checkLExprConstArg :: AnRExpr -> TypeChecker ()
      checkLExprConstArg (A _ (LExpr l)) = checkLExprConst l
      checkLExprConstArg (A _ (RBrackets r)) = checkLExprConstArg r
      checkLExprConstArg (A _ (TernaryOp _ t e)) = checkLExprConstArg t >> checkLExprConstArg e
  -- t: type of formal parameter, tn: type of actual parameter
  case m of
    Value -> case joinLeq tn t && areArrayDimEqual tn t of
      False -> logErrorDefault node . L (getLoc node) . StaticError $ UnexpectedArgType tn t
      True  -> evaluateRExpr <$> possiblyCoerce node t
    Constant -> case vc of
      RValue -> logErrorDefault node . L (getLoc node) . StaticError $ UnexpectedArgVC
      LValue -> case partialOrdEq tn t && areArrayDimEqual tn t of
        False -> logErrorDefault node . L (getLoc node) . StaticError $ UnexpectedArgType tn t
        True  -> return node
    _ -> case vc of
      RValue -> logErrorDefault node . L (getLoc node) . StaticError $ UnexpectedArgVC
      LValue -> do
        checkLExprConstArg node
        case partialOrdEq tn t  && areArrayDimEqual tn t of
          False -> logErrorDefault node . L (getLoc node) . StaticError $ UnexpectedArgType tn t
          True  -> return node

checkStmts :: [AnStmt] -> TypeChecker [AnStmt]
checkStmts stmts = do
  -- accumulateFunDecl messes up the vars env, so it must be restored
  oldVars <- gets $ _vars . head . _env
  mapM_ accumulateFunDecl stmts
  state <- get
  let (x:xs) = _env state
  put state{_env=x{_vars=oldVars}:xs}
  mapM checkStmt stmts

-- Type-checker entry point
checkProgram :: Program -> TypeChecker Program
checkProgram (Prog stmts') = do
  let isVarDecl (A _ (Decl (A _ (Dvar _ _ _)))) = True
      isVarDecl _ = False
      (vardecls, otherStmts) = partition isVarDecl stmts'
      stmts = vardecls ++ otherStmts
  -- accumulateFunDecl messes up the vars env, so it must be restored
  oldVars <- gets $ _vars . head . _env
  mapM_ checkTopLevelStmt stmts
  mapM_ accumulateFunDecl stmts
  state <- get
  let (x:xs) = _env state
  put state{_env=x{_vars=oldVars}:xs}
  env <- gets _env
  case lookupFun (Ident "main") env of
    Nothing -> logError . L noSrcSpan . StaticError $ MissingMain
    Just (FunType mt rt,_,_) -> case rt of
      BasicType (BasicTypeInt) -> return ()
      _ -> logError . L noSrcSpan . StaticError $ MainIntReturnType
  checkedStmts <- mapM checkStmt stmts
  return $ Prog checkedStmts

-- Checks that a top-level Stmt is a declaration
checkTopLevelStmt :: AnStmt ->TypeChecker ()
checkTopLevelStmt anStmt = case unAnn anStmt of
  Decl _ -> return ()
  _ -> logError . L (srcSpanFirstCharacter $ getLoc anStmt) . StaticError $ UnexpectedStmt


-- Accumulates function declarations in the local environment
-- Call at the entrance of every block
accumulateFunDecl :: AnStmt -> TypeChecker ()
accumulateFunDecl (A _ (Decl anDecl)) = case unAnn anDecl of
  Dfun (FunType mt rt) anId fps _ -> do
    let id = unAnn anId
    env <- gets _env
    case lookupFun id [head env] of
      Just (_, loc, _) -> logError . L (getLoc anDecl) . StaticError $ RedeclaredFun id loc
      Nothing  -> do
        rt' <- checkRetTypeDecl rt `catchError` logTypeDeclError (getLoc anDecl) UnTyped
        mt' <- zipWithM checkModTypeDecl fps mt
        insertFun id (FunType mt' rt', getLoc anId, [])
  -- Accumulates also the variables in the environment, just to typecheck the signature of functions
  Dvar tt anId@(A e id) maybeRExpr -> do
    env <- gets _env
    case lookupVar id [head env] of -- Error only if declared in the same scope
      Just (_, loc, _, _, _) -> logError . L (getLoc e) . StaticError $ RedeclaredVar id loc
      Nothing             -> insertVar id (tt, getLoc e, Value, [], isJust maybeRExpr)
  _ -> return ()
accumulateFunDecl _ = return ()

checkStmt :: AnStmt -> TypeChecker AnStmt
checkStmt anStmt = do
  let old_a = getAnn anStmt
      stmt  = unAnn anStmt
  case stmt of
    RExprStmt r -> do
      node <- checkAndEvalRExpr r
      when (isArrayLit node) $ do
        logError . L (getLoc node) . StaticError $ UnexpectedArrayLit
      return $ A old_a $ RExprStmt node
    Decl anDecl -> case unAnn anDecl of
      Dvar tt anId@(A e id) maybeRExpr -> do
        t <- checkTypeDecl tt `catchError` logTypeDeclError (getLoc anDecl) UnTyped
        node <- mapM checkAndEvalRExpr maybeRExpr
        -- variable redeclaration already checked
        insertVar id (t, getLoc e, Value, [], True)
        case node of
          Nothing -> throwError . L (getLoc anDecl) . StaticError $ UnInitVarIdent id
          Just anRExpr -> do
            let tr = getType anRExpr
            case t of
              ArrayType _ _ -> do
                crexpr <- checkArrayLit t anRExpr
                return $ Decl (Dvar t anId (Just crexpr) <$ anDecl) <$ anStmt
              _ -> case isArrayLit anRExpr && not (isUnTyped t) of
                True  -> throwError . L (getLoc anDecl) . StaticError $ WrongScalarInit
                False -> case joinLeq tr t of
                  False -> throwError . L (getLoc anDecl) . StaticError $ UnexpectedType tr t
                  True  -> do
                    crexpr <- evaluateRExpr <$> possiblyCoerce anRExpr t
                    return $ Decl (Dvar t anId (Just crexpr) <$ anDecl) <$ anStmt
      Dfun t id@(A _ fname) fps stmts -> do
        theEnv <- gets _env
        -- types taken from the environment because they have been already type-checked
        let (FunType mt rt, _, _) = fromJust $ lookupFun fname theEnv
        newFunLocalEnv rt fps fname
        fps' <- zipWithM accumulateFormalParameter fps mt
        stmts' <- checkStmts stmts
        theEnv <- gets _env
        byResParam <- use (funcInfo.currentFunc._2)
        let notInitParams = filter (not . view _5 . fromJust . flip lookupResParam theEnv) $ map getFormalParameterId byResParam
        when (not $ null notInitParams) $ do
          logError . L (getLoc id) . StaticError $ ResNotInit notInitParams fname
        exitFunLocalEnv
        let newDecl = Decl (Dfun (FunType mt rt) id fps' stmts' <$ anDecl) <$ anStmt
        if not (isUnit rt) && not (doesAlwaysReturn stmts')
        then logErrorDefault newDecl . L (getLoc id) . StaticError $ ReturnNotPresent (unAnn id)
        else return newDecl
    Assgn anLExpr assignmentOp anRExpr -> do
      nodel <- inferTypeLExpr anLExpr
      case assignmentOp of
        Assign -> do
          markLExprAsInit nodel
          -- catches variables not marked by markLExprAsInit
          -- (e.g. variables inside pre-post inc-decr)
          checkLExprInit nodel
        _ -> checkLExprInit nodel
      checkLExprConst nodel
      noder <- checkAndEvalRExpr anRExpr
      let vc = getVC nodel
          tr = getType noder
          t  = getType nodel
      case vc of
        RValue -> throwError . L (getLoc nodel) . StaticError $ InvalidAssgnVC
        LValue -> case t of
          ArrayType _ _ -> throwError . L (getLoc nodel) . StaticError $ InvalidAssgnArray
          _ -> case isArrayLit noder of
            True  -> throwError . L (getLoc anStmt) . StaticError $ WrongScalarAssgn
            False -> case joinLeq tr t && areArrayDimEqual t tr of
              False -> throwError . L (getLoc anRExpr) . StaticError $ UnexpectedType tr t
              True -> do
                case assignmentOp of
                  Assign -> return t
                  AssgnArith anArithOp -> inferTypeArithOp anArithOp t tr `catchError` possiblyInvalidAssignOp
                  AssgnBit anBitOp -> inferTypeBitOp anBitOp t tr `catchError` possiblyInvalidAssignOp
                  AssgnLogic anBoolOp -> inferTypeBoolOp anBoolOp t tr `catchError` possiblyInvalidAssignOp
                crexpr <- evaluateRExpr <$> possiblyCoerce noder t -- coerce to the type of the l-expr
                return $ (Assgn nodel assignmentOp crexpr) <$ anStmt
    Block listStmt -> do
      newBlockLocalEnv
      newListStmt <- checkStmts listStmt
      exitBlockLocalEnv
      return $ (Block newListStmt) <$ anStmt
    While anRExpr listStmt -> do
      node <- checkAndEvalRExpr anRExpr
      let t = getType node
      case t of
        BasicType BasicTypeBool -> return ()
        _ -> logError . L (getLoc anRExpr) . StaticError $ (InvalidGuard t)
      newLoopLocalEnv
      newListStmt <- checkStmts listStmt
      exitLoopLocalEnv
      return $ (While node newListStmt) <$ anStmt
    Until anRExpr listStmt -> do
      node <- checkAndEvalRExpr anRExpr
      let t = getType node
      case t of
        BasicType BasicTypeBool -> return ()
        _ -> logError . L (getLoc anRExpr) . StaticError $ (InvalidGuard t)
      newLoopLocalEnv
      newListStmt <- checkStmts listStmt
      exitLoopLocalEnv
      return $ (Until node newListStmt) <$ anStmt
    DoWhile anRExpr listStmt -> do
      newLoopLocalEnv
      newListStmt <- checkStmts listStmt
      exitLoopLocalEnv
      node <- checkAndEvalRExpr anRExpr
      let t = getType node
      case t of
        BasicType BasicTypeBool -> return ()
        _ -> logError . L (getLoc anRExpr) . StaticError $ (InvalidGuard t)
      return $ (DoWhile node newListStmt) <$ anStmt
    For anIdent@(A e id) rng listStmt -> do
      let counterLExpr = A noAnn (VarIdent anIdent)
      counterLExpr <- inferTypeLExpr counterLExpr `catchError` logErrorDefault counterLExpr
      case getType counterLExpr of
        BasicType BasicTypeInt -> return ()
        t -> logError . L (getLoc e) . StaticError $ UnexpectedType t (BasicType BasicTypeInt)
      newRng <- checkRange rng
      oldLoopId <- loopid <<.= -1
      newLoopLocalEnv
      newListStmt <- checkStmts listStmt
      exitLoopLocalEnv
      loopid .= oldLoopId
      return $ For anIdent newRng newListStmt  <$ anStmt
    Loop stmt -> do
      newLoopLocalEnv
      newStmt <- checkStmt (A noAnn stmt)
      exitLoopLocalEnv
      byResFps <- use $ currentFuncInfo._2
      let initParams = filter (isResParamInitialized [newStmt]) $ map getFormalParameterId byResFps
      mapM_ markIdentAsInit initParams
      return $ (Loop (unAnn newStmt)) <$ anStmt
    IfStmt anIfStmt -> do
      newIfStmt <- checkIfStmt anIfStmt
      byResFps <- use $ currentFuncInfo._2
      let initParams = filter (flip isResParamInitIf newIfStmt) $ map getFormalParameterId byResFps
      mapM_ markIdentAsInit initParams
      return $ (IfStmt newIfStmt) <$ anStmt
    UnlessStmt anUlStmt -> do
      newUlStmt <- checkUlStmt anUlStmt
      byResFps <- use $ currentFuncInfo._2
      let initParams = filter (flip isResParamInitUl newUlStmt) $ map getFormalParameterId byResFps
      mapM_ markIdentAsInit initParams
      return $ (UnlessStmt newUlStmt) <$ anStmt
    Break -> do
      loopid <- gets _loopid
      if loopid <= 0
      -- log the error and emit an empty block in place of the erroneous break
      then logErrorDefault (anStmt $> Block []) $  L (getLoc anStmt) . StaticError $ InvalidJumpStmt (unAnn anStmt)
      else return anStmt
    Continue -> do
      loopid <- gets _loopid
      if loopid <= 0
      -- log the error and emit an empty block in place of the erroneous continue
      then logErrorDefault (anStmt $> Block []) $ L (getLoc anStmt) . StaticError $ InvalidJumpStmt (unAnn anStmt)
      else return anStmt
    RetExp maybeRExpr -> do
      retList <- gets _funcInfo
      case retList of
        [] -> error "Internal Compiler Error. Please report the bug to the maintainers."
        ((retType,byResParam,fname):_) -> do
          theEnv <- use env
          let notInitParams = filter (not . view _5 . fromJust . flip lookupResParam theEnv) $ map getFormalParameterId byResParam
          when (not $ null notInitParams) $ do
            logError . L (getLoc anStmt) . StaticError $ ResNotInit notInitParams fname
          retRExpr <- mapM checkAndEvalRExpr maybeRExpr
          let rexprType = fmap getType retRExpr
          case rexprType of
            Nothing -> case retType of
              Unit -> return $ anStmt $> (RetExp retRExpr)
              _      -> throwError . L (getLoc anStmt) . StaticError $ RetUnitFromVal retType
            Just tt -> do
              let rexpr = fromJust retRExpr
              case isArrayLit rexpr of
                True  -> throwError . L (getLoc rexpr) . StaticError $ InvalidRetArrayLit
                False -> case joinLeq tt retType && areArrayDimEqual tt retType of
                  True  -> do
                    newRExpr <- evaluateRExpr <$> possiblyCoerce rexpr retType
                    return $ anStmt $> (RetExp $ Just newRExpr)
                  False -> case retType of
                    Unit -> throwError . L (getLoc anStmt) . StaticError $ RetValFromUnit
                    _    -> throwError . L (getLoc rexpr) . StaticError $ UnexpectedRetType tt retType
    TryCatch try catch -> do
      newBlockLocalEnv
      newTry <- checkStmts try
      exitBlockLocalEnv
      newBlockLocalEnv
      newCatch <- checkStmts catch
      exitBlockLocalEnv
      return $ (TryCatch newTry newCatch) <$ anStmt
  `catchError` logErrorDefault anStmt -- catches all the errors in an AnStmt

-- Infers the type of a RExpr and evaluates any constant expression
checkAndEvalRExpr :: AnRExpr -> TypeChecker AnRExpr
checkAndEvalRExpr anRExpr = evaluateRExpr <$> inferTypeRExpr anRExpr `catchError` logErrorDefault anRExpr

checkIfStmt :: AnIfStmt -> TypeChecker AnIfStmt
checkIfStmt anIfStmt@(A ann ifStmt) = case ifStmt of
  If anRExpr listStmt maybeElse -> do
    node <- checkAndEvalRExpr anRExpr
    let t = getType node
    case t of
      BasicType BasicTypeBool -> return ()
      _  -> logError . L (getLoc anRExpr) . StaticError $ (InvalidGuard t)
    newBlockLocalEnv
    newListStmt <- checkStmts listStmt
    exitBlockLocalEnv
    elseNode <- mapM checkIfStmt maybeElse
    return . A ann $ If node newListStmt elseNode
  ElseIf anRExpr listStmt maybeElse -> do
    node <- checkAndEvalRExpr anRExpr
    let t = getType node
    case t of
      BasicType BasicTypeBool -> return ()
      _  -> logError . L (getLoc anRExpr) . StaticError $ (InvalidGuard t)
    newBlockLocalEnv
    newListStmt <- checkStmts listStmt
    exitBlockLocalEnv
    elseNode <- mapM checkIfStmt maybeElse
    return (A ann (ElseIf node newListStmt elseNode))
  IElse listStmt -> do
    newBlockLocalEnv
    newListStmt <- checkStmts listStmt
    exitBlockLocalEnv
    return (A ann (IElse newListStmt))

checkUlStmt :: AnUnlessStmt -> TypeChecker AnUnlessStmt
checkUlStmt anUlStmt@(A ann ulStmt) = case ulStmt of
  Unless anRExpr listStmt maybeElse -> do
    node <- checkAndEvalRExpr anRExpr
    let t = getType node
    case t of
      BasicType BasicTypeBool -> return ()
      _  -> logError . L (getLoc anRExpr) . StaticError $ (InvalidGuard t)
    newBlockLocalEnv
    newListStmt <- checkStmts listStmt
    exitBlockLocalEnv
    elseNode <- mapM checkUlStmt maybeElse
    return (A ann (Unless node newListStmt elseNode))
  UElse listStmt -> do
    newBlockLocalEnv
    newListStmt <- checkStmts listStmt
    exitBlockLocalEnv
    return (A ann (UElse newListStmt))

-- Checks range of For loop
checkRange :: Range -> TypeChecker Range
checkRange (Range anRExpr1 anRExpr2) = do
  node1 <- checkAndEvalRExpr anRExpr1
  node2 <- checkAndEvalRExpr anRExpr2
  let t1 = getType node1
      t2 = getType node2
  when (not $ isInt t1) $
    logError . L (getLoc anRExpr1) .StaticError $ InvalidRangeOp
  when (not $ isInt t2) $
    logError . L (getLoc anRExpr2) .StaticError $ InvalidRangeOp
  return $ (Range node1 node2)


-- Checks if the Array literal AnRExpr is compliant to the declared ArrayType
-- TypeSpec must be a type validated by checkTypeDecl
checkArrayLit :: TypeSpec -> AnRExpr -> TypeChecker AnRExpr
checkArrayLit (ArrayType d t) anRExpr = case isArrayLit anRExpr of
  False -> throwError . L (getLoc anRExpr) . StaticError $ ExpectedArrayLit
  True  -> do
    let getArrayLit :: AnRExpr -> [AnRExpr]
        getArrayLit (A _ (Const _ (Array l))) = l
        getArrayDim :: TypeSpec -> AnRExpr
        getArrayDim (ArrayType d _) = d
        tr = getType anRExpr
        expectedDim = evaluateIntRExpr d -- expected array dimension
        givenDim = evaluateIntRExpr $ getArrayDim tr -- given array dimension
    if expectedDim == givenDim then do
      l <- mapM (checkArrayLit t) . getArrayLit $ anRExpr
      return $ anRExpr $> Const tr (Array l)
    else
      throwError . L (getLoc anRExpr) . StaticError $ WrongArrayLitDim givenDim expectedDim
checkArrayLit t anRExpr = do
  let tr = getType anRExpr
  if isArrayLit anRExpr && not (isUnTyped t)
    then throwError . L (getLoc anRExpr) . StaticError $ WrongScalarInit
    else case joinLeq tr t && areArrayDimEqual tr t of
      False -> throwError . L (getLoc anRExpr) . StaticError $ UnexpectedType tr t
      True  -> do
        crexpr <- evaluateRExpr <$> possiblyCoerce anRExpr t
        return crexpr

-- Checks the dimension equality between two types
areArrayDimEqual :: TypeSpec -> TypeSpec -> Bool
areArrayDimEqual (RefType l_ts) (RefType r_ts) = areArrayDimEqual l_ts r_ts
areArrayDimEqual (ArrayType l_d l_ts) (ArrayType r_d r_ts) =
  let constness_l = getConst l_d
      constness_r = getConst r_d
  in
  if (isVarExpr constness_l) || (isVarExpr constness_r)
  then areArrayDimEqual l_ts r_ts
  else (evaluateIntRExpr l_d) == (evaluateIntRExpr r_d) && areArrayDimEqual l_ts r_ts
areArrayDimEqual (ArrayType _ _) (RefType _) = error "Internal Compiler Error. Please report the bug to the maintainers."
areArrayDimEqual (RefType _) (ArrayType _ _) = error "Internal Compiler Error. Please report the bug to the maintainers."
areArrayDimEqual l r = True

-- Modifies the error in case of type-checking of an AssignOp
possiblyInvalidAssignOp :: LcErrorType -> TypeChecker a
possiblyInvalidAssignOp e@(L _ (StaticError (InvalidInfixOp op l r))) = throwError
  $ StaticError (InvalidAssgnOp op l r) <$ e
possiblyInvalidAssignOp e = throwError e

-- Accumulates function parameters in the function local environment
-- Updates modality and type of the formal parameter with those specified by ModTypeSpec
-- Call at the entrance of every function
accumulateFormalParameter :: AnFormalParameter -> ModTypeSpec -> TypeChecker AnFormalParameter
accumulateFormalParameter fp@(A _ (FormalParameter _ _ anId)) (ModTypeSpec m t) = do
  let id  = unAnn anId
      loc = getLoc anId
  local_env <- gets $ head . _env
  case lookupVar id [local_env] of
    Nothing -> do
      let isInitialized Result = False
          isInitialized _ = True
      insertVar id (t, loc, m, [], isInitialized m)
      return $ (FormalParameter m t anId) <$ fp
    Just (_, l, _, _, _) -> logErrorDefault fp . L loc . StaticError $ RedeclaredParam id l

evaluateIntRExpr :: AnRExpr -> Int
evaluateIntRExpr anRExpr =
  let rexpr = unAnn anRExpr
  in
  case rexpr of
    InfixOp op anRExpr1 anRExpr2 ->
      case op of
        ArithOp o ->
          case unAnn o of
            Add -> (evaluateIntRExpr anRExpr1) + (evaluateIntRExpr anRExpr2)
            Sub -> (evaluateIntRExpr anRExpr1) - (evaluateIntRExpr anRExpr2)
            Mul -> (evaluateIntRExpr anRExpr1) * (evaluateIntRExpr anRExpr2)
            Mod -> (evaluateIntRExpr anRExpr1) `mod` (evaluateIntRExpr anRExpr2)
            Div -> (evaluateIntRExpr anRExpr1) `div` (evaluateIntRExpr anRExpr2)
            _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
        BitOp o ->
          case unAnn o of
            BAnd   -> (evaluateIntRExpr anRExpr1) .&. (evaluateIntRExpr anRExpr2)
            BOr    -> (evaluateIntRExpr anRExpr1) .|. (evaluateIntRExpr anRExpr2)
            BXor   -> xor (evaluateIntRExpr anRExpr1) (evaluateIntRExpr anRExpr2)
            LShift -> shiftL (evaluateIntRExpr anRExpr1) (evaluateIntRExpr anRExpr2)
            RShift -> shiftR (evaluateIntRExpr anRExpr1) (evaluateIntRExpr anRExpr2)
        _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
    UnaryOp op anRExpr ->
      case unAnn op of
        Neg   -> negate (evaluateIntRExpr anRExpr)
        Plus  -> evaluateIntRExpr anRExpr
        BComp -> complement (evaluateIntRExpr anRExpr)
        _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
    Const t c ->
      case t of
        BasicType BasicTypeInt ->
          case c of
            Int n -> n
            _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
        _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
    RBrackets anRExpr -> evaluateIntRExpr anRExpr
    Coercion t (A e rexpr) -> case t of
      BasicType BasicTypeInt -> let tr = getType e
        in
        case rexpr of
          (Const _ (Char c)) -> ord c
          _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
      _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
    _ -> error "Internal Compiler Error. Please report the bug to the maintainers."

evaluateFloatRExpr :: AnRExpr -> Double
evaluateFloatRExpr anRExpr =
  let rexpr = unAnn anRExpr
  in
  case rexpr of
    InfixOp op anRExpr1 anRExpr2 ->
      case op of
        ArithOp o ->
          case unAnn o of
            Add -> (evaluateFloatRExpr anRExpr1) + (evaluateFloatRExpr anRExpr2)
            Sub -> (evaluateFloatRExpr anRExpr1) - (evaluateFloatRExpr anRExpr2)
            Mul -> (evaluateFloatRExpr anRExpr1) * (evaluateFloatRExpr anRExpr2)
            Div -> (evaluateFloatRExpr anRExpr1) / (evaluateFloatRExpr anRExpr2)
            Exp -> (evaluateFloatRExpr anRExpr1) ** (evaluateFloatRExpr anRExpr2)
            _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
        _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
    UnaryOp op anRExpr ->
      case unAnn op of
        Neg   -> negate (evaluateFloatRExpr anRExpr)
        Plus  -> evaluateFloatRExpr anRExpr
        _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
    Const t c ->
      case t of
        BasicType BasicTypeFloat ->
          case c of
            Float n -> n
            _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
        _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
    RBrackets anRExpr -> evaluateFloatRExpr anRExpr
    Coercion t anRExpr ->
      case t of
        BasicType BasicTypeFloat ->
          let tr = getType anRExpr
          in
          case tr of
            BasicType BasicTypeInt -> fromIntegral (evaluateIntRExpr anRExpr)
            BasicType BasicTypeChar -> case unAnn anRExpr of
              Const _ (Char c) -> fromIntegral (ord c)
              _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
            _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
        _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
    _ -> error "Internal Compiler Error. Please report the bug to the maintainers."

evaluateBoolRExpr :: AnRExpr -> Bool
evaluateBoolRExpr anRExpr =
  let rexpr = unAnn anRExpr
  in
  case rexpr of
    InfixOp op anRExpr1 anRExpr2 ->
      case op of
        RelOp o ->
          let (A _ rexpr1) = evaluateRExpr anRExpr1
              (A _ rexpr2) = evaluateRExpr anRExpr2
          in case unAnn o of
            Eq  -> rexpr1 == rexpr2
            Neq -> rexpr1 /= rexpr2
            Lt  -> rexpr1 <  rexpr2
            LtE -> rexpr1 <= rexpr2
            Gt  -> rexpr1 >  rexpr2
            GtE -> rexpr1 >= rexpr2
        BoolOp o ->
          case unAnn o of
            And -> (evaluateBoolRExpr anRExpr1) && (evaluateBoolRExpr anRExpr2)
            Or  -> (evaluateBoolRExpr anRExpr1) || (evaluateBoolRExpr anRExpr2)
        _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
    UnaryOp op anRExpr ->
      case unAnn op of
        Not -> not (evaluateBoolRExpr anRExpr)
        _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
    Const t c ->
      case t of
        BasicType BasicTypeBool ->
          case c of
            Bool b -> b
            _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
        _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
    RBrackets anRExpr -> evaluateBoolRExpr anRExpr
    _ -> error "Internal Compiler Error. Please report the bug to the maintainers."

-- Compile-time evaluation of constant expressions
-- Call only after inferTypeRExpr
evaluateRExpr :: AnRExpr -> AnRExpr
evaluateRExpr anRExpr =
  let constness = getConst anRExpr
      t = getType anRExpr
      old_a = getAnn anRExpr
  in
  case constness of
    VarExpr -> case unAnn anRExpr of
        InfixOp o l r -> anRExpr $> InfixOp o (evaluateRExpr l) (evaluateRExpr r)
        UnaryOp o r -> anRExpr $> UnaryOp o (evaluateRExpr r)
        FCall id l -> anRExpr $> FCall id (map evaluateRExpr l)
        RBrackets r -> anRExpr $> RBrackets (evaluateRExpr r)
        LExpr l -> anRExpr
        Ref l -> anRExpr
        Coercion ts r -> anRExpr $> Coercion ts (evaluateRExpr r)
        Const _ _ -> anRExpr
        TernaryOp g t e -> anRExpr $> TernaryOp (evaluateRExpr g) (evaluateRExpr t) (evaluateRExpr e)
    ConstExpr ->
      case t of
        BasicType BasicTypeInt ->
          let res = evaluateIntRExpr anRExpr
              evRExpr = Const t (Int res)
          in
          A old_a evRExpr
        BasicType BasicTypeFloat ->
          let res = evaluateFloatRExpr anRExpr
              evRExpr = Const t (Float res)
          in
          A old_a evRExpr
        BasicType BasicTypeBool ->
          let res = evaluateBoolRExpr anRExpr
              evRExpr = Const t (Bool res)
          in
          A old_a evRExpr
        BasicType BasicTypeString -> case unAnn anRExpr of
          Coercion _ (A _ (Const _  (Char c))) -> anRExpr $> Const t (String [c])
          _ -> anRExpr
        _ -> anRExpr

-- Checks that every execution path of a function returns a value
doesAlwaysReturn :: [AnStmt] -> Bool
doesAlwaysReturn [] = False
doesAlwaysReturn (anStmt:xs) = case unAnn anStmt of
  Block stmts -> doesAlwaysReturn stmts || doesAlwaysReturn xs
  RetExp maybeRExpr -> True
  IfStmt anIfStmt -> doesAlwaysReturnIf anIfStmt || doesAlwaysReturn xs
  UnlessStmt anUlStmt -> doesAlwaysReturnUl anUlStmt || doesAlwaysReturn xs
  Loop (Block stmts) -> doesAlwaysReturn stmts || doesAlwaysReturn xs
  Break -> False
  TryCatch try catch -> (doesAlwaysReturn try && doesAlwaysReturn catch) || doesAlwaysReturn xs
  _ -> doesAlwaysReturn xs

-- auxiliary function of doesAlwaysReturn for IfStmt
doesAlwaysReturnIf :: AnIfStmt -> Bool
doesAlwaysReturnIf anIfStmt = case unAnn anIfStmt of
  If _ stmts maybeElse -> doesAlwaysReturn stmts
                          && maybe False doesAlwaysReturnIf maybeElse
  ElseIf _ stmts maybeElse -> doesAlwaysReturn stmts
                              && maybe False doesAlwaysReturnIf maybeElse
  IElse stmts -> doesAlwaysReturn stmts

-- auxiliary function of doesAlwaysReturn for UnlessStmt
doesAlwaysReturnUl :: AnUnlessStmt -> Bool
doesAlwaysReturnUl anUlStmt = case unAnn anUlStmt of
  Unless _ stmts maybeElse -> doesAlwaysReturn stmts
                              && maybe False doesAlwaysReturnUl maybeElse
  UElse stmts -> doesAlwaysReturn stmts

getFormalParameterId :: FormalParameter -> Ident
getFormalParameterId (FormalParameter _ _ (A _ id)) = id

-- Checks recursively if a parameter with Result Modality is initialized in every execution path
isResParamInitialized :: [AnStmt] -> Ident -> Bool
isResParamInitialized [] id = False
isResParamInitialized (anStmt:xs) id = case unAnn anStmt of
  Block stmts -> isResParamInitialized stmts id
                 || isResParamInitialized xs id
  Assgn anLExpr Assign _ -> isResParamAssigned id anLExpr
  Decl (A _ (Dvar _ (A _ id') _)) -> not (id' == id) && isResParamInitialized xs id
  IfStmt anIfStmt -> isResParamInitIf id anIfStmt
                     || isResParamInitialized xs id
  UnlessStmt anUlStmt -> isResParamInitUl id anUlStmt
                         || isResParamInitialized xs id
  Loop (Block stmts) -> isResParamInitialized stmts id
                        || isResParamInitialized xs id
  TryCatch try catch -> (isResParamInitialized try id && isResParamInitialized catch id)
                        || isResParamInitialized xs id
  RetExp _ -> False
  Break -> False
  Continue -> False
  _ -> isResParamInitialized xs id

-- auxiliary function of isResParamInitialized for IfStmt
isResParamInitIf :: Ident -> AnIfStmt -> Bool
isResParamInitIf id anIfStmt =
  case unAnn anIfStmt of
    If _ stmts maybeElse -> isResParamInitialized stmts id && maybe False (isResParamInitIf id) maybeElse
    ElseIf _ stmts maybeElse ->  isResParamInitialized stmts id && maybe False (isResParamInitIf id) maybeElse
    IElse stmts -> isResParamInitialized stmts id

-- auxiliary function of isResParamInitialized for UnlessStmt
isResParamInitUl :: Ident -> AnUnlessStmt -> Bool
isResParamInitUl id anUlStmt =
  case unAnn anUlStmt of
    Unless _ stmts maybeElse -> isResParamInitialized stmts id && maybe False (isResParamInitUl id) maybeElse
    UElse stmts -> isResParamInitialized stmts id

-- auxiliary function of isResParamInitialized
isResParamAssigned :: Ident -> AnLExpr -> Bool
isResParamAssigned id lexpr = case unAnn lexpr of
  VarIdent (A _ id') -> id == id'
  _                  -> False

-- Checks the validity of the type in a declaration
-- Dimensions of an Array type are evaluated
-- Error location must be set by the caller
checkTypeDecl :: TypeSpec -> TypeChecker TypeSpec
checkTypeDecl (Unit) = throwError . L noSrcSpan . StaticError $ NilDeclaration
checkTypeDecl (ArrayType d t) = do
  i_d <- inferTypeRExpr d
  let tp = getType i_d
      constness = getConst i_d
  when (isVarExpr constness) $ do
    throwError . L noSrcSpan . StaticError $ VarSizeArray
  case tp of
    BasicType BasicTypeInt -> do
      let e_d@(A _ (Const _ (Int i))) = evaluateRExpr i_d
      when (i <= 0) $ do
        throwError . L noSrcSpan . StaticError $ SizeArrayNonPositive
      c_t <- checkTypeDecl t
      return $ ArrayType e_d c_t
    _ -> throwError . L noSrcSpan . StaticError $ SizeArrayNonInteger
checkTypeDecl (RefType t) = do
      c_t <- checkTypeDecl t
      return $ RefType c_t
checkTypeDecl ts = return ts

-- Checks the validity of the function return type
checkRetTypeDecl :: TypeSpec -> TypeChecker TypeSpec
checkRetTypeDecl (ArrayType d t) =
  throwError . L noSrcSpan . StaticError $ InvalidArrayRetType
checkRetTypeDecl (RefType t) = do
  c_t <- checkTypeDecl t
  return $ RefType c_t
checkRetTypeDecl x = return x

-- Checks the validity of a formal parameter modality and type
checkModTypeDecl :: AnFormalParameter -> ModTypeSpec -> TypeChecker ModTypeSpec
checkModTypeDecl anFp mt@(ModTypeSpec Result (ArrayType _ _)) =
  logErrorDefault (ModTypeSpec Value UnTyped) . L (getLoc anFp) . StaticError $ InvalidArrayResMod
checkModTypeDecl anFp mt@(ModTypeSpec ValueResult t@(ArrayType _ _)) = do
  t' <- checkTypeDecl t `catchError` logTypeDeclError (getLoc anFp) UnTyped
  return $ ModTypeSpec ValueResult t'
checkModTypeDecl anFp mt@(ModTypeSpec Value t@(ArrayType _ _)) = do
  t' <- checkTypeDecl t `catchError` logTypeDeclError (getLoc anFp) UnTyped
  return $ ModTypeSpec Value t'
checkModTypeDecl anFp mt@(ModTypeSpec m t@(BasicType BasicTypeString)) = case m of
  Value -> logErrorDefault (ModTypeSpec m t) . L (getLoc anFp) . StaticError $ VarSizeObjectArg
  ValueResult -> logErrorDefault (ModTypeSpec m t) . L (getLoc anFp) . StaticError $ VarSizeObjectArg
  _ -> return $ ModTypeSpec m t
checkModTypeDecl anFp (ModTypeSpec m t) = do
  t' <- checkTypeDecl t `catchError` logTypeDeclError (getLoc anFp) UnTyped
  return $ ModTypeSpec m t'

-- Checks if a declared Array is variable sized
-- Returns True if a declared Array is variable sized
-- Returns False if a declared Array is of constant dimension
isVarSizeArray :: TypeSpec -> Bool
isVarSizeArray (ArrayType d ts) =
  if isVarExpr . getConst $ d
  then True
  else isVarSizeArray ts
isVarSizeArray _ = False

-- Logs declaration error with the given SrcSpan
logTypeDeclError :: SrcSpan -> a -> LcErrorType -> TypeChecker a
logTypeDeclError l a e = catchError (localizeDeclError l e) $ logErrorDefault a

-- Auxiliary function of logTypeDeclError
localizeDeclError :: SrcSpan -> LcErrorType -> TypeChecker a
localizeDeclError l e = do
  let te = unLoc e
  case te of
    StaticError NilDeclaration -> throwError . L l $ te
    StaticError SizeArrayNonInteger -> throwError . L l $ te
    StaticError InvalidArrayRetType -> throwError . L l $ te
    StaticError VarSizeArray -> throwError . L l $ te
    StaticError SizeArrayNonPositive -> throwError . L l $ te
    _ -> throwError e
