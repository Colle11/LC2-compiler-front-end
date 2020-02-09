{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module TAC (
  Addr,
  Instruction,
  codeGenTAC,
  initialEnv,
  prettyPrintTAC
           ) where

import AST
import Types
import SourceLocation hiding (getLoc)
import TypeChecker
import Compiler.SymbolTable
import Annotations
import PrettyPrinting

import Prelude hiding (lookup)
import Data.Maybe
import Data.Monoid (First(..))
import Data.Bool
import qualified Data.List as List
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Hash
import Data.Foldable
import Control.Monad.State
import Control.Monad.Reader
import Lens.Micro.Platform

-- TAC instructions
data Instruction
  = AssgnInfixOp TypeSpec TInfixOp Addr Addr Addr
  | AssgnUnaryOp TypeSpec TUnaryOp Addr Addr
  | Copy TypeSpec Addr Addr
  | TGoto Label
  | GotoIf Addr Label
  | GotoIfFalse Addr Label
  | GotoRelOp TRelOp Addr Addr Label
  | OnExceptionGoto Label
  | EndCatch
  | Param TypeSpec Addr
  | TPCall Addr Int -- procedure name, number of arguments
  | TFCall TypeSpec Addr Addr Int -- result, function name, number of arguments
  | Return TypeSpec (Maybe Addr)
  | RIndexedCopy TypeSpec Addr Addr Addr -- x y i => x = y[i]
  | LIndexedCopy TypeSpec Addr Addr Addr -- x y i => x[i] = y
  | AddressOf Addr Addr
  | RDeref TypeSpec Addr Addr
  | LDeref TypeSpec Addr Addr
  | Label Label Instruction
  | LineComment String
  | InlineComment String
  | DataSection SDLog
  deriving (Show, Eq, Ord)

-- Amount of tac instruction indentation
indent :: Int
indent = 20

instance PrPrint Instruction where
  prettyDoc = pd
    where
      pd (AssgnInfixOp t op res l r) = prettyDoc res <+>
                                       equals <.>
                                       tacTypePrPrint t <+>
                                       prettyDoc l <+>
                                       prettyDoc op <+>
                                       prettyDoc r
      pd (AssgnUnaryOp t op res r)   = prettyDoc res <+>
                                       equals <.>
                                       tacTypePrPrint t <+>
                                       prettyDoc op <+>
                                       prettyDoc r
      pd (Copy t res a)              = prettyDoc res <+>
                                       equals <.>
                                       tacTypePrPrint t <+>
                                       prettyDoc a
      pd (TGoto l)                   = text "goto" <+> prettyDoc l
      pd (GotoIf a l)                = text "if" <+>
                                       prettyDoc a <+>
                                       text "goto" <+>
                                       prettyDoc l
      pd (GotoIfFalse a l)           = text "ifFalse" <+>
                                       prettyDoc a <+>
                                       text "goto" <+>
                                       prettyDoc l
      pd (GotoRelOp op l r lab)      = text "if" <+>
                                       prettyDoc l <+>
                                       prettyDoc op <+>
                                       prettyDoc r <+>
                                       text "goto" <+>
                                       prettyDoc lab
      pd (OnExceptionGoto l)         = text "on_exception_goto" <+>
                                       prettyDoc l
      pd (EndCatch)                  = text "end_catch"
      pd (Param t a)                 = text "param_" <.>
                                       tacTypePrPrint t <+>
                                       prettyDoc a
      pd (TPCall f i)                = text "call" <+>
                                       prettyDoc f <+>
                                       int i
      pd (TFCall t res f i)          = prettyDoc res <+>
                                       text "=" <.>
                                       tacTypePrPrint t <+>
                                       text "call" <+>
                                       prettyDoc f <+>
                                       int i
      pd (Return t m)                = text "return_" <.>
                                       tacTypePrPrint t <+>
                                       maybe empty prettyDoc m
      pd (RIndexedCopy t x y i)      = prettyDoc x <+>
                                       equals <.>
                                       tacTypePrPrint t <+>
                                       prettyDoc y <.>
                                       brackets (prettyDoc i)
      pd (LIndexedCopy t x y i)      = prettyDoc x <.>
                                       brackets (prettyDoc i) <+>
                                       equals <.>
                                       tacTypePrPrint t <+>
                                       prettyDoc y
      pd (AddressOf res a)           = prettyDoc res <+>
                                       text "=ref" <+>
                                       char '&' <.>
                                       prettyDoc a
      pd (RDeref t res a)            = prettyDoc res <+>
                                       equals <.> tacTypePrPrint t <+>
                                       char '*' <.> prettyDoc a
      pd (LDeref t res a)            = char '*' <.> prettyDoc res <+>
                                       equals <.> tacTypePrPrint t <+>
                                       prettyDoc a
      pd (Label l i@(Label _ _))     = prettyDoc l <.> char ':' $+$ prettyDoc i
      pd (Label l i)                 = prettyDoc l <.> char ':' $$ nest indent (prettyDoc i)
      pd (LineComment s)             = char '#' <+> text s
      pd (InlineComment s)           = char '#' <+> text s
      pd (DataSection sdl)           = text "\n\n.data: # static data\n" $+$ (vcat $ map prettyDoc sdl)

-- Program static data
data StaticData
  = StringPointer Addr String
  deriving (Show, Eq, Ord)

instance PrPrint StaticData where
  prettyDoc = pd
    where
      pd (StringPointer a s) = prettyDoc a $$ nest indent (doubleQuotes $ text s)

-- TAC infix operations
data TInfixOp
  = TAdd TypeSpec
  | TSub TypeSpec
  | TMul TypeSpec
  | TMod TypeSpec
  | TDiv TypeSpec
  | TExp TypeSpec
  | TBAnd
  | TBOr
  | TBXor
  | TLShift
  | TRShift
  -- Ref +/- Offset
  | TAddRefInt
  | TAddIntRef
  | TSubRefInt
  deriving (Show, Eq, Ord)

instance PrPrint TInfixOp where
  prettyDoc = pd
    where
      pd (TAdd t) = char '+' <.> tacTypePrPrint t
      pd (TSub t) = char '-' <.> tacTypePrPrint t
      pd (TMul t) = char '*' <.> tacTypePrPrint t
      pd (TMod t) = char '%' <.> tacTypePrPrint t
      pd (TDiv t) = char '/' <.> tacTypePrPrint t
      pd (TExp t) = text "**" <.> tacTypePrPrint t
      pd (TBAnd)  = char '&' <.> text "int"
      pd (TBOr)   = char '|' <.> text "int"
      pd (TBXor)  = char '^' <.> text "int"
      pd (TLShift)= text "<<" <.> text "int"
      pd (TRShift)= text ">>" <.> text "int"
      pd (TAddRefInt) = text "ref_plus_int"
      pd (TAddIntRef) = text "int_plus_ref"
      pd (TSubRefInt) = text "ref_minus_int"

-- TAC unary operations
data TUnaryOp
  = TNot
  | TNeg TypeSpec
  | TPlus TypeSpec
  | TBComp
  | TConvFrTo TypeSpec TypeSpec -- Type conversion: From-To
  deriving (Show, Eq, Ord)

instance PrPrint TUnaryOp where
  prettyDoc = pd
    where
      pd (TNot) = char '!'
      pd (TNeg t) = char '-' <.> tacTypePrPrint t
      pd (TPlus t) = char '+' <.> tacTypePrPrint t
      pd (TBComp) = char '~' <.> text "int"
      pd (TConvFrTo f t) = text "cnv_" <.>
                             tacTypePrPrint f <.>
                             text "_to_" <.>
                             tacTypePrPrint t

-- TAC relational operations
data TRelOp = TRelOp RelOp TypeSpec
  deriving (Show, Eq, Ord)

instance PrPrint TRelOp where
  prettyDoc (TRelOp op t) = prettyDoc op <.> tacTypePrPrint t

-- TAC labels
data Label
  = LCmd Int           -- cmd{Int}
  | LFun Ident Loc     -- function_id{location}
  | LEndFun Int        -- endfun{Int}
  | LSetFalse Int      -- setfalse{Int}
  | LEndSet Int        -- endset{Int}
  | LGuard Int         -- guard{Int}
  | LElse Int          -- else{Int}
  | LBody Int          -- body{Int}
  | LCatch Int         -- catch{Int}
  deriving (Show, Eq, Ord)

instance PrPrint Label where
  prettyDoc = pd
    where
      pd (LCmd i)        = text "cmd" <.> int i
      pd (LFun id l)     = prettyDoc id <.> text "@" <.> prettyDoc l
      pd (LEndFun i)     = text "endfun" <.> int i
      pd (LSetFalse i)   = text "setfalse" <.> int i
      pd (LEndSet i)     = text "endset" <.> int i
      pd (LGuard i)      = text "guard" <.> int i
      pd (LElse i)       = text "else" <.> int i
      pd (LBody i)       = text "body" <.> int i
      pd (LCatch i)      = text "catch" <.> int i

-- Pretty printing of TAC instructions
prettyPrintTAC :: [Instruction] -> String
prettyPrintTAC = render . prettyDocTAC

-- Auxiliary function of prettyPrintTAC
prettyDocTAC :: [Instruction] -> Doc
prettyDocTAC xs = foldl f empty xs
  where
    f :: Doc -> Instruction -> Doc
    f d i@(Label _ _) = d $$ prettyDoc i
    f d i@(InlineComment _) = d $$ nest indent (prettyDoc i)
    f d i@(DataSection _) = d $+$ prettyDoc i
    f d i = d $+$ nest indent (prettyDoc i)

type SymbolTable = [LocalEnv]

type TACLog = [Instruction] -> [Instruction]

type SDLog = [StaticData]

-- State of the CodeGen Monad
data CGState = CGState {
  _symtable   :: SymbolTable,
  _tac        :: TACLog,      -- generated TAC
  _tempCount  :: Int,         -- counter for unique temporaries generation
  _labelCount :: Int,         -- counter for unique labels generation
  _staticData :: SDLog,       -- generated static data
  _staticDataCount :: Int     -- counter for unique static data labels generation
  }

-- Int annotation
intAnn :: Ann
intAnn = Ann {
  _type   = (BasicType BasicTypeInt),
  _loc    = noSrcSpan,
  _const  = VarExpr,
  _valcat = RValue
}

-- Bool annotation
boolAnn :: Ann
boolAnn = Ann {
  _type   = (BasicType BasicTypeBool),
  _loc    = noSrcSpan,
  _const  = VarExpr,
  _valcat = RValue
}

-- TemplateHaskell: generates the lenses (getters+setters) for CGState
makeLenses ''CGState

-- Initial state of CodeGen Monad
initialCGState :: CGState
initialCGState = CGState [initialEnv] id 0 0 [] 0

-- Environment carried by ReaderT Monad
-- Used to simulate the inherited attributes of an attributed grammar
data IL = IL { -- IL: Inherited labels
  _BTrue  :: Maybe Label,  -- Nothing means Fall Through
  _BFalse :: Maybe Label,
  _SNext  :: Maybe Label,
  -- Loops starting label
  _LGuard :: Maybe Label,
  -- Loops labels for break and continue stmts
  _LContinue :: Maybe Label,
  _LBreak :: Maybe Label,
  -- Catch label for try-catch blocks
  _LCatch :: Maybe Label
  }

-- TemplateHaskell: generates the lenses (getters+setters) for IL
makeLenses ''IL

-- Initial state of Reader Monad
initialIL :: IL
initialIL = IL Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- THE CHOSEN MONAD STACK:
newtype CodeGenT m a = CodeGenT {
  runCGT :: StateT CGState (ReaderT IL m) a
  }
  deriving (
    Functor,
    Applicative,
    Monad,
    MonadState CGState,
    MonadReader IL
    )

-- The monad inside which we perform the code-gen
type CodeGen a = CodeGenT IO a

-- Executes the monadic action CodeGenT from the initial states CGState and IL
runCodeGenT :: IL -> CGState -> CodeGenT m a -> m (a, CGState)
runCodeGenT r s t = flip runReaderT r . flip runStateT s . runCGT $ t

-- Appends TAC to the log
pushBackTAC :: TACLog -> CodeGen ()
pushBackTAC f = do
  modify g
    where
      g :: CGState -> CGState
      g s = s {_tac = _tac s . f }

-- Entry point of the TAC
codeGenTAC :: Program -> IO [Instruction]
codeGenTAC (Prog stmts) = do
  tac <- _tac . snd <$> runCodeGenT initialIL initialCGState (codeGenProgram stmts)
  return $ tac []

-- Attaches a label to the next instruction
attachLabel :: Label -> TACLog
attachLabel lab (x:xs) = Label lab x : xs
attachLabel lab []   = [Label lab (InlineComment "no instruction")]

-- Generates a new temporary
newTemp :: CodeGen Addr
newTemp = do
  int <- tempCount <<%= (+1)
  return $ TTemp int

-- Generates a new label
newLabel :: (Int -> Label) -> CodeGen Label
newLabel c = do
  int <- labelCount <<%= (+1)
  return $ c int

-- Increments static data counter
newStaticData :: CodeGen Int
newStaticData = staticDataCount <<%= (+1)

-- Creates the local environment of a Function
newFunLocalEnv :: CodeGen ()
newFunLocalEnv = do
  symtable %= (emptyEnv:)

-- Deletes the most recent local environment
exitFunLocalEnv :: CodeGen ()
exitFunLocalEnv = do
  symtable %= (tail)

-- Creates a new local environment
newBlockLocalEnv :: CodeGen ()
newBlockLocalEnv = do
  modify addEnv
    where
      addEnv s = s{_symtable=emptyEnv:_symtable s}

-- Deletes the most recent local environment
exitBlockLocalEnv :: CodeGen ()
exitBlockLocalEnv = symtable %= tail

{- SYMBOL TABLE HELPER FUNCTIONS -}

-- Generic insert
-- Usage: insert (symbolTable.localenv.vars) key value
insert :: (MonadState s m, Eq k, Hashable k) => Traversal' s (HashMap k v) -> k -> v -> m ()
insert l k v = l %= Hash.insert k v

-- Lookup in all the visible scopes
-- Usage: lookup vars envs key
lookup :: (Eq k, Hashable k) => Lens' LocalEnv (HashMap k v) -> [LocalEnv] -> k -> Maybe v
lookup l xs k = getFirst . mconcat $ map (First . Hash.lookup k .  view l) xs

-- Lookup only on the current local environment
lookupLocal :: (Eq k, Hashable k) => Lens' LocalEnv (HashMap k v) -> [LocalEnv] -> k -> Maybe v
lookupLocal l xs k = xs ^. localenv . l & Hash.lookup k

{- GENERAZIONE TAC -}

-- Code-gen of a RExpr
codeGenRExpr :: AnRExpr -> CodeGen (Addr, [Addr])
codeGenRExpr aR = case unAnn aR of
  Const t (String s) -> do
    int <- newStaticData
    let strptr = TStrPtr int
    staticData %= (StringPointer strptr s:)
    return (strptr, [TConst . Int $ sizeof (coerceStringToRef t)])
  Const t c -> return (TConst c, [TConst . Int $ sizeof t])
  InfixOp op l r -> case op of
    ArithOp o -> codeGenArithOp (unAnn o) l r
    BitOp   o -> codeGenBitOp (unAnn o) l r
    RelOp   o -> do
      sf <- newLabel LSetFalse
      es <- newLabel LEndSet
      let basicBool = BasicType BasicTypeBool
          f il = il {_BTrue=Nothing, _BFalse=Just sf}
      local f $ codeGenRelOp (unAnn o) l r
      res <- newTemp
      pushBackTAC $ (++) [
             Copy  basicBool res . TConst . Bool $ True,
             TGoto es,
             Label sf .
             Copy basicBool res . TConst . Bool $ False
           ] . attachLabel es
      return (res, [TConst . Int $ sizeof basicBool])
    BoolOp  o -> do
      sf <- newLabel LSetFalse
      es <- newLabel LEndSet
      let basicBool = BasicType BasicTypeBool
          f il = il {_BTrue=Nothing, _BFalse=Just sf}
      local f $ codeGenBoolOp (unAnn o) l r
      res <- newTemp
      pushBackTAC $ (++) [
             Copy basicBool res . TConst . Bool $ True,
             TGoto es,
             Label sf .
             Copy basicBool res . TConst . Bool $ False
           ] . attachLabel es
      return (res, [TConst . Int $ sizeof basicBool])
  UnaryOp op anRExpr -> case unAnn op of
    Not -> do
      sf <- newLabel LSetFalse
      es <- newLabel LEndSet
      let basicBool = BasicType BasicTypeBool
          f il = il {_BFalse=Nothing, _BTrue=Just sf}
      local f $ codeGenBoolGuards anRExpr
      res <- newTemp
      pushBackTAC $ (++) [
             Copy basicBool res . TConst . Bool $ True,
             TGoto es,
             Label sf .
             Copy basicBool res . TConst . Bool $ False
           ] . attachLabel es
      return (res, [TConst . Int $ sizeof basicBool])
    Neg -> do
      let opType = getType anRExpr
          unaryNeg = TNeg opType
      (t, [w]) <- codeGenRExpr anRExpr
      res <- newTemp
      pushBackTAC $ (AssgnUnaryOp opType unaryNeg res t :)
      return (res, [w])
    Plus -> codeGenRExpr anRExpr
    BComp -> do
      let opType = getType anRExpr
      (t, [w]) <- codeGenRExpr anRExpr
      res <- newTemp
      pushBackTAC $ (AssgnUnaryOp opType TBComp res t :)
      return (res, [w])
  RBrackets anRExpr -> codeGenRExpr anRExpr
  Coercion toT anRExpr -> do
    let fromT = getType anRExpr
        conv = TConvFrTo fromT toT
    case (toT, fromT) of
      (BasicType BasicTypeString, BasicType BasicTypeChar) -> do
        (temp, [_]) <- codeGenRExpr anRExpr
        pushParam fromT temp
        res <- newTemp
        -- Call to runtime routine create_string_from_char
        pushBackTAC $ (TFCall (coerceStringToRef toT) res (TName (Ident "create_string_from_char") (NoLoc "")) 1 :)
        return (res, [TConst . Int . sizeof $ coerceStringToRef toT])
      _ -> do
        (temp, ws) <- codeGenRExpr anRExpr
        res <- newTemp
        pushBackTAC $ (AssgnUnaryOp toT conv res temp :)
        return (res, ws)
  LExpr anLExpr -> do
    let lexprType = coerceStringToRef $ getType anLExpr
    (addr, offset, ws, isRef) <- codeGenLExpr anLExpr
    case isArrayType lexprType of
      False -> do
        temp <- newTemp
        case offset of
          Just o -> pushBackTAC $ (RIndexedCopy lexprType temp addr o :)
          Nothing -> case isRef of
            True  -> pushBackTAC $ (RDeref lexprType temp addr :)
            False -> pushBackTAC $ (Copy lexprType temp addr :)
        return (temp, ws)
      True -> do
        case offset of
          Just o -> do
            let opType = coerceArrayToRef lexprType
                refSize = TConst . Int $ sizeof opType
            temp1 <- newTemp
            pushBackTAC $ (AssgnInfixOp opType TAddRefInt temp1 addr o :)
            return (temp1, refSize : tail ws)
          Nothing -> do
            let opType = coerceArrayToRef lexprType
                refSize = TConst . Int $ sizeof opType
            temp <- newTemp
            pushBackTAC $ (Copy opType temp addr :)
            return (temp, refSize : tail ws)
  Ref anLExpr -> do
    let lType = getType anLExpr
        refSize  = TConst . Int . sizeof $ getType aR
    (addr, offset, ws, isRef) <- codeGenLExpr anLExpr
    case offset of
      Just o -> do
        let opType = RefType (getArrayBasicType lType)
        temp <- newTemp
        pushBackTAC $ (AssgnInfixOp opType TAddRefInt temp addr o :)
        return (temp, refSize:ws)
      Nothing -> do
        temp <- newTemp
        case isRef || isArrayType lType of
          True  -> pushBackTAC $ (Copy (RefType lType) temp addr :)
          False -> pushBackTAC $ (AddressOf temp addr :)
        return (temp, refSize:ws)
  FCall anId args -> do
    let id = unAnn anId
    env <- use symtable
    case lookup funs env id of
      Nothing -> error "Internal Compiler Error. Please report the bug to the maintainers."
      Just (FunType mt rt, loc, ws) -> do
        zipWithM_ generateFuncArgument mt args
        let numParams = foldl numberOfParameters 0 mt
        case rt of
          Unit -> do
            pushBackTAC $ (TPCall (TName id (srcSpanStartLoc loc)) numParams :)
            return $ (TInvAddr, ws)
          _ -> do
            res <- newTemp
            pushBackTAC $ (TFCall (coerceStringToRef rt) res (TName id (srcSpanStartLoc loc)) numParams :)
            return (res, ws)
  TernaryOp gRExpr tRExpr eRExpr -> do
    sf <- newLabel LSetFalse
    es <- newLabel LEndSet
    let resType = getType tRExpr
        f il = il {_BTrue=Nothing, _BFalse=Just sf}
    local f $ codeGenBoolGuards gRExpr
    res <- newTemp
    -- true case (then)
    (tTemp, tWs) <- codeGenRExpr tRExpr
    pushBackTAC $ (++) [
      Copy resType res tTemp,
      TGoto es
      ] . attachLabel sf
    -- false case (else)
    (eTemp, eWs) <- codeGenRExpr eRExpr
    pushBackTAC $ (Copy resType res eTemp :)
    pushBackTAC $ attachLabel es
    return (res, tWs)

-- Calculates the number of parameters of a function
numberOfParameters :: Int -> ModTypeSpec -> Int
numberOfParameters n (ModTypeSpec Value t)
  | isArrayType t = n + (elementsOfStaticArray t)
  | otherwise = n + 1
numberOfParameters n _ = n + 1

-- Code-gen of the LValue of a function argument
codeGenLValueArg :: AnRExpr -> CodeGen Addr
codeGenLValueArg anRExpr = case unAnn anRExpr of
  RBrackets rexpr -> codeGenLValueArg rexpr
  LExpr anLExpr -> do
    let lexprType = getType anLExpr
    (addr, offset, ws, isRef) <- codeGenLExpr anLExpr
    temp <- newTemp
    case isArrayType lexprType of
      False -> case offset of
        Nothing -> case isRef of
          False -> pushBackTAC $ (AddressOf temp addr :)
          -- if addr is already a by-ref var, just copy its address
          True  -> pushBackTAC $ (Copy (RefType lexprType) temp addr :)
        Just o -> do
          let refType = RefType lexprType
          pushBackTAC $ (AssgnInfixOp refType TAddRefInt temp addr o :)
      True -> do
        let refType = coerceArrayToRef lexprType
        case offset of
          Nothing -> pushBackTAC $ (Copy refType temp addr :)
          Just o -> do
            pushBackTAC $ (AssgnInfixOp refType TAddRefInt temp addr o :)
    return temp
  TernaryOp gRExpr tRExpr eRExpr -> do
    sf <- newLabel LSetFalse
    es <- newLabel LEndSet
    let resType = RefType $ getType tRExpr
        f il = il {_BTrue=Nothing, _BFalse=Just sf}
    local f $ codeGenBoolGuards gRExpr
    res <- newTemp
    -- true case (then)
    tTemp <- codeGenLValueArg tRExpr
    pushBackTAC $ (++) [
      Copy resType res tTemp,
      TGoto es
      ] . attachLabel sf
    -- false case (else)
    eTemp <- codeGenLValueArg eRExpr
    pushBackTAC $ (Copy resType res eTemp :)
    pushBackTAC $ attachLabel es
    return res
  _ -> error "Internal Compiler Error. Please report the bug to the maintainers."

-- Code-gen of arithmetic operation
codeGenArithOp :: ArithOp -> AnRExpr -> AnRExpr -> CodeGen (Addr, [Addr])
codeGenArithOp o e1 e2 = do
  let e1Type = getType e1
  addr <- codeGenRExpr e1
  codeGenAssignArithOp o e1Type addr e2

-- Code-gen of arithmetic-op assignment: left operand already placed in an address
codeGenAssignArithOp :: ArithOp -> TypeSpec -> (Addr,[Addr]) -> AnRExpr -> CodeGen (Addr, [Addr])
codeGenAssignArithOp o e1Type (t1,ws1) e2 = do
  let e2Type = getType e2
      int = (BasicType BasicTypeInt)
  (t2, ws2) <- codeGenRExpr e2
  case (ws1, ws2) of
    ((_:w:_), [_]) -> do -- e1 is a RefType
      let op = arithOpToTInfixOp o e1Type e2Type
      temp <- newTemp
      pushBackTAC $ (AssgnInfixOp int (TMul int) temp t2 w :)
      temp1 <- newTemp
      pushBackTAC $ (AssgnInfixOp e1Type op temp1 t1 temp :)
      return (temp1, ws1)
    ([_], (_:w:_)) -> do -- e2 is a RefType
      let op = arithOpToTInfixOp o e1Type e2Type
      temp <- newTemp
      pushBackTAC $ (AssgnInfixOp int (TMul int) temp t1 w :)
      temp1 <- newTemp
      pushBackTAC $ (AssgnInfixOp e2Type op temp1 temp t2 :)
      return (temp1, ws2)
    ([_], [w]) -> do -- Two BasicTypes
      let op = arithOpToTInfixOp o e1Type e2Type
      res <- newTemp
      pushBackTAC $ (AssgnInfixOp e1Type op res t1 t2 :)
      return (res, [w])
    (_:_:_, _:_:_) -> do -- This is the case of (Ref - Ref)
      let op = arithOpToTInfixOp o e1Type e2Type
      res <- newTemp
      pushBackTAC $ (AssgnInfixOp int op res t1 t2 :)
      -- even if we have two ref types we return a BasicTypeInt,
      -- thus we propagate the width of an Int
      return (res, [TConst . Int $ sizeof int])

-- Code-gen of bitwise operation
codeGenBitOp :: BitOp -> AnRExpr -> AnRExpr -> CodeGen (Addr, [Addr])
codeGenBitOp o e1 e2 = do
  addr <- codeGenRExpr e1
  codeGenAssignBitOp o addr e2

-- Code-gen of bit-op assignment: left operand already placed in an address
codeGenAssignBitOp :: BitOp -> (Addr,[Addr]) -> AnRExpr -> CodeGen (Addr, [Addr])
codeGenAssignBitOp o (t1,_) e2 = do
  let assgnType = getType e2
      op = bitOpToTInfixOp o
  -- Bit-ops only permitted on BasicTypeInt,
  -- so it propagates the width of the basic type
  (t2, [w]) <- codeGenRExpr e2
  res <- newTemp
  pushBackTAC $ (AssgnInfixOp assgnType op res t1 t2 :)
  return (res, [w])

-- Converts from ArithOp to TInfixOp
arithOpToTInfixOp :: ArithOp -> TypeSpec -> TypeSpec -> TInfixOp
arithOpToTInfixOp Add (RefType _) (BasicType BasicTypeInt) = TAddRefInt
arithOpToTInfixOp Add (BasicType BasicTypeInt) (RefType _) = TAddIntRef
arithOpToTInfixOp Add l r = TAdd l
arithOpToTInfixOp Sub (RefType _) (BasicType BasicTypeInt) = TSubRefInt
arithOpToTInfixOp Sub (RefType _) (RefType _) = TSub (RefType UnTyped)
arithOpToTInfixOp Sub l r = TSub l
arithOpToTInfixOp Mul l r = TMul l
arithOpToTInfixOp Mod l r = TMod l
arithOpToTInfixOp Div l r = TDiv l
arithOpToTInfixOp Exp l r = TExp l

-- Converts from BitOp to TInfixOp
bitOpToTInfixOp :: BitOp -> TInfixOp
bitOpToTInfixOp BAnd   = TBAnd
bitOpToTInfixOp BOr    = TBOr
bitOpToTInfixOp BXor   = TBXor
bitOpToTInfixOp LShift = TLShift
bitOpToTInfixOp RShift = TRShift

-- Code-gen of Boolean guards with short-circuiting
-- Returns True if at least one TAC instruction has been generated:
-- useful to avoid attaching multiple labels to the same TAC instruction
codeGenBoolGuards :: AnRExpr -> CodeGen Bool
codeGenBoolGuards anRExpr = case unAnn anRExpr of
  Const _ (Bool b) -> case b of
    True -> do
      lab <- asks _BTrue
      when (isJust lab) $
        pushBackTAC $ (TGoto (fromJust lab) :)
      return $ isJust lab
    False -> do
      lab <- asks _BFalse
      when (isJust lab) $
        pushBackTAC $ (TGoto (fromJust lab) :)
      return $ isJust lab
  InfixOp op l r -> case op of
    BoolOp o -> codeGenBoolOp (unAnn o) l r
    RelOp  o -> codeGenRelOp (unAnn o) l r
    _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
  UnaryOp op expr -> case unAnn op of
    Not -> do
      let f :: IL -> IL
          f il = il { _BTrue = _BFalse il, _BFalse = _BTrue il}
      local f $ codeGenBoolGuards expr
    _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
  RBrackets rexpr -> codeGenBoolGuards rexpr
  FCall _ _ -> do
    -- Returns a BasicTypeBool because inside a bool guard
    (temp, [_]) <- codeGenRExpr anRExpr
    btrue <- asks _BTrue
    bfalse <- asks _BFalse
    case (btrue, bfalse) of
      (Nothing, Nothing) -> return ()
      (Just l , Nothing) -> pushBackTAC $ (GotoIf temp l :)
      (Nothing, Just l ) -> pushBackTAC $ (GotoIfFalse temp l :)
      (Just tt, Just ff) -> do
        pushBackTAC $ (GotoIf temp tt :)
        pushBackTAC $ (TGoto ff :)
    return True
  LExpr _ -> do
    -- Returns a BasicTypeBool because inside a bool guard
    (temp, [_]) <- codeGenRExpr anRExpr
    btrue <- asks _BTrue
    bfalse <- asks _BFalse
    case (btrue, bfalse) of
      (Nothing, Nothing) -> return ()
      (Just l , Nothing) -> pushBackTAC $ (GotoIf temp l :)
      (Nothing, Just l ) -> pushBackTAC $ (GotoIfFalse temp l :)
      (Just tt, Just ff) -> do
        pushBackTAC $ (GotoIf temp tt :)
        pushBackTAC $ (TGoto ff :)
    return True
  TernaryOp _ _ _ -> do
    (temp, [_]) <- codeGenRExpr anRExpr
    btrue <- asks _BTrue
    bfalse <- asks _BFalse
    case (btrue, bfalse) of
      (Nothing, Nothing) -> return ()
      (Just l , Nothing) -> pushBackTAC $ (GotoIf temp l :)
      (Nothing, Just l ) -> pushBackTAC $ (GotoIfFalse temp l :)
      (Just tt, Just ff) -> do
        pushBackTAC $ (GotoIf temp tt :)
        pushBackTAC $ (TGoto ff :)
    return True
  _ -> error "Internal Compiler Error. Please report the bug to the maintainers."

-- Code-gen of Boolean operations with short-circuiting
-- Returns True if at least one TAC instrucion has been generated
codeGenBoolOp :: BoolOp -> AnRExpr -> AnRExpr -> CodeGen Bool
codeGenBoolOp (And) b1 b2 = do
  let g :: Maybe Label -> CodeGen (Maybe Label)
      g Nothing = Just <$> newLabel LCmd
      g x = return x
  bFalse <- asks _BFalse
  b1False <- g bFalse
  let f :: IL -> IL
      f a = a { _BTrue = Nothing, _BFalse=b1False}
  hasGen <- local f (codeGenBoolGuards b1)
  hasGen <- (hasGen ||) <$> codeGenBoolGuards b2
  when (isNothing bFalse && hasGen) $
    pushBackTAC $ attachLabel $ fromJust b1False
  return hasGen
codeGenBoolOp (Or) b1 b2 = do
  let g :: Maybe Label -> CodeGen (Maybe Label)
      g Nothing = Just <$> newLabel LCmd
      g x = return x
  bTrue <- asks _BTrue
  b1True <- g bTrue
  let f :: IL -> IL
      f a = a { _BFalse=Nothing, _BTrue=b1True }
  hasGen <- local f (codeGenBoolGuards b1)
  hasGen <- (hasGen ||) <$> codeGenBoolGuards b2
  when (isNothing bTrue && hasGen) $
    pushBackTAC $ attachLabel $ fromJust b1True
  return hasGen

-- Code-gen of bool-op assignment with short-circuiting: left operand already placed in an address
codeGenAssignBoolOp :: BoolOp -> (Addr,[Addr]) -> AnRExpr -> CodeGen ()
codeGenAssignBoolOp (And) (temp,_) b2 = do
  let g :: Maybe Label -> CodeGen (Maybe Label)
      g Nothing = Just <$> newLabel LCmd
      g x = return x
  bFalse <- asks _BFalse
  b1False@(Just l) <- g bFalse
  pushBackTAC $ (GotoIfFalse temp l :)
  codeGenBoolGuards b2
  when (isNothing bFalse) $
    pushBackTAC $ attachLabel $ fromJust b1False
codeGenAssignBoolOp (Or) (temp,_) b2 = do
  let g :: Maybe Label -> CodeGen (Maybe Label)
      g Nothing = Just <$> newLabel LCmd
      g x = return x
  bTrue <- asks _BTrue
  b1True@(Just l) <- g bTrue
  pushBackTAC $ (GotoIf temp l :)
  codeGenBoolGuards b2
  when (isNothing bTrue) $
    pushBackTAC $ attachLabel $ fromJust b1True

-- Code-gen of relational operations
-- Returns True if at least one TAC instrucion has been generated
codeGenRelOp :: RelOp -> AnRExpr -> AnRExpr -> CodeGen Bool
codeGenRelOp o e1 e2 = do
  (tl, _) <- codeGenRExpr e1
  (tr, _) <- codeGenRExpr e2
  let opType = getType e1
      tRelOp = TRelOp o opType
  btrue  <- asks _BTrue
  bfalse <- asks _BFalse
  case opType of
    BasicType BasicTypeString -> do
      -- Here `o` is only Eq or Neq because other RelOps are not allowed on Strings
        let stringRef = coerceStringToRef opType
            isEq Eq = True
            isEq _ = False
        pushParam stringRef tl
        pushParam stringRef tr
        res <- newTemp
        -- Call to runtime routine are_strings_equal
        pushBackTAC $ (TFCall (BasicType BasicTypeBool) res (TName (Ident "are_strings_equal") (NoLoc "")) 2 :)
        case if isEq o then (btrue, bfalse) else (bfalse, btrue) of
          (Nothing, Nothing) -> return ()
          (Just l , Nothing) -> pushBackTAC $ (GotoIf res l :)
          (Nothing, Just l ) -> pushBackTAC $ (GotoIfFalse res l :)
          (Just tt, Just ff) -> do
            pushBackTAC $ (GotoIf res tt :)
            pushBackTAC $ (TGoto ff :)
    _ -> case (btrue, bfalse) of
      (Nothing, Nothing) -> return ()
      (Just l , Nothing) -> pushBackTAC (GotoRelOp tRelOp tl tr l :)
      (Nothing, Just l ) -> pushBackTAC $ (GotoRelOp (negateTRelOp tRelOp) tl tr l :)
      (Just tt, Just ff) -> do
        pushBackTAC (GotoRelOp tRelOp tl tr tt :)
        pushBackTAC (TGoto ff :)
  return True

negateRelOp :: RelOp -> RelOp
negateRelOp Eq = Neq
negateRelOp Neq = Eq
negateRelOp Lt = GtE
negateRelOp LtE = Gt
negateRelOp Gt = LtE
negateRelOp GtE = Lt

negateTRelOp :: TRelOp -> TRelOp
negateTRelOp (TRelOp o t) = flip TRelOp t . negateRelOp $ o

-- Code-gen of function arguments
generateFuncArgument :: ModTypeSpec -> AnRExpr -> CodeGen ()
generateFuncArgument (ModTypeSpec m t) anRExpr = do
  let rType = getType anRExpr
  case m of
    Value -> case isArrayType t of
      False -> do
        (temp, _) <- codeGenRExpr anRExpr
        pushBackTAC $ (Param rType temp :)
      True  -> do
        let arrayBasicType = coerceStringToRef $ getArrayBasicType t
            offsets = offsetsOfStaticArray t
        temp <- codeGenLValueArg anRExpr -- Base address of array
        mapM_ (pushArrayElem temp arrayBasicType) offsets
    Constant -> case t of
      -- Compound types are passed by reference
      ArrayType _ _ -> do
        temp <- codeGenLValueArg anRExpr
        pushBackTAC $ (Param (RefType rType) temp :)
      BasicType BasicTypeString -> do
        temp <- codeGenLValueArg anRExpr
        pushBackTAC $ (Param (RefType rType) temp :)
      -- Simple types are passed by value
      _ -> do
        (temp, _) <- codeGenRExpr anRExpr
        pushBackTAC $ (Param rType temp :)
    _ -> do
      temp <- codeGenLValueArg anRExpr
      pushBackTAC $ (Param (RefType rType) temp :)

-- Calculates the number of elements in an Array
elementsOfStaticArray :: TypeSpec -> Int
elementsOfStaticArray (ArrayType (A _ (Const _ (Int i))) t) = i * (elementsOfStaticArray t)
elementsOfStaticArray _ = 1

-- Calculates the offset of each element of the Array
offsetsOfStaticArray :: TypeSpec -> [Addr]
offsetsOfStaticArray t = map (TConst . Int ) [ elemSize*x | x <- [0..(numElems-1)]]
  where
    numElems = elementsOfStaticArray t
    arrayBasicType = getArrayBasicType t
    elemSize = sizeof arrayBasicType

-- Retrieves the value of Array[offset]
retrieveArrayElem :: Addr ->      -- Array base address
                     TypeSpec ->  -- Element type
                     Addr ->      -- Element offset
                     CodeGen Addr -- Element
retrieveArrayElem addr t offset = do
  temp <- newTemp
  pushBackTAC $ (RIndexedCopy t temp addr offset :)
  return temp

-- Pushes a function actual parameter
pushParam :: TypeSpec -> Addr -> CodeGen ()
pushParam t addr = pushBackTAC $ (Param t addr :)

pushArrayElem :: Addr -> TypeSpec -> Addr -> CodeGen ()
pushArrayElem addr t offset = do
  temp <- retrieveArrayElem addr t offset
  pushParam t temp

-- Code-gen of a LExpr
codeGenLExpr :: AnLExpr -> CodeGen (Addr, Maybe Addr, [Addr], Bool)
codeGenLExpr anlexpr = case unAnn anlexpr of
  VarIdent anId -> do
    let id = unAnn anId
    envs <- use symtable
    case lookup vars envs id of
      Nothing -> error "Internal Compiler Error. Please report the bug to the maintainers."
      Just (t, l, m, widths, _) -> do
        let loc = srcSpanStartLoc l
        case m of
          Value -> return (TName id loc, Nothing, widths, False)
          Constant -> do
            let isRef = case t of
                  BasicType BasicTypeString -> True
                  ArrayType _ _ -> True
                  _ -> False
            return (TName id loc, Nothing, widths, isRef)
          Reference -> return (TName id loc, Nothing, widths, True)
          _ -> return (getVarLocalCopy id loc, Nothing, widths, False)
  ArrayElem anLExpr anRExpr -> do
    lexpr <- codeGenLExpr anLExpr
    let int = BasicType BasicTypeInt
    case lexpr of
      (addr, Nothing, _:w1:ws, isRef)-> do
        (i1, [_]) <- codeGenRExpr anRExpr
        temp <- emitMulOperation int i1 w1
        return (addr, Just temp, w1:ws, isRef)
      (addr, Just offset, _:wk:ws, isRef) -> do
        (ik, [_]) <- codeGenRExpr anRExpr
        temp1 <- emitMulOperation int ik wk
        newOffset <- emitAddOperation int offset temp1
        return (addr, Just newOffset, wk:ws, isRef)
  LBrackets anLExpr -> codeGenLExpr anLExpr
  PrePostIncDecr prepost incdec anLExpr -> do
    lexpr <- codeGenLExpr anLExpr
    let opType = getType anLExpr
    case lexpr of
      (addr, Just offset, w:ws, isRef) -> do
        let unit = case opType of
              BasicType BasicTypeInt -> TConst $ Int 1
              BasicType BasicTypeFloat -> TConst $ Float 1.0
              RefType _ -> head ws
              _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
        case prepost of
          Post -> do
            temp <- newTemp
            pushBackTAC $ (RIndexedCopy opType temp addr offset :)
            temp1 <- newTemp
            pushBackTAC $ (AssgnInfixOp opType (incDecrToTInfixOp incdec opType) temp1 temp unit :)
            pushBackTAC $ (LIndexedCopy opType addr temp1 offset :)
            return (temp, Nothing, w:ws, False)
          Pre -> do
            temp <- newTemp
            pushBackTAC $ (RIndexedCopy opType temp addr offset :)
            pushBackTAC $ (AssgnInfixOp opType (incDecrToTInfixOp incdec opType) temp temp unit :)
            pushBackTAC $ (LIndexedCopy opType addr temp offset :)
            return (temp, Nothing, w:ws, False)
      (addr, Nothing, w:ws, isRef) -> do
        let unit = case opType of
              BasicType BasicTypeInt -> TConst $ Int 1
              BasicType BasicTypeFloat -> TConst $ Float 1.0
              RefType _ -> head ws
              _ -> error "Internal Compiler Error. Please report the bug to the maintainers."
        case prepost of
          Post -> do
            temp <- newTemp
            case isRef of
              True  -> do
                pushBackTAC $ (RDeref opType temp addr :)
                temp1 <- newTemp
                pushBackTAC $ (AssgnInfixOp opType (incDecrToTInfixOp incdec opType) temp1 temp unit :)
                pushBackTAC $ (LDeref opType addr temp1:)
              False -> do
                pushBackTAC $ (Copy opType temp addr :)
                pushBackTAC $ (AssgnInfixOp opType (incDecrToTInfixOp incdec opType) addr addr unit :)
            return (temp, Nothing, w:ws, False)
          Pre -> do
            case isRef of
              True -> do
                temp <- newTemp
                pushBackTAC $ (RDeref opType temp addr :)
                pushBackTAC $ (AssgnInfixOp opType (incDecrToTInfixOp incdec opType) temp temp unit :)
                pushBackTAC $ (LDeref opType addr temp :)
              False -> do
                pushBackTAC $ (AssgnInfixOp opType (incDecrToTInfixOp incdec opType) addr addr unit :)
            return (addr, Nothing, w:ws, isRef)
  Deref anRExpr -> do
    (rexpr, ws) <- codeGenRExpr anRExpr
    -- We postpone the dereferencing because here we don't know
    -- if it is a right-deref or a left-deref: we achieve this by setting
    -- the field isRef to true. The widths refer to the referred type, so
    -- we drop one width.
    return (rexpr, Nothing, drop 1 ws, True)

incDecrToTInfixOp :: IncDecr -> TypeSpec -> TInfixOp
incDecrToTInfixOp Inc (RefType _) = TAddRefInt
incDecrToTInfixOp Decr (RefType _) = TSubRefInt
incDecrToTInfixOp Inc t  = TAdd t
incDecrToTInfixOp Decr t = TSub t

-- If the addresses are constants, the result is computed at compile-time
emitAddOperation :: TypeSpec -> Addr -> Addr -> CodeGen Addr
emitAddOperation t@(BasicType BasicTypeInt) (TConst (Int i)) (TConst (Int j)) = return . TConst . Int $ i + j
emitAddOperation t a1 a2 = do
  temp <- newTemp
  pushBackTAC $
    (AssgnInfixOp t (TAdd t) temp a1 a2 :)
  return temp

-- If the addresses are constants, the result is computed at compile-time
emitMulOperation :: TypeSpec -> Addr -> Addr -> CodeGen Addr
emitMulOperation t@(BasicType BasicTypeInt) (TConst (Int i)) (TConst (Int j)) = return . TConst . Int $ i * j
emitMulOperation t a1 a2 = do
  temp <- newTemp
  pushBackTAC $
    (AssgnInfixOp t (TMul t) temp a1 a2 :)
  return temp

-- Generates the code of a list of Stmt:
-- for each Stmt it generates the code and attaches a label to the next one only if necessary
codeGenLabeledStmts :: [AnStmt] -> CodeGen ()
codeGenLabeledStmts (stmt@(A _ (Decl (A _ (Dvar _ _ Nothing)))):xs) = do
  codeGenStmt stmt
  codeGenLabeledStmts xs
codeGenLabeledStmts (stmt@(A _ (Decl (A _ (Dfun _ _ _ _)))):xs) = do
  codeGenStmt stmt
  codeGenLabeledStmts xs
codeGenLabeledStmts (x:xs@(y:_)) = do
  guardLabel <- justTheGuardLabel x
  case unAnn y of
    Loop stmt -> do
      loopLabel <- Just <$> newLabel LBody
      local (set sNext loopLabel . set lGuard guardLabel) $ codeGenStmt x
      local (set lGuard loopLabel) $ codeGenLabeledStmts xs
    While _ _ -> do
      whileGuardLabel <- Just <$> newLabel LGuard
      local (set sNext whileGuardLabel . set lGuard guardLabel) $ codeGenStmt x
      local (set lGuard whileGuardLabel) $ codeGenLabeledStmts xs
    Until _ _ -> do
      untilGuardLabel <- Just <$> newLabel LGuard
      local (set sNext untilGuardLabel . set lGuard guardLabel) $ codeGenStmt x
      local (set lGuard untilGuardLabel) $ codeGenLabeledStmts xs
    DoWhile _ _ -> do
      doWhileBodyLabel <- Just <$> newLabel LBody
      local (set sNext doWhileBodyLabel . set lGuard guardLabel) $ codeGenStmt x
      local (set lGuard doWhileBodyLabel) $ codeGenLabeledStmts xs
    _ -> do
      cmdLabel <- newLabel LCmd
      local (set sNext (Just cmdLabel) . set lGuard guardLabel) $ codeGenStmt x
      pushBackTAC $ attachLabel cmdLabel
      codeGenLabeledStmts xs
codeGenLabeledStmts xs = do
  snext <- view sNext
  case xs of
    [x] -> do
      guardLabel <- justTheGuardLabel x
      local (set sNext snext . set lGuard guardLabel) $ codeGenStmt x
    [] -> return ()

-- Auxiliary function of codeGenLabeledStmts
-- If lGuard is Just, returns lGuard
-- If lGuard is Nothing, returns Just a new label
justTheGuardLabel :: AnStmt -> CodeGen (Maybe Label)
justTheGuardLabel anstmt = do
  let newGuardLabel = case unAnn anstmt of
        Loop _ -> newLabel LBody >>= return . Just
        While _ _ -> newLabel LGuard >>= return . Just
        Until _ _ -> newLabel LGuard >>= return . Just
        DoWhile _ _ -> newLabel LBody >>= return . Just
        _ -> return Nothing
  guardLabel <- view lGuard
  maybe newGuardLabel (return . Just) guardLabel

codeGenStmts :: [AnStmt] -> CodeGen ()
codeGenStmts stmts = do
  oldVars <- gets $ _vars . head . _symtable
  mapM_ accumulateFunDecl stmts
  state <- get
  let newEnv@(x:xs) = _symtable state
  put state{_symtable=x{_vars=oldVars}:xs}
  codeGenLabeledStmts stmts

-- Code-gen entry point
codeGenProgram :: [AnStmt] -> CodeGen ()
codeGenProgram stmts' = do
  let isVarDecl (A _ (Decl (A _ (Dvar _ _ _)))) = True
      isVarDecl _ = False
      (vardecls, fundecls) = List.partition isVarDecl stmts'
      int = BasicType BasicTypeInt
  codeGenStmts vardecls
  temp <- newTemp
  pushBackTAC $ (TFCall int temp (TName (Ident "main") (NoLoc "")) 0 :)
  pushBackTAC $ (Param int temp :)
  pushBackTAC $ (TPCall (TName (Ident "exit") (NoLoc "")) 1 :)
  initTAC <- tac <<.= id
  codeGenStmts fundecls
  tac %= (initTAC .)
  sectionData <- use staticData
  pushBackTAC $ (DataSection (reverse sectionData) :)

-- The function isNoOp* return true if no TAC instruction is generated

isNoOpStmt :: AnStmt -> Bool
isNoOpStmt anStmt = case unAnn anStmt of
  RExprStmt _ -> False
  Assgn _ _ _ -> False
  Decl anDecl -> case unAnn anDecl of
    Dvar _ _ (Just _) -> False
    _ -> True
  Loop _ -> False
  While _ _ -> False
  Until _ _ -> False
  DoWhile g b -> isNoOpUlGuard g && isNoOpBlock b
  For _ _ _ -> False
  Block stmts -> isNoOpBlock stmts
  IfStmt anIf -> isNoOpIfStmt anIf
  UnlessStmt anUl -> isNoOpUlStmt anUl
  Break -> False
  Continue -> False
  RetExp _ -> False
  TryCatch _ _ -> False

isNoOpBlock :: [AnStmt] -> Bool
isNoOpBlock = all isNoOpStmt

isNoOpIfGuard :: AnRExpr -> Bool
isNoOpIfGuard anRExpr = case unAnn anRExpr of
  Const _ (Bool True) -> True
  _ -> False

isNoOpIfStmt :: AnIfStmt -> Bool
isNoOpIfStmt anIf = case unAnn anIf of
  If guard body maybeElse -> isNoOpMaybeIfStmt maybeElse &&
                             isNoOpBlock body    &&
                             isNoOpIfGuard guard
  ElseIf guard body maybeElse -> isNoOpMaybeIfStmt maybeElse &&
                                 isNoOpBlock body    &&
                                 isNoOpIfGuard guard
  IElse body -> isNoOpBlock body

isNoOpMaybeIfStmt :: Maybe AnIfStmt -> Bool
isNoOpMaybeIfStmt = maybe True isNoOpIfStmt

isNoOpUlGuard :: AnRExpr -> Bool
isNoOpUlGuard anRExpr = case unAnn anRExpr of
  Const _ (Bool False) -> True
  _ -> False

isNoOpUlStmt :: AnUnlessStmt -> Bool
isNoOpUlStmt anIf = case unAnn anIf of
  Unless guard body maybeElse -> isNoOpMaybeUlStmt maybeElse &&
                                 isNoOpBlock body    &&
                                 isNoOpUlGuard guard
  UElse body -> isNoOpBlock body

isNoOpMaybeUlStmt :: Maybe AnUnlessStmt -> Bool
isNoOpMaybeUlStmt = maybe True isNoOpUlStmt

-- Accumulates function declarations in the local environment
-- Call at the entrance of every block
accumulateFunDecl :: AnStmt -> CodeGen ()
accumulateFunDecl (A _ (Decl anDecl)) = case unAnn anDecl of
  Dfun tt@(FunType _ rt) anId _ _ -> do
    let id = unAnn anId
    ws <- computeWidths rt
    insert (symtable.localenv.funs) id (tt, getLoc anId, ws)
  Dvar tt anId maybeRExpr -> do
    let id = unAnn anId
        loc = getLoc anId
    ws <- computeWidths tt
    insert (symtable.localenv.vars) id (tt, loc, Value, ws, isJust maybeRExpr)
  _ -> return ()
accumulateFunDecl _ = return ()

-- Computes the widths of the type
computeWidths :: TypeSpec -> CodeGen [Addr]
computeWidths t@(BasicType _) = return [TConst . Int $ sizeof t]
computeWidths (Unit) = return [TConst $ Int 0]
computeWidths (ArrayType d t) = do
  ws@(w:_) <- computeWidths t
  case (unAnn d, w) of
    (Const _ (Int i), TConst (Int j)) ->
      -- Compute widths at compile-time
      return $ (TConst . Int $ i*j) : ws
    _ -> do
      let int = BasicType BasicTypeInt
      (temp, [_]) <- codeGenRExpr d
      res <- newTemp
      pushBackTAC $ (AssgnInfixOp int (TMul int) res temp w :)
      return $ res:ws
computeWidths ref@(RefType t) = do
  ws <- computeWidths t
  return $ (TConst . Int $ sizeof ref) : ws

-- Parameters: base address, current index, rexpr
-- Returns: new index
codeGenArrayLit :: Addr -> Int -> AnRExpr -> CodeGen Int
codeGenArrayLit base index anRExpr = do
  let rType = coerceStringToRef $ getType anRExpr
      getArrayLit (A _ (Const _ (Array a))) = a
  case rType of
    ArrayType _ _ -> do
      newIndex <- foldlM (codeGenArrayLit base) index $ getArrayLit anRExpr
      return newIndex
    _ -> do
      let offset = index * (sizeof rType)
      (temp, _) <- codeGenRExpr anRExpr
      pushBackTAC $ (LIndexedCopy rType base temp (TConst $ Int offset) :)
      return $ index + 1

codeGenIfStmt :: IfStmt -> CodeGen ()
codeGenIfStmt (If anRExpr body maybeElse)
  | isNoOpMaybeIfStmt maybeElse = do
      snext <- view sNext
      let f :: IL -> IL
          f = set bTrue Nothing
            . set bFalse snext
      local f $ codeGenBoolGuards anRExpr
      newBlockLocalEnv
      codeGenStmts body
      exitBlockLocalEnv
  | otherwise = do
      snext <- view sNext
      elseLabel <- newLabel LElse
      let f :: IL -> IL
          f = set bTrue Nothing
            . set bFalse (Just elseLabel)
      local f $ codeGenBoolGuards anRExpr
      newBlockLocalEnv
      codeGenStmts body
      exitBlockLocalEnv
      pushBackTAC $ (TGoto (fromJust snext) :)
      pushBackTAC $ attachLabel elseLabel
      codeGenIfStmt (unAnn $ fromJust maybeElse)
codeGenIfStmt (ElseIf anRExpr body maybeElse)
  | isNoOpMaybeIfStmt maybeElse = do
      snext <- view sNext
      let f :: IL -> IL
          f = set bTrue Nothing
            . set bFalse snext
      local f $ codeGenBoolGuards anRExpr
      newBlockLocalEnv
      codeGenStmts body
      exitBlockLocalEnv
  | otherwise = do
      snext <- view sNext
      elseLabel <- newLabel LElse
      let f :: IL -> IL
          f = set bTrue Nothing
            . set bFalse (Just elseLabel)
      local f $ codeGenBoolGuards anRExpr
      newBlockLocalEnv
      codeGenStmts body
      exitBlockLocalEnv
      pushBackTAC $ (TGoto (fromJust snext) :)
      pushBackTAC $ attachLabel elseLabel
      codeGenIfStmt (unAnn $ fromJust maybeElse)
codeGenIfStmt (IElse body) = do
  newBlockLocalEnv
  codeGenStmts body
  exitBlockLocalEnv

codeGenUlStmt :: UnlessStmt -> CodeGen ()
codeGenUlStmt (Unless anRExpr body maybeElse)
  | isNoOpMaybeUlStmt maybeElse = do
      snext <- view sNext
      let f :: IL -> IL
          f = set bFalse Nothing
            . set bTrue snext
      local f $ codeGenBoolGuards anRExpr
      newBlockLocalEnv
      codeGenStmts body
      exitBlockLocalEnv
  | otherwise = do
      snext <- view sNext
      elseLabel <- newLabel LElse
      let f :: IL -> IL
          f = set bFalse Nothing
            . set bTrue (Just elseLabel)
      local f $ codeGenBoolGuards anRExpr
      newBlockLocalEnv
      codeGenStmts body
      exitBlockLocalEnv
      pushBackTAC $ (TGoto (fromJust snext) :)
      pushBackTAC $ attachLabel elseLabel
      codeGenUlStmt (unAnn $ fromJust maybeElse)
codeGenUlStmt (UElse body) = do
  newBlockLocalEnv
  codeGenStmts body
  exitBlockLocalEnv

codeGenStmt :: AnStmt -> CodeGen ()
codeGenStmt anStmt = do
 let ann = getAnn anStmt
 case unAnn anStmt of
  IfStmt anIf -> codeGenIfStmt (unAnn anIf)
  UnlessStmt anUl -> codeGenUlStmt (unAnn anUl)
  RExprStmt anRExpr -> do
    (temp, _) <- codeGenRExpr anRExpr
    case temp of
      TConst _ -> do
        res <- newTemp
        pushBackTAC $ (Copy (getType anRExpr) res temp :)
      _ -> return ()
  Decl anDecl -> case unAnn anDecl of
    Dvar tt anId@(A e id) maybeRExpr -> do
      let loc = getLoc e
      ws <- computeWidths tt
      insert (symtable.localenv.vars) id (tt, loc, Value, ws, isJust maybeRExpr)
      when (isJust maybeRExpr) $ do
        let anRExpr = fromJust maybeRExpr
            rtype = getType anRExpr
            addr = TName id (srcSpanStartLoc loc)
        case rtype of
          ArrayType _ _ -> do
            codeGenArrayLit addr 0 anRExpr
            return ()
          BasicType BasicTypeString -> do
            (temp, _) <- codeGenRExpr anRExpr
            pushBackTAC $ (Copy (RefType rtype) addr temp :)
          _ -> do
            (temp, _) <- codeGenRExpr anRExpr
            pushBackTAC $ (Copy rtype addr temp :)
    Dfun (FunType _ rt) anId@(A _ (Ident s)) fps stmts -> do
      let loc = srcSpanStartLoc $ getLoc anId
      currentTac <- tac <<.= id
      newFunLocalEnv
      pushBackTAC $ attachLabel $ LFun (unAnn anId) loc
      pushBackTAC $ (LineComment "preamble of function" :)
      mapM_ accumulateFormalParameter fps
      pushBackTAC $ (LineComment "code of function" :)
      endfun <- newLabel LEndFun
      local (set sNext $ Just endfun) $ codeGenStmts stmts
      when (isUnit rt) $ do
        pushBackTAC $ (LineComment "postamble of function" :)
      pushBackTAC $ attachLabel endfun
      case rt of
        Unit -> do
          generatePostamble
          pushBackTAC $ (Return rt Nothing :)
        _ -> pushBackTAC $ (LineComment ("error \"control of function " ++ s ++ " should not reach this point\"") :)
      exitFunLocalEnv
      pushBackTAC currentTac
      return ()
  Assgn anLExpr assignmentOp anRExpr -> do
    (addr,offset,width,isRef) <- codeGenLExpr anLExpr
    let rtype = getType anRExpr
        ltype = coerceStringToRef $ getType anLExpr
    case assignmentOp of
      Assign -> do
        (temp, _) <- codeGenRExpr anRExpr
        case offset of
          Nothing -> case isRef of
            True  -> pushBackTAC $ (LDeref ltype addr temp :)
            False -> pushBackTAC $ (Copy ltype addr temp :)
          Just o  -> pushBackTAC $ (LIndexedCopy ltype addr temp o :)
      AssgnArith anArithOp -> do
        (temp, ws) <- fromLExprToRExpr ltype (addr,offset,width,isRef)
        (res, _) <- codeGenAssignArithOp (unAnn anArithOp) ltype (temp,ws) anRExpr
        case offset of
          Nothing -> case isRef of
            True  -> pushBackTAC $ (LDeref ltype addr res :)
            False -> pushBackTAC $ (Copy ltype addr res :)
          Just o -> pushBackTAC $ (LIndexedCopy ltype addr res o :)
      AssgnBit anBitOp -> do
        (temp, ws) <- fromLExprToRExpr ltype (addr,offset,width,isRef)
        (res, _) <- codeGenAssignBitOp (unAnn anBitOp) (temp,ws) anRExpr
        case offset of
          Nothing -> case isRef of
            True  -> pushBackTAC $ (LDeref ltype addr res :)
            False -> pushBackTAC $ (Copy ltype addr res :)
          Just o  -> pushBackTAC $ (LIndexedCopy ltype addr res o :)
      AssgnLogic anBoolOp -> do
        sf <- newLabel LSetFalse
        es <- newLabel LEndSet
        let basicBool = BasicType BasicTypeBool
            f il = il {_BTrue=Nothing, _BFalse=Just sf}
        (temp, ws) <- fromLExprToRExpr ltype (addr,offset,width,isRef)
        local f $ codeGenAssignBoolOp (unAnn anBoolOp) (temp,ws) anRExpr
        res <- newTemp
        pushBackTAC $ (++) [
              Copy basicBool res . TConst . Bool $ True,
              TGoto es,
              Label sf .
              Copy basicBool res . TConst . Bool $ False
            ] . attachLabel es
        case offset of
          Nothing -> case isRef of
            True  -> pushBackTAC $ (LDeref ltype addr res :)
            False -> pushBackTAC $ (Copy ltype addr res :)
          Just o -> pushBackTAC $ (LIndexedCopy ltype addr res o :)
  RetExp maybeRExpr -> do
    maybeTemp <- mapM codeGenRExpr maybeRExpr
    let retType = maybe Unit (coerceStringToRef . getType) maybeRExpr
        retTemp = maybe Nothing (Just . fst) maybeTemp
    pushBackTAC $ (LineComment "(premature) postamble of function" :)
    generatePostamble
    pushBackTAC $ (Return retType retTemp :)
  While anRExpr stmts -> do
    condLabel <- view lGuard
    snext <- view sNext
    pushBackTAC $ attachLabel . fromJust $ condLabel
    let f :: IL -> IL
        f = set bTrue Nothing
          . set bFalse snext
    local f $ codeGenBoolGuards anRExpr
    bodyLabel <- newLabel LBody
    pushBackTAC $ attachLabel bodyLabel
    let g :: IL -> IL
        g = set lBreak snext -- Sets break jumping label
          . set lContinue condLabel -- Sets continue jumping label
          . set lGuard Nothing
          . set sNext condLabel
    newBlockLocalEnv
    local g $ codeGenStmts stmts
    pushBackTAC $ (TGoto (fromJust condLabel) :)
    exitBlockLocalEnv
  DoWhile anRExpr stmts -> do
    bodyLabel <- view lGuard
    snext <- view sNext
    pushBackTAC $ attachLabel (fromJust bodyLabel)
    condLabel <- Just <$> newLabel LGuard
    let g :: IL -> IL
        g = set lBreak snext -- Sets break jumping label
          . set lContinue condLabel -- Sets continue jumping label
          . set lGuard Nothing
          . set sNext condLabel
    newBlockLocalEnv
    local g $ codeGenStmts stmts
    exitBlockLocalEnv
    pushBackTAC $ attachLabel . fromJust $ condLabel
    let f :: IL -> IL
        f = set bTrue bodyLabel
          . set bFalse Nothing
    local f $ codeGenBoolGuards anRExpr
    return ()
  Until anRExpr stmts -> do
    condLabel <- view lGuard
    snext <- view sNext
    pushBackTAC $ attachLabel . fromJust $ condLabel
    let f :: IL -> IL
        f = set bTrue snext
          . set bFalse Nothing
    local f $ codeGenBoolGuards anRExpr
    bodyLabel <- newLabel LBody
    pushBackTAC $ attachLabel bodyLabel
    let g :: IL -> IL
        g = set lBreak snext -- Sets break jumping label
          . set lContinue condLabel -- Sets continue jumping label
          . set lGuard Nothing
          . set sNext condLabel
    newBlockLocalEnv
    local g $ codeGenStmts stmts
    pushBackTAC $ (TGoto (fromJust condLabel) :)
    exitBlockLocalEnv
  Block stmts -> do
    newBlockLocalEnv
    codeGenStmts stmts
    exitBlockLocalEnv
  Loop stmt -> do
    bodyLabel <- view lGuard
    snext <- view sNext
    pushBackTAC $ attachLabel (fromJust bodyLabel)
    let g :: IL -> IL
        g = set lBreak snext
          . set lContinue bodyLabel -- Sets continue jumping label
          . set lGuard Nothing
          . set sNext bodyLabel
    local g $ codeGenStmt (A ann stmt)
    pushBackTAC $ (TGoto (fromJust bodyLabel) :)
  Break -> do
    nextLabel <- view lBreak
    pushBackTAC $ (TGoto (fromJust nextLabel) :)
  Continue -> do
    guardLabel <- view lContinue
    pushBackTAC $ (TGoto (fromJust guardLabel) :)
  For anIdent range@(Range l r) stmts -> do
    let counterLExpr = A intAnn (VarIdent anIdent)
        counterRExpr = A intAnn (LExpr counterLExpr)
        upperBoundGuard = A boolAnn (InfixOp (RelOp (A intAnn LtE)) counterRExpr r)
        counterIncLExpr = A intAnn (PrePostIncDecr Pre Inc counterLExpr)
        counterIncRExpr = A intAnn (LExpr counterIncLExpr)
        counterIncStmt = A noAnn (RExprStmt counterIncRExpr)
        counterInit = A noAnn (Assgn counterLExpr Assign l)
        whileStmt =  A noAnn (While upperBoundGuard [A noAnn (Block stmts), counterIncStmt])
        newStmt = A noAnn (Block [counterInit, whileStmt])
    codeGenStmt newStmt
  TryCatch try catch -> do
    catchLabel <- newLabel LCatch
    newBlockLocalEnv
    pushBackTAC $ (OnExceptionGoto catchLabel :)
    local (set lCatch $ Just catchLabel) $ codeGenStmts try
    snext <- view sNext
    pushBackTAC $ (TGoto (fromJust snext) :)
    exitBlockLocalEnv
    newBlockLocalEnv
    pushBackTAC $ attachLabel catchLabel
    codeGenStmts catch
    lcatch <- view lCatch
    if (isNothing lcatch)
    then pushBackTAC $ (EndCatch :)
    else pushBackTAC $ (OnExceptionGoto (fromJust lcatch) :)
    exitBlockLocalEnv

-- Returns the RValue of a LExpr
fromLExprToRExpr :: TypeSpec -> (Addr, Maybe Addr, [Addr], Bool) -> CodeGen (Addr,[Addr])
fromLExprToRExpr lexprType (addr, offset, ws, isRef)  = do
  case isArrayType lexprType of -- An array is not assignable, so this must be False
    False -> do
      temp <- newTemp
      case offset of
        Just o -> pushBackTAC $ (RIndexedCopy lexprType temp addr o :)
        Nothing -> case isRef of
          True  -> pushBackTAC $ (RDeref lexprType temp addr :)
          False -> pushBackTAC $ (Copy lexprType temp addr :)
      return (temp, ws)
    True -> error "Internal Compiler Error. Please report the bug to the maintainers."

-- Accumulates function parameters in the function local environment
-- and generates the preamble of the function
-- Call at the entrance of every function
accumulateFormalParameter :: AnFormalParameter -> CodeGen ()
accumulateFormalParameter (A _ (FormalParameter m t anId)) = do
  let loc = getLoc anId
      id  = unAnn anId
      isInit Result = False
      isInit _ = True
  ws <- computeWidths t
  insert (symtable.localenv.vars) id (t, loc, m, ws, isInit m)
  case m of
    ValueResult -> do
      let startLoc = srcSpanStartLoc loc
          localVar = getVarLocalCopy id startLoc
          outerVar = TName id startLoc
      case isArrayType t of
        True  -> do
          let offsets = offsetsOfStaticArray t
              arrayBasicType = coerceStringToRef $ getArrayBasicType t
          mapM_ (copyArrayElemFromTo outerVar localVar arrayBasicType) offsets
        False -> pushBackTAC $ (RDeref t localVar outerVar :)
    _ -> return ()

-- Gets the local copy of a variable
getVarLocalCopy :: Ident -> Loc -> Addr
getVarLocalCopy (Ident s) l = flip TName l . Ident $ newIdent
  where
    newIdent = s ++ "$local$copy"

-- Generates the postamble of a function
generatePostamble :: CodeGen ()
generatePostamble = do
  localVars <- use $ symtable.localenv.vars
  let byResVars = getByResVariables localVars
  mapM_ codeGenByResPost byResVars

-- Gets parameters with Result and Value-Result modalities
getByResVariables :: HashMap Ident (TypeSpec, SrcSpan, Modality, [Width], IsInit) ->
                     [(Ident, (TypeSpec, SrcSpan, Modality, [Width], IsInit))]
getByResVariables hm = List.sortOn s . Hash.toList . Hash.filter f $ hm
  where
    f :: (TypeSpec, SrcSpan, Modality, [Width], IsInit) -> Bool
    f (_, _, m, _, _) = case m of
      ValueResult -> True
      Result -> True
      _ -> False
    s :: (Ident,(TypeSpec, SrcSpan, Modality, [Width], IsInit)) -> SrcSpan
    s (_,(_,l,_,_,_)) = l

-- Auxiliary function of generatePostamble for Result and Value-Result parameters
codeGenByResPost :: (Ident, (TypeSpec, SrcSpan, Modality, [Width], IsInit)) -> CodeGen ()
codeGenByResPost (id, (t, srcSpan, _, _, _)) = do
  let loc = srcSpanStartLoc srcSpan
  case t of
    ArrayType _ _ -> do
      let offsets = offsetsOfStaticArray t
          localArray = getVarLocalCopy id loc
          outerArray = TName id loc
          arrayBasicType = coerceStringToRef $ getArrayBasicType t
      mapM_ (copyArrayElemFromTo localArray outerArray arrayBasicType) offsets
    _ -> pushBackTAC $ (LDeref (coerceStringToRef t) (TName id loc) (getVarLocalCopy id loc) :)

-- Copies an element from an Array to another
copyArrayElemFromTo :: Addr ->     -- Base address of source array
                       Addr ->     -- Base address of destination array
                       TypeSpec -> -- Element Type
                       Addr ->     -- Offset of element
                       CodeGen ()
copyArrayElemFromTo s d t o = do
  temp <- retrieveArrayElem s t o
  pushBackTAC $ (LIndexedCopy t d temp o :)
