
module Compiler.Errors (
  ErrorType(..),
  LcErrorType,
  StaticError(..),
  printErrors
  ) where

import Types
import Annotations
import SourceLocation hiding (getLoc)
import AST
import PrettyPrinting

import Data.List
import qualified Data.Vector as V


data ErrorType
  = StaticError StaticError
  deriving (Eq, Show)

instance PrPrint ErrorType where
  prettyDoc = pd
    where
      pd (StaticError e) = prettyDoc e

type LcErrorType = Located ErrorType

data StaticError
  = InvalidInfixOp InfixOp TypeSpec TypeSpec
  | InvalidUnaryOp UnaryOp TypeSpec
  | IncompatibleTypes TypeSpec TypeSpec
  | UnexpectedType TypeSpec TypeSpec
  | UnexpectedArgType TypeSpec TypeSpec
  | UnexpectedRetType TypeSpec TypeSpec
  | UnexpectedArgVC
  | ExpectedArrayType TypeSpec
  | InvalidDerefOp TypeSpec
  | InvalidRefVC
  | UndeclaredVarIdent Ident
  | UnInitVarIdent Ident
  | VarIdentNotModif Ident
  | UndeclaredFunIdent Ident
  | InvalidIncDecrVC IncDecr
  | InvalidIncDecrOp IncDecr TypeSpec
  | WrongArgNum Int Int
  | ExpectedArrayLit
  | UnexpectedArrayLit
  | WrongScalarInit
  | WrongScalarAssgn
  | InvalidAssgnVC
  | InvalidAssgnArray
  | RedeclaredVar Ident SrcSpan
  | RedeclaredParam Ident SrcSpan
  | RedeclaredFun Ident SrcSpan
  | InvalidAssgnOp InfixOp TypeSpec TypeSpec
  | InvalidGuard TypeSpec
  | InvalidRangeOp
  | InvalidJumpStmt Stmt
  | ResNotInit [Ident] Ident
  | CaptureOfResParam Ident Ident
  | ReturnNotPresent Ident
  | RetValFromUnit
  | RetUnitFromVal TypeSpec
  | InvalidArrayRetType
  | InvalidArrayResMod
  | InvalidRetArrayLit
  | NilDeclaration
  | SizeArrayNonPositive
  | SizeArrayNonInteger
  | VarSizeArray
  | VarSizeObjectArg
  | WrongArrayLitDim Int Int
  | MissingMain
  | MainIntReturnType
  | UnexpectedStmt
  | InvalidTernaryOps TypeSpec TypeSpec
  | TError String   -- generic error
  deriving (Eq, Show)

instance PrPrint StaticError where
  prettyDoc = pd
    where
      pd (InvalidInfixOp op l r) = text "Invalid operands to binary"
                               <+> prettyDoc op
                               <+> text "(have types"
                               <+> quotes (prettyDoc l)
                               <+> text "and"
                               <+> quotes (prettyDoc r)
                               <.> text ")"
      pd (InvalidAssgnOp op l r) = text "Invalid operands to assignment"
                               <+> quotes (prettyDoc op <.> equals)
                               <+> text "(have types"
                               <+> quotes (prettyDoc l)
                               <+> text "and"
                               <+> quotes (prettyDoc r)
                               <.> text ")"
      pd (IncompatibleTypes l r) = text "Incompatible types"
                               <+> quotes (prettyDoc l)
                               <+> text "and"
                               <+> quotes (prettyDoc r)
      pd (InvalidUnaryOp op t)   = text "Invalid operand to unary"
                               <+> prettyDoc op
                               <+> text "(has type"
                               <+> quotes (prettyDoc t)
                               <.> text ")"
      pd (UnexpectedType g e)    = text "Expected type"
                               <+> quotes (prettyDoc e) -- e: expected type
                               <+> text "(has type"
                               <+> quotes (prettyDoc g) -- g: given type
                               <.> text ")"
      pd (UnexpectedArgType g e) = text "Expected argument of type"
                               <+> quotes (prettyDoc e) -- e: expected type
                               <+> text "(has type"
                               <+> quotes (prettyDoc g) -- g: given type
                               <.> text ")"
      pd (UnexpectedRetType g e) = text "Expected return value of type"
                               <+> quotes (prettyDoc e) -- e: expected type
                               <+> text "(has type"
                               <+> quotes (prettyDoc g) -- g: given type
                               <.> text ")"
      pd (UnexpectedArgVC)       = text "Expected l-value as function argument"
      pd (ExpectedArrayType g)   = text "Expected array"
                               <+> text "(has type"
                               <+> quotes (prettyDoc g) -- g: given type
                               <.> text ")"
      pd (InvalidDerefOp t)      = text "Invalid operand to unary"
                               <+> text "'*'"
                               <+> text "(has type"
                               <+> quotes (prettyDoc t)
                               <.> text ")"
      pd (UndeclaredVarIdent id) = text "Undeclared variable identifier"
                               <+> quotes (prettyDoc id)
      pd (UnInitVarIdent id)     = text "Variable"
                               <+> quotes (prettyDoc id)
                               <+> text "has not been initialized"
      pd (UndeclaredFunIdent id) = text "Undeclared function"
                               <+> quotes (prettyDoc id)
      pd (VarIdentNotModif id)   = text "Variable"
                               <+> quotes (prettyDoc id)
                               <+> text "can not be modified"
      pd (InvalidIncDecrVC id)   = text "l-value required as"
                               <+> prettyDoc id
                               <+> text "operand"
      pd (InvalidIncDecrOp id t) = text "Invalid operand to"
                               <+> prettyDoc id
                               <+> text "(has type"
                               <+> quotes (prettyDoc t)
                               <.> text ")"
      pd (InvalidRefVC)          = text "l-value required as"
                               <+> text "'&'"
                               <+> text "operand"
      pd (WrongArgNum g e)       = text "Expected"
                               <+> int e  -- e: expected int
                               <+> text "arguments, given"
                               <+> int g  -- g: given int
      pd (WrongArrayLitDim g e)  = text "Expected"
                               <+> int e  -- e: expected int
                               <+> text "elements in array initializer, given"
                               <+> int g  -- g: given int
      pd (ExpectedArrayLit)      = text "Expected array initializer"
      pd (UnexpectedArrayLit)    = text "Unexpected array initializer"
      pd (WrongScalarInit)       = text "Initializing scalar with array initializer"
      pd (WrongScalarAssgn)      = text "Assigning array initializer to a scalar type"
      pd (InvalidAssgnVC)        = text "l-value required in an assignment"
      pd (InvalidAssgnArray)     = text "Invalid array assignment"
      pd (RedeclaredVar id l)    = text "Variable"
                               <+> quotes (prettyDoc id)
                               <+> text "already declared at"
                               <+> prettyDoc l -- l: location of previous declaration
      pd (RedeclaredParam id l)  = text "Function parameter"
                               <+> quotes (prettyDoc id)
                               <+> text "already declared at"
                               <+> prettyDoc l -- l: location of previous declaration
      pd (RedeclaredFun id l)    = if (isEmpty $ prettyDoc l) then
                                     text "Redeclaring predefined function"
                                     <+> quotes (prettyDoc id)
                                   else
                                     text "Function"
                                     <+> quotes (prettyDoc id)
                                     <+> text "already declared at"
                                     <+> prettyDoc l -- l: location of previous declaration
      pd (InvalidGuard t)        = text "Guard is not boolean"
                               <+> text "(has type"
                               <+> quotes (prettyDoc t)
                               <.> text ")"
      pd (InvalidRangeOp)        = text "Range delimiter is not integer"
      pd (InvalidJumpStmt stmt)  = quotes (prettyDoc stmt) -- stmt: break or continue
                               <+> text "statement not within indefinite loop"
      pd (ResNotInit ids f)      = text "Function"
                               <+> quotes (prettyDoc f)
                               <+> text "does not always define the result parameters"
                               <+> (brackets . hsep . punctuate comma . map (quotes . prettyDoc) $ ids)
      pd (CaptureOfResParam id f)= text "Function"
                               <+> quotes (prettyDoc f)
                               <+> text "captures result parameter"
                               <+> quotes (prettyDoc id)
      pd (ReturnNotPresent id)   = text "Function"
                               <+> quotes (prettyDoc id)
                               <+> text "does not always returns a value"
      pd (RetValFromUnit)        = text "Returning value from function returning nil"
      pd (RetUnitFromVal t)      = text "Returning no value from function returning"
                               <+> quotes (prettyDoc t)
      pd (InvalidArrayRetType)   = text "Functions can not return an array type"
      pd (InvalidArrayResMod)    = text "'result' can not be the modality of arrays"
      pd (NilDeclaration)        = text "Variable declared nil"
      pd (SizeArrayNonPositive)  = text "Size of array is not a positive integer"
      pd (SizeArrayNonInteger)   = text "Size of array has non-integer type"
      pd (VarSizeArray)          = text "Arrays can not be of variable size"
      pd (VarSizeObjectArg)      = text "Variable-sized object can not be passed by value"
      pd (InvalidRetArrayLit)    = text "Unexpected array initializer in function return value"
      pd (MissingMain)           = text "The program entry point is missing: declare the main function"
      pd (MainIntReturnType)     = text "Main function must return an int"
      pd (UnexpectedStmt)        = text "Unexpected statement at file scope"
      pd (InvalidTernaryOps t e) = text "Operands of ternary operator must have the same -not nil- type"
                               <+> text "(have types"
                               <+> quotes (prettyDoc t)
                               <+> text "and"
                               <+> quotes (prettyDoc e)
                               <.> text ")"
      pd (TError s)              = text s

-- Functions for the pretty printing of errors

printError :: V.Vector (V.Vector Char) -> LcErrorType -> String
printError fc (L l e) = "\n" ++ render (prettyDoc l <+> prettyDoc e) ++ "\n" ++ highlightSrcSpan fc l

printErrors :: V.Vector (V.Vector Char) -> [LcErrorType] -> IO ()
printErrors fc = mapM_ (putStrLn . (printError fc)) . sortOn getLoc . (filter (not . isWrongError))

isWrongError :: LcErrorType -> Bool
isWrongError (L _ (StaticError te)) = isUnTypedError te

isUnTypedError :: StaticError -> Bool
isUnTypedError (InvalidInfixOp _ l r)  = isUnTyped l || isUnTyped r
isUnTypedError (InvalidAssgnOp _ l r)  = isUnTyped l || isUnTyped r
isUnTypedError (IncompatibleTypes l r) = isUnTyped l || isUnTyped r
isUnTypedError (InvalidUnaryOp _ t)    = isUnTyped t
isUnTypedError (UnexpectedType g e)    = isUnTyped g || isUnTyped e
isUnTypedError (UnexpectedArgType g e) = isUnTyped g || isUnTyped e
isUnTypedError (UnexpectedRetType g e) = isUnTyped g || isUnTyped e
isUnTypedError (ExpectedArrayType g)     = isUnTyped g
isUnTypedError (InvalidDerefOp t)      = isUnTyped t
isUnTypedError (InvalidIncDecrOp _ t)  = isUnTyped t
isUnTypedError (InvalidGuard t)        = isUnTyped t
isUnTypedError (RetUnitFromVal t)      = isUnTyped t
isUnTypedError _                       = False
