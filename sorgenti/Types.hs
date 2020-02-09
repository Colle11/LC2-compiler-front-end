{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveFunctor,DeriveFoldable, DeriveTraversable #-}

module Types (
             BasicType(..),
             AnBasicType,
             TypeSpec(..),
             AnTypeSpec,
             ModTypeSpec(..),
             Modality(..),
             AnModality,
             Constness(..),
             ValueCategory(..),
             joinLt,
             ltUnTypeable,
             module Algebra.Lattice,
             APO.partialOrdEq,
             lt,
             isInt,
             isFloat,
             isRefType,
             isArrayType,
             isUnit,
             isUnTyped,
             getArrayBasicType,
             coerceArrayToRef,
             coerceStringToRef,
             isLValue,
             isVarExpr,
             sizeof,
             tacTypePrPrint
             ) where

import Annotations
import PrettyPrinting as PP
import {-# SOURCE #-} AST
import Algebra.Lattice
import Algebra.PartialOrd as APO

data BasicType
  = BasicTypeBool
  | BasicTypeChar
  | BasicTypeFloat
  | BasicTypeInt
  | BasicTypeString
  deriving (Eq, Ord, Show)

type AnBasicType = Annotated BasicType

instance PrPrint BasicType where
  prettyDoc = pd
    where
      pd (BasicTypeBool)   = text "bool"
      pd (BasicTypeChar)   = text "char"
      pd (BasicTypeFloat)  = text "float"
      pd (BasicTypeInt)    = text "int"
      pd (BasicTypeString) = text "string"

data ModTypeSpec = ModTypeSpec Modality TypeSpec
  deriving (Eq, Ord, Show)

instance PrPrint ModTypeSpec where
  prettyDoc = pd
    where
      pd (ModTypeSpec m t) = prettyDoc m <+> prettyDoc t

data Modality
  = Value
  | Reference
  | Result
  | ValueResult
  | Constant
  deriving (Eq, Ord, Show)

type AnModality = Annotated Modality

instance PrPrint Modality where
  prettyDoc = pd
    where
      pd (Value) = text "val"
      pd (Reference) = text "ref"
      pd (Result) = text "res"
      pd (ValueResult) = text "valres"
      pd (Constant) = text "const"

data TypeSpec
  = BasicType BasicType
  | ArrayType AnRExpr TypeSpec
  | RefType TypeSpec
  | FunType [ModTypeSpec] TypeSpec
  | Unit -- corresponds to nil
  -- Term of the language that does not respect the typing rules
  | UnTypeable -- Corresponds to Top
  -- No information at all on the type of the term
  | UnTyped -- Corresponds to Bottom
  deriving (Eq, Ord, Show)

type AnTypeSpec = Annotated TypeSpec

instance PrPrint TypeSpec where
  prettyDoc = pd
    where
      pd (BasicType b) = prettyDoc b
      pd (RefType t) = prettyDoc t <.> text "*"
      pd (ArrayType r t) = prettyDoc (getArrayBasicType t) <.> brackets (prettyDoc r) <.> arrayPrPrint t
        where
          arrayPrPrint (ArrayType r t) = brackets (prettyDoc r) <.> arrayPrPrint t
          arrayPrPrint _ = empty
      pd (Unit) = text "nil"
      pd (UnTypeable) = text "UNTYPEABLE"
      pd (UnTyped) = text "UNTYPED"
      pd (FunType _ _) = text "FUNTYPE"

-- TAC pretty printing of types
tacTypePrPrint :: TypeSpec -> Doc
tacTypePrPrint (BasicType b) = prettyDoc b
tacTypePrPrint (RefType _) = text "ref"
tacTypePrPrint (ArrayType _ _) = text "arr"
tacTypePrPrint t = prettyDoc t

instance PartialOrd BasicType where
  leq BasicTypeInt BasicTypeFloat = True
  leq BasicTypeChar BasicTypeInt = True
  leq BasicTypeChar BasicTypeFloat = True
  leq BasicTypeChar BasicTypeString = True
  leq x y = x == y

-- We organize the Types in a Bounded Join SemiLattice and we get a Monoid
--                      UnTypeable
--                /    /   |    \   \   \
--               /    /   Float  \   \   \
--              /    /     |      \   \   \
--           Bool  String Int   Array Ref  Unit
--              \        \ |      /   /   /
--               \        Char   /   /   /
--                \        |    /   /   /
--                       UnTyped
--------------------------------------------------------------------------

instance PartialOrd TypeSpec where
  leq UnTyped _ = True
  leq _ UnTypeable = True
  leq (BasicType x) (BasicType y) = leq x y
  leq (RefType x) (RefType y) = leq x y && leq y x
  leq (ArrayType d1 t1) (ArrayType d2 t2) = leq t1 t2 && leq t2 t1
  leq x y = x == y

-- lessThan wrt the Partial Order
lt :: (PartialOrd a) => a -> a -> Bool
lt x y = leq x y && not (partialOrdEq x y) -- && not (leq y x)

instance JoinSemiLattice TypeSpec where
  x \/ y | comparable x y = if leq x y then y else x
         | otherwise = UnTypeable

-- Definition similar to joinLeq
joinLt:: (Eq a, JoinSemiLattice a) => a -> a -> Bool
joinLt x y = x /= y && joinLeq x y

-- lessThan UnTypeable
ltUnTypeable:: TypeSpec -> Bool
ltUnTypeable = flip joinLt UnTypeable

instance BoundedJoinSemiLattice TypeSpec where
  bottom = UnTyped

instance Semigroup TypeSpec where
  (<>) = (\/)

instance Monoid TypeSpec where
  mempty = bottom

data Constness
  = ConstExpr
  | VarExpr
  deriving (Eq, Show, Ord)

instance Semigroup Constness where
  ConstExpr <> x = x
  x <> ConstExpr = x
  x <> _ = x

instance Monoid Constness where
  mempty = ConstExpr

data ValueCategory
  = RValue
  | LValue
  deriving (Eq,Ord, Show)

isInt:: TypeSpec -> Bool
isInt (BasicType BasicTypeInt) = True
isInt _ = False

isFloat:: TypeSpec -> Bool
isFloat (BasicType BasicTypeFloat) = True
isFloat _ = False

isRefType:: TypeSpec -> Bool
isRefType (RefType _) = True
isRefType _ = False

isArrayType:: TypeSpec -> Bool
isArrayType (ArrayType _ _) = True
isArrayType _ = False

isUnit :: TypeSpec -> Bool
isUnit Unit = True
isUnit _    = False

isUnTyped :: TypeSpec -> Bool
isUnTyped (ArrayType _ t) = isUnTyped t
isUnTyped (RefType t) = isUnTyped t
isUnTyped UnTyped = True
isUnTyped _       = False

getArrayBasicType :: TypeSpec -> TypeSpec
getArrayBasicType (ArrayType _ t) = getArrayBasicType t
getArrayBasicType t = t

coerceArrayToRef :: TypeSpec -> TypeSpec
coerceArrayToRef (ArrayType d t) = RefType t
coerceArrayToRef x = x

coerceStringToRef :: TypeSpec -> TypeSpec
coerceStringToRef t@(BasicType BasicTypeString) = RefType t
coerceStringToRef t = t

isLValue :: ValueCategory -> Bool
isLValue LValue = True
isLValue _ = False

isVarExpr :: Constness -> Bool
isVarExpr VarExpr = True
isVarExpr _ = False

-- Gives the size in basic memory units (bytes) of our types
sizeof :: TypeSpec -> Int
sizeof ( BasicType BasicTypeInt )    = 4
sizeof ( BasicType BasicTypeFloat )  = 8
sizeof t@(BasicType BasicTypeString) = sizeof $ RefType t -- a reference to the string
sizeof ( BasicType BasicTypeChar )   = 1
sizeof ( BasicType BasicTypeBool )   = 1
sizeof ( RefType _ )                 = 8
sizeof t                             = error ("Internal Compiler Error: sizeof("
                                    ++ toString t
                                    ++ ") can not be statically computed")
