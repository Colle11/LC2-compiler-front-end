
module Types where

import {-# SOURCE #-} Annotations
import {-# SOURCE #-} AST
import qualified Data.Semigroup as S

data BasicType
instance Eq BasicType
instance Ord BasicType
instance Show BasicType

type AnBasicType = Annotated BasicType

data ModTypeSpec

instance Eq ModTypeSpec
instance Ord ModTypeSpec
instance Show ModTypeSpec

data TypeSpec
  = BasicType BasicType
  | ArrayType AnRExpr TypeSpec
  | RefType TypeSpec
  | FunType [ModTypeSpec] TypeSpec
  | Unit
  | UnTypeable
  | UnTyped

instance Eq TypeSpec
instance Ord TypeSpec
instance Show TypeSpec

type AnTypeSpec = Annotated TypeSpec

data Constness
  = ConstExpr
  | VarExpr

instance Eq Constness
instance Show Constness
instance Ord Constness
instance S.Semigroup Constness
instance Monoid Constness


data ValueCategory
  = RValue
  | LValue

instance Eq ValueCategory
instance Ord ValueCategory
instance Show ValueCategory
